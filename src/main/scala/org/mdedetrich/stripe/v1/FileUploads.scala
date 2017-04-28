package org.mdedetrich.stripe.v1

import java.io.InputStream
import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.stream.Materializer
import akka.stream.scaladsl.StreamConverters
import com.typesafe.scalalogging.LazyLogging
import enumeratum.{Enum, EnumEntry, EnumFormats, PlayJsonEnum}
import org.mdedetrich.stripe.{ApiKey, FileUploadEndpoint, InvalidJsonModelException}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object FileUploads extends LazyLogging {

  // Purpose
  sealed abstract class Purpose(value: String) extends EnumEntry {
    override val entryName = value
  }

  object Purpose extends Enum[Purpose] with PlayJsonEnum[Purpose] {
    val values = findValues

    case object BusinessLogo            extends Purpose("business_logo")
    case object DisputeEvidence         extends Purpose("dispute_evidence")
    case object IdentityDocument        extends Purpose("identity_document")
    case object IncorporationArticle    extends Purpose("incorporation_article")
    case object IncorporationDocument   extends Purpose("incorporation_document")
    case object PaymentProviderTransfer extends Purpose("payment_provider_transfer")
    case object ProductFeed             extends Purpose("product_feed")

    implicit val purposeFormats = EnumFormats.formats(Purpose)
  }

  // FileUpload

  case class FileUpload(
      id: String,
      created: OffsetDateTime,
      purpose: Purpose,
      size: Long,
      url: Option[String]
  )

  implicit val customerReads: Reads[FileUpload] = (
    (__ \ "id").read[String] ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "purpose").read[Purpose] ~
      (__ \ "size").read[Long] ~
      (__ \ "url").readNullable[String]
  ).tupled.map((FileUpload.apply _).tupled)

  implicit val fileUploadReadsWrites = Json.writes[FileUpload]

  def upload(purpose: Purpose, fileName: String, inputStream: InputStream)(
      implicit client: HttpExt,
      materializer: Materializer,
      apiKey: ApiKey,
      endpoint: FileUploadEndpoint,
      executionContext: ExecutionContext): Future[Try[FileUpload]] = {

    val finalUrl = endpoint.url + s"/v1/files"

    val eventualFormData = for {
      fileStream <- HttpEntity(MediaTypes.`application/octet-stream`,
                               StreamConverters.fromInputStream(() => inputStream)).toStrict(1 minute)
    } yield
      Multipart
        .FormData(
          Multipart.FormData.BodyPart(
            "file",
            fileStream
          ),
          Multipart.FormData.BodyPart("purpose", purpose.entryName)
        )
        .toEntity()

    val eventualReq = for {
      formData <- eventualFormData
    } yield
      HttpRequest(uri = finalUrl,
                  entity = formData,
                  method = HttpMethods.POST,
                  headers = List(Authorization(BasicHttpCredentials(apiKey.apiKey, ""))))

    for {
      req      <- eventualReq
      response <- client.singleRequest(req)
      parsed   <- parseStripeServerError(response, finalUrl, None, None, logger)
      result = parsed match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[FileUpload](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.status.intValue(), finalUrl, None, None, jsValue, errors)
              },
              model => model
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }

    } yield result

  }
}
