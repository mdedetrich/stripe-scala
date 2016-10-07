package org.mdedetrich.stripe.v1

import java.io.InputStream
import java.time.OffsetDateTime

import com.ning.http.client.multipart.{ByteArrayPart, StringPart}
import com.typesafe.scalalogging.LazyLogging
import dispatch.{Http, url}
import enumeratum.{Enum, EnumEntry, EnumFormats, PlayJsonEnum}
import org.mdedetrich.stripe.{ApiKey, FileUploadEndpoint, InvalidJsonModelException}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object FileUploads extends LazyLogging {

  // Purpose
  sealed abstract class Purpose(value: String) extends EnumEntry {
    override val entryName = value
  }

  object Purpose extends Enum[Purpose] with PlayJsonEnum[Purpose] {
    val values = findValues

    case object BusinessLogo extends Purpose("business_logo")
    case object DisputeEvidence extends Purpose("dispute_evidence")
    case object IdentityDocument extends Purpose("identity_document")
    case object IncorporationArticle extends Purpose("incorporation_article")
    case object IncorporationDocument extends Purpose("incorporation_document")
    case object PaymentProviderTransfer extends Purpose("payment_provider_transfer")
    case object ProductFeed extends Purpose("product_feed")

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
      implicit apiKey: ApiKey,
      endpoint: FileUploadEndpoint,
      ec: ExecutionContext): Future[Try[FileUpload]] = {

    val data = Stream.continually(inputStream.read).takeWhile(_ != -1).map(_.toByte).toArray
    upload(purpose, fileName, data)
  }

  def upload(purpose: Purpose, fileName: String, data: Array[Byte])(
      implicit apiKey: ApiKey,
      endpoint: FileUploadEndpoint,
      ec: ExecutionContext): Future[Try[FileUpload]] = {

    val finalUrl = endpoint.url + s"/v1/files"

    val paramsPart = new StringPart("purpose", purpose.entryName)
    val filePart = new ByteArrayPart("file", data, null, null, fileName)

    val req = url(finalUrl).addHeader("Content-Type", "multipart/form-data")
      .POST.as(apiKey.apiKey, "")
      .addBodyPart(paramsPart)
      .addBodyPart(filePart)

    Http(req).map { response =>
      parseStripeServerError(
          response, finalUrl, None, None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[FileUpload](jsValue)
            jsResult.fold(
                errors => {
                  throw InvalidJsonModelException(response.getStatusCode,
                                                  finalUrl,
                                                  None,
                                                  None,
                                                  jsValue,
                                                  errors)
                },
                model => model
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }
}
