package org.mdedetrich.stripe.v1

import java.io.InputStream
import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.stream.Materializer
import akka.stream.scaladsl.StreamConverters
import com.typesafe.scalalogging.LazyLogging
import defaults._
import enumeratum.{Enum, EnumEntry}
import io.circe.{Decoder, Encoder}
import org.mdedetrich.stripe.{ApiKey, FileUploadEndpoint}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object FileUploads extends LazyLogging {

  // Purpose
  sealed abstract class Purpose(value: String) extends EnumEntry {
    override val entryName = value
  }

  object Purpose extends Enum[Purpose] {
    val values = findValues

    case object BusinessLogo            extends Purpose("business_logo")
    case object DisputeEvidence         extends Purpose("dispute_evidence")
    case object IdentityDocument        extends Purpose("identity_document")
    case object IncorporationArticle    extends Purpose("incorporation_article")
    case object IncorporationDocument   extends Purpose("incorporation_document")
    case object PaymentProviderTransfer extends Purpose("payment_provider_transfer")
    case object ProductFeed             extends Purpose("product_feed")

    implicit val purposeDecoder: Decoder[Purpose] = enumeratum.Circe.decoder(Purpose)
    implicit val purposeEncoder: Encoder[Purpose] = enumeratum.Circe.encoder(Purpose)
  }

  // FileUpload

  case class FileUpload(
      id: String,
      created: OffsetDateTime,
      purpose: Purpose,
      size: Long,
      url: Option[String]
  )

  implicit val fileUploadDecoder: Decoder[FileUpload] = Decoder.forProduct5(
    "id",
    "created",
    "purpose",
    "size",
    "url"
  )(FileUpload.apply)

  implicit val fileUploadEncoder: Encoder[FileUpload] = Encoder.forProduct5(
    "id",
    "created",
    "purpose",
    "size",
    "url"
  )(x => FileUpload.unapply(x).get)

  def upload(purpose: Purpose, fileName: String, inputStream: InputStream)(
      implicit http: HttpExt,
      materializer: Materializer,
      apiKey: ApiKey,
      fileUploadChunkTimeout: FileUploadChunkTimeout,
      endpoint: FileUploadEndpoint,
      executionContext: ExecutionContext
  ): Future[Try[FileUpload]] = {

    val finalUrl = endpoint.url + s"/v1/files"

    val eventualFormData = for {
      fileStream <- HttpEntity(
        MediaTypes.`application/octet-stream`,
        StreamConverters.fromInputStream(() => inputStream)
      ).toStrict(fileUploadChunkTimeout.duration)
    } yield
      Multipart
        .FormData(
          Multipart.FormData.BodyPart(
            "file",
            fileStream,
            Map("filename" -> fileName)
          ),
          Multipart.FormData.BodyPart("purpose", purpose.entryName)
        )
        .toEntity()

    val eventualReq = for {
      formData <- eventualFormData
    } yield
      HttpRequest(
        uri = finalUrl,
        entity = formData,
        method = HttpMethods.POST,
        headers = List(Authorization(BasicHttpCredentials(apiKey.apiKey, "")))
      )

    for {
      req      <- eventualReq
      response <- http.singleRequest(req)
      parsed   <- parseStripeServerError[FileUpload](response, finalUrl, None, None, logger)
      result = parsed match {
        case Right(triedJsValue) =>
          util.Success(triedJsValue.get)
        case Left(error) =>
          util.Failure(error)
      }

    } yield result

  }
}
