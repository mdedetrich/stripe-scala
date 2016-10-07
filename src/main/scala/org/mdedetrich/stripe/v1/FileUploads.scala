package org.mdedetrich.stripe.v1

import java.io.InputStream

import com.ning.http.client.multipart.{ByteArrayPart, StringPart}
import com.typesafe.scalalogging.LazyLogging
import dispatch.{Http, url}
import enumeratum.{Enum, EnumEntry, EnumFormats}
import org.mdedetrich.stripe.{ApiKey, FileUploadEndpoint, InvalidJsonModelException}
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object FileUploads extends LazyLogging {

  sealed abstract class Purpose(val value: String) extends EnumEntry

  object Purpose extends Enum[Purpose] {
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

  def upload(purpose: Purpose, fileName: String, inputStream: InputStream)(
      implicit apiKey: ApiKey,
      endpoint: FileUploadEndpoint,
      ec: ExecutionContext): Future[Try[JsValue]] = {

    val data = Stream.continually(inputStream.read).takeWhile(_ != -1).map(_.toByte).toArray
    upload(purpose, fileName, data)
  }

  def upload(purpose: Purpose, fileName: String, data: Array[Byte])(
      implicit apiKey: ApiKey,
      endpoint: FileUploadEndpoint,
      ec: ExecutionContext): Future[Try[JsValue]] = {

    val finalUrl = endpoint.url + s"/v1/files"

    val paramsPart = new StringPart("purpose", purpose.value)
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
            val jsResult = Json.fromJson[JsValue](jsValue)
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
