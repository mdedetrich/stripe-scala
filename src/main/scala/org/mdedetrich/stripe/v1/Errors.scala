package org.mdedetrich.stripe.v1

import com.ning.http.client.Response
import enumeratum._
import play.api.libs.functional.syntax._
import play.api.libs.json._

object Errors {

  /**
    * Errors taken from https://stripe.com/docs/api#errors
    *
    * @param id
    */
  sealed abstract class Type(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Type extends Enum[Type] {

    val values = findValues

    case object ApiConnectionError extends Type("api_connection_error")

    case object ApiError extends Type("api_error")

    case object AuthenticationError extends Type("authentication_error")

    case object CardError extends Type("card_error")

    case object InvalidRequestError extends Type("invalid_request_error")

    case object RateLimitError extends Type("rate_limit_error")

  }

  implicit val typeFormats = EnumFormats.formats(Type, insensitive = true)

  /**
    * Codes taken from https://stripe.com/docs/api#errors
    *
    * @param id
    */
  sealed abstract class Code(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Code extends Enum[Code] {

    val values = findValues

    case object InvalidNumber extends Code("invalid_number")

    case object InvalidExpiryMonth extends Code("invalid_expiry_month")

    case object InvalidExpiryYear extends Code("invalid_expiry_year")

    case object InvalidCVC extends Code("invalid_cvc")

    case object IncorrectNumber extends Code("incorrect_number")

    case object ExpiredCard extends Code("expired_card")

    case object IncorrectCVC extends Code("incorrect_cvc")

    case object IncorrectZip extends Code("incorrect_zip")

    case object CardDeclined extends Code("card_declined")

    case object Missing extends Code("missing")

    case object ProcessingError extends Code("processing_error")

  }

  implicit val codeFormats = EnumFormats.formats(Code, insensitive = true)

  sealed abstract class Error(val httpCode: Long,
                              val `type`: Type,
                              val code: Option[Code],
                              val message: Option[String],
                              val param: Option[String]) extends Exception

  object Error {

    case class BadRequest(override val `type`: Type,
                          override val code: Option[Code],
                          override val message: Option[String],
                          override val param: Option[String]) extends Error(400, `type`, code, message, param)

    case class Unauthorized(override val `type`: Type,
                            override val code: Option[Code],
                            override val message: Option[String],
                            override val param: Option[String]) extends Error(401, `type`, code, message, param)

    case class RequestFailed(override val `type`: Type,
                             override val code: Option[Code],
                             override val message: Option[String],
                             override val param: Option[String]) extends Error(402, `type`, code, message, param)

    case class NotFound(override val `type`: Type,
                        override val code: Option[Code],
                        override val message: Option[String],
                        override val param: Option[String]) extends Error(404, `type`, code, message, param)

    case class TooManyRequests(override val `type`: Type,
                               override val code: Option[Code],
                               override val message: Option[String],
                               override val param: Option[String]) extends Error(429, `type`, code, message, param)

  }

  private def tupledErrorReads = (
    (__ \ "type").read[Type] ~
      (__ \ "code").readNullable[Code] ~
      (__ \ "message").readNullable[String] ~
      (__ \ "param").readNullable[String]
    ).tupled


  implicit val badRequestReads: Reads[Error.BadRequest] = tupledErrorReads.map((Error.BadRequest.apply _).tupled)
  implicit val unauthorizedReads: Reads[Error.Unauthorized] = tupledErrorReads.map((Error.Unauthorized.apply _).tupled)
  implicit val requestFailedReads: Reads[Error.RequestFailed] = tupledErrorReads.map((Error.RequestFailed.apply _).tupled)
  implicit val notFoundReads: Reads[Error.NotFound] = tupledErrorReads.map((Error.NotFound.apply _).tupled)
  implicit val tooManyRequestsReads: Reads[Error.TooManyRequests] = tupledErrorReads.map((Error.TooManyRequests.apply _).tupled)

  private def errorWrites(error: Error) =
    Json.obj(
      "type" -> error.`type`,
      "code" -> error.code,
      "message" -> error.message,
      "param" -> error.param
    )

  implicit val badRequestWrites: Writes[Error.BadRequest] =
    Writes((badRequest: Error.BadRequest) =>
      errorWrites(badRequest)
    )

  implicit val unauthorizedWrites: Writes[Error.Unauthorized] =
    Writes((unauthorized: Error.Unauthorized) =>
      errorWrites(unauthorized)
    )

  implicit val requestFailedWrites: Writes[Error.RequestFailed] =
    Writes((requestFailed: Error.RequestFailed) =>
      errorWrites(requestFailed)
    )

  implicit val notFoundWrites: Writes[Error.NotFound] =
    Writes((notFound: Error.NotFound) =>
      errorWrites(notFound)
    )

  implicit val tooManyRequestsWrites: Writes[Error.TooManyRequests] =
    Writes((tooManyRequests: Error.TooManyRequests) =>
      errorWrites(tooManyRequests)
    )

  /**
    * This is thrown when you receive either a 500, 502, 503 or 504
    *
    * @param response
    */

  case class StripeServerError(val response: Response) extends Exception {
    override def getMessage = s"Stripe server error, status code is ${response.getStatusCode}"
  }

  case class UnhandledServerError(val response: Response) extends Exception {
    override def getMessage = s"Unhandled server error, status code is ${response.getStatusCode}"
  }

}
