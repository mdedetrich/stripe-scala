package org.mdedetrich.stripe.v1

import akka.http.scaladsl.model.HttpResponse
import cats.instances.either._
import cats.syntax.either._
import enumeratum._
import io.circe.Decoder.Result
import io.circe._

object Errors {

  /**
    * @see https://stripe.com/docs/api#errors
    * @param id
    */
  sealed abstract class Type(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Type extends Enum[Type] {
    val values = findValues

    case object ApiConnectionError  extends Type("api_connection_error")
    case object ApiError            extends Type("api_error")
    case object AuthenticationError extends Type("authentication_error")
    case object CardError           extends Type("card_error")
    case object InvalidRequestError extends Type("invalid_request_error")
    case object RateLimitError      extends Type("rate_limit_error")

    implicit val errorTypeDecoder: Decoder[Type] = enumeratum.Circe.decoder(Type)
    implicit val errorTypeEncoder: Encoder[Type] = enumeratum.Circe.encoder(Type)
  }

  /**
    * @see from https://stripe.com/docs/api#errors
    * @param id
    */
  sealed abstract class Code(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Code extends Enum[Code] {
    val values = findValues

    case object InvalidNumber       extends Code("invalid_number")
    case object InvalidExpiryMonth  extends Code("invalid_expiry_month")
    case object InvalidExpiryYear   extends Code("invalid_expiry_year")
    case object InvalidCVC          extends Code("invalid_cvc")
    case object IncorrectNumber     extends Code("incorrect_number")
    case object ExpiredCard         extends Code("expired_card")
    case object IncorrectCVC        extends Code("incorrect_cvc")
    case object IncorrectZip        extends Code("incorrect_zip")
    case object CardDeclined        extends Code("card_declined")
    case object Missing             extends Code("missing")
    case object ProcessingError     extends Code("processing_error")
    case object TransfersNotAllowed extends Code("transfers_not_allowed")

    implicit val errorCodeDecoder: Decoder[Code] = enumeratum.Circe.decoder(Code)
    implicit val errorCodeEncoder: Encoder[Code] = enumeratum.Circe.encoder(Code)
  }

  /**
    * Typed error from stripe
    *
    * @see https://stripe.com/docs/api#errors
    * @param httpCode
    * @param `type`
    * @param code
    * @param message
    * @param param Note that if we get an empty String param from stripe, this will get mapped to [[None]]
    */
  sealed abstract class Error(
      val httpCode: Long,
      val `type`: Type,
      val code: Option[Code],
      val message: Option[String],
      val param: Option[String]
  ) extends Exception {
    override def toString: String   = s"""Error($httpCode, ${`type`}, $code, $message, $param)"""
    override def getMessage: String = toString
  }

  object Error {

    final case class BadRequest(
        override val `type`: Type,
        override val code: Option[Code],
        override val message: Option[String],
        override val param: Option[String]
    ) extends Error(400, `type`, code, message, param)

    final case class Unauthorized(
        override val `type`: Type,
        override val code: Option[Code],
        override val message: Option[String],
        override val param: Option[String]
    ) extends Error(401, `type`, code, message, param)

    final case class RequestFailed(
        override val `type`: Type,
        override val code: Option[Code],
        override val message: Option[String],
        override val param: Option[String]
    ) extends Error(402, `type`, code, message, param)

    final case class NotFound(
        override val `type`: Type,
        override val code: Option[Code],
        override val message: Option[String],
        override val param: Option[String]
    ) extends Error(404, `type`, code, message, param)

    final case class TooManyRequests(
        override val `type`: Type,
        override val code: Option[Code],
        override val message: Option[String],
        override val param: Option[String]
    ) extends Error(429, `type`, code, message, param)
  }

  private def errorDecoder(
      c: HCursor
  ): (Result[Type], Result[Option[Code]], Result[Option[String]], Result[Option[String]]) =
    (
      c.downField("type").as[Type],
      c.downField("code").as[Option[Code]],
      c.downField("message").as[Option[String]],
      c.downField("param").as[Option[String]].map {
        case Some(s) if s.isEmpty => None
        case s                    => s
      }
    )

  import cats.syntax.apply._
  import cats.instances.either._

  implicit val badRequestDecoder: Decoder[Error.BadRequest] =
    Decoder.instance[Error.BadRequest] { c =>
      errorDecoder(c).mapN(Error.BadRequest.apply)
    }
  implicit val unauthorizedDecoder: Decoder[Error.Unauthorized] =
    Decoder.instance[Error.Unauthorized] { c =>
      errorDecoder(c).mapN(Error.Unauthorized.apply)
    }
  implicit val requestFailedDecoder: Decoder[Error.RequestFailed] =
    Decoder.instance[Error.RequestFailed] { c =>
      errorDecoder(c).mapN(Error.RequestFailed.apply)
    }
  implicit val notFoundDecoder: Decoder[Error.NotFound] =
    Decoder.instance[Error.NotFound] { c =>
      errorDecoder(c).mapN(Error.NotFound.apply)
    }
  implicit val tooManyRequestsDecoder: Decoder[Error.TooManyRequests] =
    Decoder.instance[Error.TooManyRequests] { c =>
      errorDecoder(c).mapN(Error.TooManyRequests.apply)
    }

  private def errorEncoder: Encoder[Error] =
    Encoder.forProduct4(
      "type",
      "code",
      "message",
      "param"
    )(x => (x.`type`, x.code, x.message, x.param))

  implicit val badRequestEncoder: Encoder[Error.BadRequest] =
    Encoder.instance[Error.BadRequest](e => errorEncoder(e))
  implicit val unauthorizedEncoder: Encoder[Error.Unauthorized] =
    Encoder.instance[Error.Unauthorized](e => errorEncoder(e))
  implicit val requestFailedEncoder: Encoder[Error.RequestFailed] =
    Encoder.instance[Error.RequestFailed](e => errorEncoder(e))
  implicit val notFoundEncoder: Encoder[Error.NotFound] =
    Encoder.instance[Error.NotFound](e => errorEncoder(e))
  implicit val tooManyRequestsEncoder: Encoder[Error.TooManyRequests] =
    Encoder.instance[Error.TooManyRequests](e => errorEncoder(e))

  /**
    * This is thrown when you receive either a 500, 502, 503 or 504
    *
    * @param httpResponse
    */
  final case class StripeServerError(httpResponse: HttpResponse) extends Exception {
    override def getMessage =
      s"Stripe server error, status code is ${httpResponse.status.intValue()}"
  }

  final case class UnhandledServerError(httpResponse: HttpResponse) extends Exception {
    override def getMessage =
      s"Unhandled server error, status code is ${httpResponse.status.intValue()}"
  }
}
