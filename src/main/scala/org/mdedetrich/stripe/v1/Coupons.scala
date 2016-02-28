package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.mdedetrich.stripe.{InvalidJsonModelException, Endpoint, ApiKey, IdempotencyKey}
import dispatch.Defaults._
import dispatch._
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

import scala.concurrent.Future
import scala.util.Try

object Coupons extends LazyLogging {

  sealed abstract class Duration(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Duration extends Enum[Duration] {

    val values = findValues

    case object Forever extends Duration("forever")

    case object Once extends Duration("once")

    case object Repeating extends Duration("repeating")

  }

  implicit val durationFormats = EnumFormats.formats(Duration, insensitive = true)

  case class Coupon(id: String,
                    amountOff: Option[Long],
                    created: DateTime,
                    currency: Option[Currency],
                    duration: Duration,
                    durationInMonths: Option[Long],
                    livemode: Boolean,
                    maxRedemptions: Option[Long],
                    metadata: Option[Map[String, String]],
                    percentOff: Option[BigDecimal],
                    redeemBy: Option[DateTime],
                    timesRedeemed: Long,
                    valid: Boolean
                   ) extends StripeObject

  object Coupon {
    def default(id: String,
                created: DateTime,
                duration: Duration,
                livemode: Boolean,
                timesRedeemed: Long,
                valid: Boolean) = Coupon(
      id,
      None,
      created,
      None,
      duration,
      None,
      livemode,
      None,
      None,
      None,
      None,
      timesRedeemed,
      valid
    )
  }

  implicit val couponReads: Reads[Coupon] = (
    (__ \ "id").read[String] ~
      (__ \ "amount_off").readNullable[Long] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").readNullable[Currency] ~
      (__ \ "duration").read[Duration] ~
      (__ \ "duration_in_months").readNullable[Long] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "max_redemptions").readNullable[Long] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "percent_off").readNullable[BigDecimal] ~
      (__ \ "redeem_by").readNullable[Long].map {
        maybeTimestamp => maybeTimestamp.map { timestamp =>
          new DateTime(timestamp * 1000)
        }
      } ~
      (__ \ "times_redeemed").read[Long] ~
      (__ \ "valid").read[Boolean]
    ).tupled.map((Coupon.apply _).tupled)

  implicit val couponWrites: Writes[Coupon] =
    Writes((coupon: Coupon) =>
      Json.obj(
        "id" -> coupon.id,
        "object" -> "coupon",
        "amount_off" -> coupon.amountOff,
        "created" -> coupon.created.getMillis / 1000,
        "currency" -> coupon.amountOff,
        "duration" -> coupon.duration,
        "duration_in_months" -> coupon.durationInMonths,
        "livemode" -> coupon.livemode,
        "max_redemptions" -> coupon.maxRedemptions,
        "metadata" -> coupon.metadata,
        "percent_off" -> coupon.percentOff,
        "redeem_by" -> coupon.redeemBy.map(_.getMillis / 1000),
        "times_redeemed" -> coupon.timesRedeemed,
        "valid" -> coupon.valid
      )
    )

  case class CouponInput(id: Option[String],
                         duration: Duration,
                         amountOff: Option[Long],
                         currency: Option[Currency],
                         durationInMonths: Option[Long],
                         maxRedemptions: Option[Long],
                         metadata: Option[Map[String, String]],
                         percentOff: Option[BigDecimal],
                         redeemBy: Option[DateTime]
                        )

  object CouponInput {
    def default(duration: Duration): CouponInput = CouponInput(
      None,
      duration,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )
  }

  implicit val couponInputReads: Reads[CouponInput] = (
    (__ \ "id").readNullable[String] ~
      (__ \ "duration").read[Duration] ~
      (__ \ "amount_off").readNullable[Long] ~
      (__ \ "currency").readNullable[Currency] ~
      (__ \ "duration_in_months").readNullable[Long] ~
      (__ \ "max_redemptions").readNullable[Long] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "percent_off").readNullable[BigDecimal] ~
      (__ \ "redeemBy").readNullable[Long].map(_.map { timestamp => new DateTime(timestamp * 1000) })
    ).tupled.map((CouponInput.apply _).tupled)

  implicit val couponInputWrites: Writes[CouponInput] =
    Writes((couponInput: CouponInput) =>
      Json.obj(
        "id" -> couponInput.id,
        "duration" -> couponInput.duration,
        "amount_off" -> couponInput.amountOff,
        "currency" -> couponInput.currency,
        "duration_in_months" -> couponInput.durationInMonths,
        "max_redemptions" -> couponInput.maxRedemptions,
        "metadata" -> couponInput.metadata,
        "percent_off" -> couponInput.percentOff,
        "redeemBy" -> couponInput.redeemBy.map(_.getMillis / 1000)
      )
    )

  def create(couponInput: CouponInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Coupon]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "id" -> couponInput.id,
        "duration" -> Option(couponInput.duration.entryName),
        "amount_off" -> couponInput.amountOff.map(_.toString),
        "currency" -> couponInput.currency.map(_.iso.toLowerCase),
        "duration_in_months" -> couponInput.durationInMonths.map(_.toString),
        "max_redemptions" -> couponInput.maxRedemptions.map(_.toString),
        "percent_off" -> couponInput.percentOff.map(_.toString()),
        "redeemBy" -> couponInput.redeemBy.map(dateTime => (dateTime.getMillis / 1000).toString)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(couponInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/charges"

    val req = {
      val r = (
        url(finalUrl)
          .addHeader("Content-Type", "application/x-www-form-urlencoded")
          << postFormParameters
        ).POST.as(apiKey.apiKey, "")

      idempotencyKey match {
        case Some(key) =>
          r.addHeader(idempotencyKeyHeader, key.key)
        case None =>
          r
      }
    }

    Http(req).map { response =>

      parseStripeServerError(response, finalUrl, Option(postFormParameters), None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[Coupon](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, Option(postFormParameters), None, jsValue, errors)
              }, coupon => coupon
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }
}
