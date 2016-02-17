package org.mdedetrich.stripe.v1

import org.mdedetrich.utforsca.SealedContents
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

object Coupons {

  sealed abstract class Duration(val id: String)

  case class UnknownDuration(val id: String) extends Exception {
    override def getMessage = s"Unknown Coupon Duration, received $id"
  }

  object Duration {

    case object Forever extends Duration("forever")

    case object Once extends Duration("once")

    case object Repeating extends Duration("repeating")

    lazy val all: Set[Duration] = SealedContents.values[Duration]

  }

  implicit val durationReads: Reads[Duration] = Reads.of[String].map { durationId =>
    Duration.all.find(_.id == durationId).getOrElse {
      throw UnknownDuration(durationId)
    }
  }

  implicit val durationWrites: Writes[Duration] =
    Writes((duration: Duration) => JsString(duration.id))

  case class Coupon(id: String,
                    amountOff: Option[Long],
                    created: DateTime,
                    currency: Option[Currency],
                    duration: Duration,
                    durationInMonths: Option[Long],
                    livemode: Boolean,
                    maxRedemptions: Option[Long],
                    metadata: Option[Map[String,String]],
                    percentOff: Option[BigDecimal],
                    redeemBy: Option[DateTime],
                    timesRedeemed: Long,
                    valid: Boolean
                   ) extends StripeObject

  implicit val couponReads: Reads[Coupon] = (
    (__ \ "id").read[String] ~
      (__ \ "amount_off").readNullable[Long] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").readNullable[Currency] ~
      (__ \ "duration").read[Duration] ~
      (__ \ "duration_in_months").readNullable[Long] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "max_redemptions").readNullable[Long] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String,String]] ~
      (__ \ "percent_off").readNullable[BigDecimal] ~
      (__ \ "redeem_by").readNullable[Long].map {
        maybeTimestamp => maybeTimestamp.map { timestamp =>
          new DateTime(timestamp * 1000)
        }
      } ~
      (__ \ "times_redeemed").read[Long] ~
      (__ \ "valid").read[Boolean]
    ).tupled.map(Coupon.tupled)

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

}
