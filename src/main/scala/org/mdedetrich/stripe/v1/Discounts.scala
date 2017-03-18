package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import org.mdedetrich.stripe.v1.Coupons._
import play.api.libs.functional.syntax._
import play.api.libs.json._

object Discounts {

  case class Discount(coupon: Coupon,
                      customer: String,
                      end: OffsetDateTime,
                      start: OffsetDateTime,
                      subscription: Option[String])
      extends StripeObject

  implicit val discountReads: Reads[Discount] = (
    (__ \ "coupon").read[Coupon] ~
      (__ \ "customer").read[String] ~
      (__ \ "end").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "start").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "subscription").readNullable[String]
  ).tupled.map((Discount.apply _).tupled)

  implicit val discountWrites: Writes[Discount] = Writes(
    (discount: Discount) =>
      Json.obj(
        "object"       -> "discount",
        "coupon"       -> discount.coupon,
        "customer"     -> discount.customer,
        "end"          -> Json.toJson(discount.end)(stripeDateTimeWrites),
        "start"        -> Json.toJson(discount.start)(stripeDateTimeWrites),
        "subscription" -> discount.subscription
    ))
}
