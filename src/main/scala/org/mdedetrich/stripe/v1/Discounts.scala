package org.mdedetrich.stripe.v1

import org.joda.time.DateTime
import org.mdedetrich.stripe.v1.Coupons._
import play.api.libs.json._
import play.api.libs.functional.syntax._

object Discounts {

  case class Discount(coupon: Coupon,
                      customer: String,
                      end: DateTime,
                      start: DateTime,
                      subscription: Option[String]
                     ) extends StripeObject

  implicit val discountReads: Reads[Discount] = (
    (__ \ "coupon").read[Coupon] ~
      (__ \ "customer").read[String] ~
      (__ \ "end").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "start").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "subscription").readNullable[String]
    ).tupled.map((Discount.apply _).tupled)

  implicit val discountWrites: Writes[Discount] =
    Writes((discount: Discount) =>
      Json.obj(
        "object" -> "discount",
        "coupon" -> discount.coupon,
        "customer" -> discount.customer,
        "end" -> discount.end.getMillis / 1000,
        "start" -> discount.start.getMillis / 1000,
        "subscription" -> discount.subscription
      )
    )

}
