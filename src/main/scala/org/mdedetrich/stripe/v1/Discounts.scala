package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import defaults._
import io.circe.{Decoder, Encoder}
import org.mdedetrich.stripe.v1.Coupons._

object Discounts {

  case class Discount(coupon: Coupon,
                      customer: String,
                      end: OffsetDateTime,
                      start: OffsetDateTime,
                      subscription: Option[String])
      extends StripeObject

  implicit val discountDecoder: Decoder[Discount] = Decoder.forProduct5(
    "coupon",
    "customer",
    "end",
    "start",
    "subscription"
  )(Discount.apply)

  implicit val discountEncoder: Encoder[Discount] = Encoder.forProduct6(
    "object",
    "coupon",
    "customer",
    "end",
    "start",
    "subscription"
  )(x => ("discount", x.coupon, x.customer, x.end, x.start, x.subscription))
}
