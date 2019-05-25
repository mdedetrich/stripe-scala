package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import com.typesafe.scalalogging.LazyLogging
import defaults._
import io.circe.{Decoder, Encoder}

/**
  * @see https://stripe.com/docs/api#application_fees
  */
object ApplicationFees extends LazyLogging {

  case class ApplicationFee(
      id: String,
      amount: BigDecimal,
      application: String,
      created: OffsetDateTime,
      currency: Currency,
      originatingTransaction: String
  ) extends StripeObject

  implicit val applicationFeeDecoder: Decoder[ApplicationFee] = Decoder.forProduct6(
    "id",
    "amount",
    "application",
    "created",
    "currency",
    "originating_transaction"
  )(ApplicationFee.apply)

  implicit val applicationFeeEncoder: Encoder[ApplicationFee] = Encoder.forProduct7(
    "id",
    "object",
    "amount",
    "application",
    "created",
    "currency",
    "originating_transaction"
  )(
    x =>
      (
        x.id,
        "application_fee",
        x.amount,
        x.application,
        x.created,
        x.currency,
        x.originatingTransaction
      )
  )
}
