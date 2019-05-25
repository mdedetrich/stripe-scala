package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import defaults._
import io.circe.{Decoder, Encoder}

object Alipays {

  /**
    * @see https://stripe.com/docs/api#alipay_account_object
    * @param id
    * @param created
    * @param livemode
    * @param reusable        True if you can create multiple payments using this
    *                        account. If the account is reusable, then you can
    *                        freely choose the amount of each payment.
    * @param used            Whether this Alipay account object
    * @param username        The username for the Alipay account.
    * @param customer
    * @param fingerprint     Uniquely identifies the account and will
    *                        be the same across all Alipay account objects
    *                        that are linked to the same Alipay account.
    * @param metadata        A set of key/value pairs that you can attach
    *                        to a customer object. It can be useful for storing
    *                        additional information about the customer in a
    *                        structured format.
    * @param paymentAmount   If the Alipay account object is not reusable,
    *                        the exact amount that you can create a charge for.
    * @param paymentCurrency If the Alipay account object is not reusable,
    *                        the exact currency that you can create a charge for.
    *                        has ever been used for a payment.
    */
  case class AliPay(
      id: String,
      created: OffsetDateTime,
      livemode: Boolean,
      reusable: Boolean,
      used: Boolean,
      username: String,
      customer: Option[String] = None,
      fingerprint: Option[String] = None,
      metadata: Option[Map[String, String]] = None,
      paymentAmount: Option[BigDecimal] = None,
      paymentCurrency: Option[Currency] = None
  )

  implicit val aliPayDecoder: Decoder[AliPay] = Decoder.forProduct11(
    "id",
    "created",
    "livemode",
    "reusable",
    "used",
    "username",
    "customer",
    "fingerprint",
    "metadata",
    "payment_amount",
    "payment_currency"
  )(AliPay.apply)

  implicit val aliPayEncoder: Encoder[AliPay] = Encoder.forProduct12(
    "id",
    "object",
    "created",
    "livemode",
    "reusable",
    "used",
    "username",
    "customer",
    "fingerprint",
    "metadata",
    "payment_amount",
    "payment_currency"
  )(
    x =>
      (
        x.id,
        "alipay_account",
        x.created,
        x.customer,
        x.fingerprint,
        x.livemode,
        x.metadata,
        x.paymentAmount,
        x.paymentCurrency,
        x.reusable,
        x.used,
        x.username
      )
  )
}
