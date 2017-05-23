package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import defaults._
import io.circe.{Decoder, Encoder}

object Alipays {

  /**
    * @see https://stripe.com/docs/api#alipay_account_object
    * @param id
    * @param created
    * @param customer
    * @param fingerprint     Uniquely identifies the account and will
    *                        be the same across all Alipay account objects
    *                        that are linked to the same Alipay account.
    * @param livemode
    * @param metadata        A set of key/value pairs that you can attach
    *                        to a customer object. It can be useful for storing
    *                        additional information about the customer in a
    *                        structured format.
    * @param paymentAmount   If the Alipay account object is not reusable,
    *                        the exact amount that you can create a charge for.
    * @param paymentCurrency If the Alipay account object is not reusable,
    *                        the exact currency that you can create a charge for.
    * @param reusable        True if you can create multiple payments using this
    *                        account. If the account is reusable, then you can
    *                        freely choose the amount of each payment.
    * @param used            Whether this Alipay account object
    *                        has ever been used for a payment.
    * @param username        The username for the Alipay account.
    */
  case class AliPay(id: String,
                    created: OffsetDateTime,
                    customer: Option[String],
                    fingerprint: Option[String],
                    livemode: Boolean,
                    metadata: Option[Map[String, String]],
                    paymentAmount: Option[BigDecimal],
                    paymentCurrency: Option[Currency],
                    reusable: Boolean,
                    used: Boolean,
                    username: String)

  object AliPay {
    def default(id: String,
                created: OffsetDateTime,
                livemode: Boolean,
                reusable: Boolean,
                used: Boolean,
                username: String): AliPay = AliPay(
      id,
      created,
      None,
      None,
      livemode,
      None,
      None,
      None,
      reusable,
      used,
      username
    )
  }

  implicit val aliPayDecoder: Decoder[AliPay] = Decoder.forProduct11(
    "id",
    "created",
    "customer",
    "fingerprint",
    "livemode",
    "metadata",
    "payment_amount",
    "payment_currency",
    "reusable",
    "used",
    "username"
  )(AliPay.apply)

  implicit val aliPayEncoder: Encoder[AliPay] = Encoder.forProduct12(
    "id",
    "object",
    "created",
    "customer",
    "fingerprint",
    "livemode",
    "metadata",
    "payment_amount",
    "payment_currency",
    "reusable",
    "used",
    "username"
  )(
    x =>
      (x.id,
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
       x.username))
}
