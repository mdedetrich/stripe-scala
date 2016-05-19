package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import org.mdedetrich.playjson.Utils._

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

  implicit val alipayReads: Reads[AliPay] = (
      (__ \ "id").read[String] ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "customer").readNullable[String] ~
      (__ \ "fingerprint").readNullable[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "payment_amount").readNullable[BigDecimal] ~
      (__ \ "payment_currency").readNullable[Currency] ~
      (__ \ "reusable").read[Boolean] ~
      (__ \ "used").read[Boolean] ~
      (__ \ "username").read[String]
  ).tupled.map((AliPay.apply _).tupled)

  implicit val alipayWrites: Writes[AliPay] = Writes(
      (alipay: AliPay) =>
        Json.obj(
            "id" -> alipay.id,
            "object" -> "alipay_account",
            "created" -> Json.toJson(alipay.created)(stripeDateTimeWrites),
            "customer" -> alipay.customer,
            "fingerprint" -> alipay.fingerprint,
            "livemode" -> alipay.livemode,
            "metadata" -> alipay.metadata,
            "payment_amount" -> alipay.paymentAmount,
            "payment_currency" -> alipay.paymentCurrency,
            "reusable" -> alipay.reusable,
            "used" -> alipay.used,
            "username" -> alipay.username
      ))
}
