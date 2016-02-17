package org.mdedetrich.stripe.v1

import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

object TransferReversals {

  case class TransferReversal(id: String,
                              amount: BigDecimal,
                              balanceTransaction: String,
                              created: DateTime,
                              currency: Currency,
                              metadata: Option[Map[String,String]],
                              transfer: String) extends StripeObject

  implicit val transferReversalReads: Reads[TransferReversal] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "balance_transaction").read[String] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").read[Currency] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String,String]] ~
      (__ \ "transfer").read[String]
    ).tupled.map(TransferReversal.tupled)

  implicit val transferReversalWrites: Writes[TransferReversal] =
    Writes((transferReversal: TransferReversal) =>
      Json.obj(
        "id" -> transferReversal.id,
        "object" -> "transfer_reversal",
        "amount" -> transferReversal.amount,
        "balance_transaction" -> transferReversal.balanceTransaction,
        "created" -> transferReversal.created.getMillis / 1000,
        "currency" -> transferReversal.currency,
        "metadata" -> transferReversal.metadata,
        "transfer" -> transferReversal.transfer
      )
    )

}
