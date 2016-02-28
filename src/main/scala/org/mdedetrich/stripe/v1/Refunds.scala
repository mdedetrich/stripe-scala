package org.mdedetrich.stripe.v1

import com.github.nscala_time.time.Imports._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import org.mdedetrich.playjson.Utils._
import enumeratum._

/**
  * Taken from https://stripe.com/docs/api/curl#refunds
  */

object Refunds {

  sealed abstract class Reason(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Reason extends Enum[Reason] {
    val values = findValues

    case object Duplicate extends Reason("duplicate")

    case object Fraudulent extends Reason("fraudulent")

    case object RequestedByCustomer extends Reason("requested_by_customer")

  }

  implicit val reasonFormats = EnumFormats.formats(Reason, insensitive = true)

  case class RefundData(id: String,
                        amount: BigDecimal,
                        balanceTransaction: String,
                        charge: String,
                        created: DateTime,
                        currency: Currency,
                        metadata: Option[Map[String, String]],
                        reason: Reason,
                        receiptNumber: String
                       )

  implicit val refundDataReads: Reads[RefundData] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "balance_transaction").read[String] ~
      (__ \ "charge").read[String] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").read[Currency] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "reason").read[Reason] ~
      (__ \ "receipt_number").read[String]
    ).tupled.map((RefundData.apply _).tupled)


  implicit val refundDataWrites: Writes[RefundData] =
    Writes((refundData: RefundData) =>
      Json.obj(
        "id" -> refundData.id,
        "amount" -> refundData.amount,
        "balanceTransaction" -> refundData.balanceTransaction,
        "charge" -> refundData.charge,
        "created" -> refundData.created.getMillis / 1000,
        "currency" -> refundData.currency,
        "metadata" -> refundData.metadata,
        "reason" -> refundData.reason,
        "receipt_number" -> refundData.receiptNumber
      )
    )

  case class RefundsData(data: List[RefundData],
                         hasMore: Boolean,
                         totalCount: Long,
                         url: String)

  implicit val refundsDataReads: Reads[RefundsData] = (
    (__ \ "data").read[List[RefundData]] ~
      (__ \ "has_more").read[Boolean] ~
      (__ \ "total_count").read[Long] ~
      (__ \ "url").read[String]
    ).tupled.map((RefundsData.apply _).tupled)

  implicit val refundsDataWrites: Writes[RefundsData] =
    Writes((refundsData: RefundsData) =>
      Json.obj(
        "data" -> refundsData.data,
        "has_more" -> refundsData.hasMore,
        "total_count" -> refundsData.totalCount,
        "url" -> refundsData.url
      )

    )

  case class RefundInput(charge: String,
                         amount: BigDecimal,
                         metadata: Option[Map[String, String]] = None,
                         reason: Reason,
                         refundApplicationFee: Boolean,
                         reverseTransfer: Boolean
                        )

  implicit val refundInputReads: Reads[RefundInput] = (
    (__ \ "charge").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "reason").read[Reason] ~
      (__ \ "refund_application_fee").read[Boolean] ~
      (__ \ "reverse_transfer").read[Boolean]
    ).tupled.map((RefundInput.apply _ ).tupled)


  implicit val refundInputWrites: Writes[RefundInput] =
    Writes((refundInput: RefundInput) =>
      Json.obj(
        "charge" -> refundInput.charge,
        "amount" -> refundInput.amount,
        "metadata" -> refundInput.metadata,
        "reason" -> refundInput.reason,
        "refund_application_fee" -> refundInput.refundApplicationFee,
        "reverse_transfer" -> refundInput.reverseTransfer
      )
    )

}
