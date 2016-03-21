package org.mdedetrich.stripe.v1

import com.github.nscala_time.time.Imports._
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.functional.syntax._
import play.api.libs.json._
import org.mdedetrich.playjson.Utils._
import enumeratum._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey}

import scala.concurrent.Future
import scala.util.Try

/**
  * @see https://stripe.com/docs/api/curl#refunds
  */

object Refunds extends LazyLogging {

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

  case class Refund(id: String,
                    amount: BigDecimal,
                    balanceTransaction: String,
                    charge: String,
                    created: DateTime,
                    currency: Currency,
                    metadata: Option[Map[String, String]],
                    reason: Reason,
                    receiptNumber: String
                   )

  implicit val refundReads: Reads[Refund] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "balance_transaction").read[String] ~
      (__ \ "charge").read[String] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "reason").read[Reason] ~
      (__ \ "receipt_number").read[String]
    ).tupled.map((Refund.apply _).tupled)


  implicit val refundWrites: Writes[Refund] =
    Writes((refundData: Refund) =>
      Json.obj(
        "id" -> refundData.id,
        "amount" -> refundData.amount,
        "balanceTransaction" -> refundData.balanceTransaction,
        "charge" -> refundData.charge,
        "created" -> Json.toJson(refundData.created)(stripeDateTimeWrites),
        "currency" -> refundData.currency,
        "metadata" -> refundData.metadata,
        "reason" -> refundData.reason,
        "receipt_number" -> refundData.receiptNumber
      )
    )

  case class RefundInput(charge: String,
                         amount: Option[BigDecimal],
                         metadata: Option[Map[String, String]] = None,
                         reason: Reason,
                         refundApplicationFee: Option[Boolean],
                         reverseTransfer: Option[Boolean]
                        )

  implicit val refundInputReads: Reads[RefundInput] = (
    (__ \ "charge").read[String] ~
      (__ \ "amount").readNullable[BigDecimal] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "reason").read[Reason] ~
      (__ \ "refund_application_fee").readNullable[Boolean] ~
      (__ \ "reverse_transfer").readNullable[Boolean]
    ).tupled.map((RefundInput.apply _).tupled)


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

  def create(refundInput: RefundInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Refund]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "charge" -> Option(refundInput.charge),
        "amount" -> refundInput.amount.map(_.toString()),
        "reason" -> Option(refundInput.reason.id),
        "refund_application_fee" -> refundInput.refundApplicationFee.map(_.toString),
        "reverse_transfer" -> refundInput.reverseTransfer.map(_.toString)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(refundInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/customers"

    createRequestPOST[Refund](finalUrl, postFormParameters, idempotencyKey, logger)

  }

  def get(id: String)
         (implicit apiKey: ApiKey,
          endpoint: Endpoint): Future[Try[Refund]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$id"

    createRequestGET[Refund](finalUrl, logger)

  }

  case class RefundListInput(charge: Option[String],
                             endingBefore: Option[String],
                             limit: Option[Long],
                             startingAfter: Option[String])
  
  object RefundListInput {
    def default: RefundListInput = RefundListInput(
      None,
      None,
      None,
      None
    )
  }

  case class RefundList(override val url: String,
                        override val hasMore: Boolean,
                        override val data: List[Refund],
                        override val totalCount: Option[Long]
                       ) extends Collections.List[Refund](
    url, hasMore, data, totalCount
  )

  object RefundList extends Collections.ListJsonMappers[Refund] {
    implicit val refundListReads: Reads[RefundList] =
      listReads.tupled.map((RefundList.apply _).tupled)

    implicit val refundListWrites: Writes[RefundList] =
      listWrites
  }
  
  def list(refundListInput: RefundListInput,
           includeTotalCount: Boolean)
          (implicit apiKey: ApiKey,
           endpoint: Endpoint): Future[Try[RefundList]] = {
    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl = if (includeTotalCount)
        "/include[]=total_count"
      else
        ""

      val baseUrl = endpoint.url + s"/v1/refunds$totalCountUrl"

      (baseUrl ?
        ("charge" -> refundListInput.charge) ?
        ("ending_before" -> refundListInput.endingBefore) ?
        ("limit" -> refundListInput.limit.map(_.toString)) ?
        ("starting_after" -> refundListInput.startingAfter)
        ).toString()

    }

    createRequestGET[RefundList](finalUrl, logger)
    
  }

}
