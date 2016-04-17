package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import play.api.libs.functional.syntax._
import play.api.libs.json._
import org.mdedetrich.playjson.Utils._
import enumeratum._
import org.joda.time.DateTime
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

  /**
    * @see https://stripe.com/docs/api#refund_object
    * @param id
    * @param amount             Amount, in cents.
    * @param balanceTransaction Balance transaction that describes
    *                           the impact on your account balance.
    * @param charge             ID of the charge that was refunded.
    * @param created
    * @param currency           Three-letter ISO code representing the currency.
    * @param metadata           A set of key/value pairs that you can attach to
    *                           the object. It can be useful for storing additional
    *                           information in a structured format.
    * @param reason             Reason for the refund. If set, possible values are
    *                           [[Reason.Duplicate]], [[Reason.Fraudulent]], and
    *                           [[Reason.RequestedByCustomer]].
    * @param receiptNumber      This is the transaction number that appears on
    *                           email receipts sent for this refund.
    */

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

  /**
    * @see https://stripe.com/docs/api#create_refund
    * @param charge               The identifier of the charge to refund.
    * @param amount               A positive integer in cents representing
    *                             how much of this charge to refund. Can only
    *                             refund up to the unrefunded amount remaining
    *                             of the charge.
    * @param metadata             A set of key/value pairs that you can attach
    *                             to a refund object. It can be useful for
    *                             storing additional information about the
    *                             refund in a structured format. You can unset
    *                             individual keys if you POST an empty value
    *                             for that key. You can clear all keys if you
    *                             POST an empty value for metadata.
    * @param reason               String indicating the reason for the refund.
    *                             If set, possible values are [[Reason.Duplicate]],
    *                             [[Reason.Fraudulent]], and [[Reason.RequestedByCustomer]].
    *                             Specifying fraudulent as the reason when you believe the
    *                             charge to be fraudulent will help us improve our
    *                             fraud detection algorithms.
    * @param refundApplicationFee Boolean indicating whether the
    *                             application fee should be refunded
    *                             when refunding this charge. If a full
    *                             charge refund is given, the full application
    *                             fee will be refunded. Else, the application
    *                             fee will be refunded with an amount proportional
    *                             to the amount of the charge refunded. An application
    *                             fee can only be refunded by the application that
    *                             created the charge.
    * @param reverseTransfer      Boolean indicating whether the transfer
    *                             should be reversed when refunding this charge.
    *                             The transfer will be reversed for the same amount
    *                             being refunded (either the entire or partial amount).
    *                             A transfer can only be reversed by the application
    *                             that created the charge.
    */

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

  /**
    * @see https://stripe.com/docs/api#list_refunds
    * @param charge        Only return refunds for the
    *                      charge specified by this charge ID.
    * @param endingBefore  A cursor for use in pagination. [[endingBefore]]
    *                      is an object ID that defines your place in the list.
    *                      For instance, if you make a list request and
    *                      receive 100 objects, starting with obj_bar,
    *                      your subsequent call can include [[endingBefore]]=obj_bar
    *                      in order to fetch the previous page of the list.
    * @param limit         A limit on the number of objects to be returned.
    *                      Limit can range between 1 and 100 items.
    * @param startingAfter A cursor for use in pagination. [[startingAfter]]
    *                      is an object ID that defines your place in
    *                      the list. For instance, if you make a list
    *                      request and receive 100 objects, ending
    *                      with obj_foo, your subsequent call can include
    *                      [[startingAfter]]=obj_foo in order to fetch the
    *                      next page of the list.
    */

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
