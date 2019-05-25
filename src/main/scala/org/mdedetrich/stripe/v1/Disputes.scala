package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import io.circe.{Decoder, Encoder}
import org.mdedetrich.stripe.v1.Balances._
import org.mdedetrich.stripe.v1.defaults._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Disputes extends LazyLogging {

  case class DisputeEvidence(
      accessActivityLog: Option[String],
      billingAddress: Option[String],
      cancellationPolicy: Option[String],
      cancellationPolicyDisclosure: Option[String],
      cancellationRebuttal: Option[String],
      customerCommunication: Option[String],
      customerEmailAddress: Option[String],
      customerName: Option[String],
      customerPurchaseIp: Option[String],
      customerSignature: Option[String],
      duplicateChargeDocumentation: Option[String],
      duplicateChargeExplanation: Option[String],
      duplicateChargeId: Option[String],
      productDescription: Option[String],
      receipt: Option[String],
      refundPolicy: Option[String],
      refundPolicyDisclosure: Option[String],
      refundRefusalExplanation: Option[String],
      serviceDate: Option[String],
      serviceDocumentation: Option[String],
      shippingAddress: Option[String],
      shippingCarrier: Option[String],
      shippingDate: Option[String],
      shippingDocumentation: Option[String],
      shippingTrackingNumber: Option[String],
      uncategorizedFile: Option[String],
      uncategorizedText: Option[String]
  ) extends StripeObject

  private val disputeEvidenceDecoderOne = Decoder.forProduct22(
    "access_activity_log",
    "billing_address",
    "cancellation_policy",
    "cancellation_policy_disclosure",
    "cancellation_rebuttal",
    "customer_communication",
    "customer_email_address",
    "customer_name",
    "customer_purchase_ip",
    "customer_signature",
    "duplicate_charge_documentation",
    "duplicate_charge_explanation",
    "duplicate_charge_id",
    "product_description",
    "receipt",
    "refund_policy",
    "refund_policy_disclosure",
    "refund_refusal_explanation",
    "service_date",
    "service_documentation",
    "shipping_address",
    "shipping_carrier"
  )(
    Tuple22.apply(
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String]
    )
  )

  private val disputeEvidenceDecoderTwo = Decoder.forProduct5(
    "shipping_date",
    "shipping_documentation",
    "shipping_tracking_number",
    "uncategorized_file",
    "uncategorized_text"
  )(
    Tuple5.apply(
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String]
    )
  )

  implicit val disputeEvidenceDecoder: Decoder[DisputeEvidence] = Decoder.instance[DisputeEvidence] { c =>
    for {
      one <- disputeEvidenceDecoderOne(c)
      two <- disputeEvidenceDecoderTwo(c)
    } yield {
      val (
        accessActivityLog,
        billingAddress,
        cancellationPolicy,
        cancellationPolicyDisclosure,
        cancellationRebuttal,
        customerCommunication,
        customerEmailAddress,
        customerName,
        customerPurchaseIp,
        customerSignature,
        duplicateChargeDocumentation,
        duplicateChargeExplanation,
        duplicateChargeId,
        productDescription,
        receipt,
        refundPolicy,
        refundPolicyDisclosure,
        refundRefusalExplanation,
        serviceDate,
        serviceDocumentation,
        shippingAddress,
        shippingCarrier
      )                                                                                                       = one
      val (shippingDate, shippingDocumentation, shippingTrackingNumber, uncategorizedFile, uncategorizedText) = two

      DisputeEvidence(
        accessActivityLog,
        billingAddress,
        cancellationPolicy,
        cancellationPolicyDisclosure,
        cancellationRebuttal,
        customerCommunication,
        customerEmailAddress,
        customerName,
        customerPurchaseIp,
        customerSignature,
        duplicateChargeDocumentation,
        duplicateChargeExplanation,
        duplicateChargeId,
        productDescription,
        receipt,
        refundPolicy,
        refundPolicyDisclosure,
        refundRefusalExplanation,
        serviceDate,
        serviceDocumentation,
        shippingAddress,
        shippingCarrier,
        shippingDate,
        shippingDocumentation,
        shippingTrackingNumber,
        uncategorizedFile,
        uncategorizedText
      )
    }
  }

  private val disputeEvidenceEncoderOne: Encoder[DisputeEvidence] = Encoder.forProduct22(
    "access_activity_log",
    "billing_address",
    "cancellation_policy",
    "cancellation_policy_disclosure",
    "cancellation_rebuttal",
    "customer_communication",
    "customer_email_address",
    "customer_name",
    "customer_purchase_ip",
    "customer_signature",
    "duplicate_charge_documentation",
    "duplicate_charge_explanation",
    "duplicate_charge_id",
    "product_description",
    "receipt",
    "refund_policy",
    "refund_policy_disclosure",
    "refund_refusal_explanation",
    "service_date",
    "service_documentation",
    "shipping_address",
    "shipping_carrier"
  )(
    x =>
      (
        x.accessActivityLog,
        x.billingAddress,
        x.cancellationPolicy,
        x.cancellationPolicyDisclosure,
        x.cancellationRebuttal,
        x.customerCommunication,
        x.customerEmailAddress,
        x.customerName,
        x.customerPurchaseIp,
        x.customerSignature,
        x.duplicateChargeDocumentation,
        x.duplicateChargeExplanation,
        x.duplicateChargeId,
        x.productDescription,
        x.receipt,
        x.refundPolicy,
        x.refundPolicyDisclosure,
        x.refundRefusalExplanation,
        x.serviceDate,
        x.serviceDocumentation,
        x.shippingAddress,
        x.shippingCarrier
      )
  )

  private val disputeEvidenceEncoderTwo: Encoder[DisputeEvidence] = Encoder.forProduct5(
    "shipping_date",
    "shipping_documentation",
    "shipping_tracking_number",
    "uncategorized_file",
    "uncategorized_text"
  )(
    x =>
      (
        x.shippingDate,
        x.shippingDocumentation,
        x.shippingTrackingNumber,
        x.uncategorizedFile,
        x.uncategorizedText
      )
  )

  implicit val disputeEvidenceEncoder: Encoder[DisputeEvidence] =
    Encoder.instance[DisputeEvidence](x => disputeEvidenceEncoderOne(x).deepMerge(disputeEvidenceEncoderTwo(x)))

  case class EvidenceDetails(dueBy: OffsetDateTime, hasEvidence: Boolean, pastDue: Boolean, submissionCount: Long)

  implicit val evidenceDetailsDecoder: Decoder[EvidenceDetails] = Decoder.forProduct4(
    "due_by",
    "has_evidence",
    "past_due",
    "submission_count"
  )(EvidenceDetails.apply)

  implicit val evidenceDetailsEncoder: Encoder[EvidenceDetails] = Encoder.forProduct4(
    "due_by",
    "has_evidence",
    "past_due",
    "submission_count"
  )(x => EvidenceDetails.unapply(x).get)

  sealed abstract class Reason(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Reason extends Enum[Reason] {
    val values = findValues

    case object Duplicate               extends Reason("duplicate")
    case object Fraudulent              extends Reason("fraudulent")
    case object SubscriptionCanceled    extends Reason("subscription_canceled")
    case object ProductUnacceptable     extends Reason("product_unacceptable")
    case object ProductNotReceived      extends Reason("product_not_received")
    case object Unrecognized            extends Reason("unrecognized")
    case object CreditNotProcessed      extends Reason("credit_not_processed")
    case object IncorrectAccountDetails extends Reason("incorrect_account_details")
    case object InsufficientFunds       extends Reason("insufficient_funds")
    case object BankCannotProcess       extends Reason("bank_cannot_process")
    case object DebitNotAuthorized      extends Reason("debit_not_authorized")
    case object General                 extends Reason("general")

    implicit val disputeReasonDecoder: Decoder[Reason] = enumeratum.Circe.decoder(Reason)
    implicit val disputeReasonEncoder: Encoder[Reason] = enumeratum.Circe.encoder(Reason)
  }

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {
    val values = findValues

    case object WarningNeedsResponse extends Status("warning_needs_response")
    case object WarningUnderReview   extends Status("warning_under_review")
    case object WarningClosed        extends Status("warning_closed")
    case object NeedsResponse        extends Status("needs_response")
    case object ResponseDisabled     extends Status("response_disabled")
    case object UnderReview          extends Status("under_review")
    case object ChargeRefunded       extends Status("charge_refunded")
    case object Won                  extends Status("won")
    case object Lost                 extends Status("lost")

    implicit val disputeStatusDecoder: Decoder[Status] = enumeratum.Circe.decoder(Status)
    implicit val disputeStatusEncoder: Encoder[Status] = enumeratum.Circe.encoder(Status)
  }

  case class Dispute(
      id: String,
      amount: BigDecimal,
      balanceTransactions: List[BalanceTransaction],
      charge: String,
      created: OffsetDateTime,
      currency: Currency,
      evidence: DisputeEvidence,
      evidenceDetails: EvidenceDetails,
      isChargeRefundable: Boolean,
      livemode: Boolean,
      metadata: Option[Map[String, String]],
      reason: Reason,
      status: Status
  ) extends StripeObject

  implicit val disputeDecoder: Decoder[Dispute] = Decoder.forProduct13(
    "id",
    "amount",
    "balance_transactions",
    "charge",
    "created",
    "currency",
    "evidence",
    "evidence_details",
    "is_charge_refundable",
    "livemode",
    "metadata",
    "reason",
    "status"
  )(Dispute.apply)

  implicit val disputeEncoder: Encoder[Dispute] = Encoder.forProduct14(
    "id",
    "object",
    "amount",
    "balance_transactions",
    "charge",
    "created",
    "currency",
    "evidence",
    "evidence_details",
    "is_charge_refundable",
    "livemode",
    "metadata",
    "reason",
    "status"
  )(
    x =>
      (
        x.id,
        "dispute",
        x.amount,
        x.balanceTransactions,
        x.charge,
        x.created,
        x.currency,
        x.evidence,
        x.evidenceDetails,
        x.isChargeRefundable,
        x.livemode,
        x.metadata,
        x.reason,
        x.status
      )
  )

  def get(id: String)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Dispute]] = {
    val finalUrl = endpoint.url + s"/v1/disputes/$id"

    createRequestGET[Dispute](finalUrl, logger)
  }

  def close(id: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Dispute]] = {
    val finalUrl = endpoint.url + s"/v1/disputes/$id/close"

    createRequestPOST[Dispute](finalUrl, Map.empty, idempotencyKey, logger)
  }

  case class DisputeListInput(
      created: Option[ListFilterInput] = None,
      endingBefore: Option[String] = None,
      limit: Option[String] = None,
      startingAfter: Option[String] = None
  )

  case class DisputeList(
      override val url: String,
      override val hasMore: Boolean,
      override val data: List[Dispute],
      override val totalCount: Option[Long]
  ) extends Collections.List[Dispute](url, hasMore, data, totalCount)

  object DisputeList extends Collections.ListJsonMappers[Dispute] {
    implicit val disputeListDecoder: Decoder[DisputeList] =
      listDecoder(implicitly)(DisputeList.apply)

    implicit val disputeListEncoder: Encoder[DisputeList] =
      listEncoder[DisputeList]
  }

  def list(disputeListInput: DisputeListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[DisputeList]] = {
    val finalUrl = {
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/customers$totalCountUrl"

      val created: Uri = disputeListInput.created match {
        case Some(createdInput) =>
          listFilterInputToUri(createdInput, baseUrl, "created")
        case None => baseUrl
      }

      val queries = PostParams.flatten(
        List(
          "ending_before"  -> disputeListInput.endingBefore,
          "limit"          -> disputeListInput.limit.map(_.toString),
          "starting_after" -> disputeListInput.startingAfter
        )
      )

      val query = queries.foldLeft(created.query())((a, b) => b +: a)
      created.withQuery(query)

    }

    createRequestGET[DisputeList](finalUrl, logger)
  }
}
