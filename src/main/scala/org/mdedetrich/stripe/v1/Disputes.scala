package org.mdedetrich.stripe.v1

import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.stripe.v1.Balances._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

object Disputes {

  case class DisputeEvidence(accessActivityLog: Option[String],
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
                             uncategorizedText: Option[String]) extends StripeObject

  private val disputeEvidenceReadsOne = (
    (__ \ "access_activity_log").readNullable[String] ~
      (__ \ "billing_address").readNullable[String] ~
      (__ \ "cancellation_policy").readNullable[String] ~
      (__ \ "cancellation_policy_disclosure").readNullable[String] ~
      (__ \ "cancellation_rebuttal").readNullable[String] ~
      (__ \ "customer_communication").readNullable[String] ~
      (__ \ "customer_email_address").readNullable[String] ~
      (__ \ "customer_name").readNullable[String] ~
      (__ \ "customer_purchase_ip").readNullable[String] ~
      (__ \ "customer_signature").readNullable[String] ~
      (__ \ "duplicate_charge_documentation").readNullable[String] ~
      (__ \ "duplicate_charge_explanation").readNullable[String] ~
      (__ \ "duplicate_charge_id").readNullable[String] ~
      (__ \ "product_description").readNullable[String] ~
      (__ \ "receipt").readNullable[String] ~
      (__ \ "refund_policy").readNullable[String] ~
      (__ \ "refund_policy_disclosure").readNullable[String] ~
      (__ \ "refund_refusal_explanation").readNullable[String] ~
      (__ \ "service_date").readNullable[String] ~
      (__ \ "service_documentation").readNullable[String] ~
      (__ \ "shipping_address").readNullable[String]
    ).tupled

  private val disputeEvidenceReadsTwo = (
    (__ \ "shipping_carrier").readNullable[String] ~
      (__ \ "shipping_date").readNullable[String] ~
      (__ \ "shipping_documentation").readNullable[String] ~
      (__ \ "shipping_tracking_number").readNullable[String] ~
      (__ \ "uncategorized_file").readNullable[String] ~
      (__ \ "uncategorized_text").readNullable[String]
    ).tupled

  implicit val disputeEvidenceReads: Reads[DisputeEvidence] = (
    disputeEvidenceReadsOne ~ disputeEvidenceReadsTwo
    ) { (one, two) =>
    val (accessActivityLog,
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
    shippingAddress
      ) = one

    val (shippingCarrier,
    shippingDate,
    shippingDocumentation,
    shippingTrackingNumber,
    uncategorizedFile,
    uncategorizedText
      ) = two

    DisputeEvidence(accessActivityLog,
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

  implicit val disputeEvidenceWrites: Writes[DisputeEvidence] =
    Writes((disputeEvidence: DisputeEvidence) =>
      Json.obj(
        "access_activity_log" -> disputeEvidence.accessActivityLog,
        "billing_address" -> disputeEvidence.billingAddress,
        "cancellation_policy" -> disputeEvidence.cancellationPolicy,
        "cancellation_policy_disclosure" -> disputeEvidence.cancellationPolicyDisclosure,
        "cancellation_rebuttal" -> disputeEvidence.cancellationRebuttal,
        "customer_communication" -> disputeEvidence.customerCommunication,
        "customer_email_address" -> disputeEvidence.customerEmailAddress,
        "customer_name" -> disputeEvidence.customerName,
        "customer_purchase_ip" -> disputeEvidence.customerPurchaseIp,
        "customer_signature" -> disputeEvidence.customerSignature,
        "duplicate_charge_documentation" -> disputeEvidence.duplicateChargeDocumentation,
        "duplicate_charge_explanation" -> disputeEvidence.duplicateChargeExplanation,
        "duplicate_charge_id" -> disputeEvidence.duplicateChargeId,
        "product_description" -> disputeEvidence.productDescription,
        "receipt" -> disputeEvidence.receipt,
        "refund_policy" -> disputeEvidence.refundPolicy,
        "refund_policy_disclosure" -> disputeEvidence.refundPolicyDisclosure,
        "refund_refusal_explanation" -> disputeEvidence.refundRefusalExplanation,
        "service_date" -> disputeEvidence.serviceDate,
        "service_documentation" -> disputeEvidence.serviceDocumentation,
        "shipping_address" -> disputeEvidence.shippingAddress,
        "shipping_carrier" -> disputeEvidence.shippingCarrier,
        "shipping_date" -> disputeEvidence.shippingDate,
        "shipping_documentation" -> disputeEvidence.shippingDocumentation,
        "shipping_tracking_number" -> disputeEvidence.shippingTrackingNumber,
        "uncategorized_file" -> disputeEvidence.uncategorizedFile,
        "uncategorized_text" -> disputeEvidence.uncategorizedText
      )
    )

  case class EvidenceDetails(dueBy: DateTime,
                             hasEvidence: Boolean,
                             pastDue: Boolean,
                             submissionCount: Long)

  implicit val evidenceDetailsReads: Reads[EvidenceDetails] = (
    (__ \ "due_by").read[DateTime](stripeDateTimeReads) ~
      (__ \ "has_evidence").read[Boolean] ~
      (__ \ "past_due").read[Boolean] ~
      (__ \ "submission_count").read[Long]
    ).tupled.map((EvidenceDetails.apply _).tupled)

  implicit val evidenceDetailsWrites: Writes[EvidenceDetails] =
    Writes((evidenceDetails: EvidenceDetails) =>
      Json.obj(
        "due_by" -> Json.toJson(evidenceDetails.dueBy)(stripeDateTimeWrites),
        "has_evidence" -> evidenceDetails.hasEvidence,
        "past_due" -> evidenceDetails.pastDue,
        "submission_count" -> evidenceDetails.submissionCount
      )
    )

  sealed abstract class Reason(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Reason extends Enum[Reason] {

    val values = findValues

    case object Duplicate extends Reason("duplicate")

    case object Fraudulent extends Reason("fraudulent")

    case object SubscriptionCanceled extends Reason("subscription_canceled")

    case object ProductUnacceptable extends Reason("product_unacceptable")

    case object ProductNotReceived extends Reason("product_not_received")

    case object Unrecognized extends Reason("unrecognized")

    case object CreditNotProcessed extends Reason("credit_not_processed")

    case object IncorrectAccountDetails extends Reason("incorrect_account_details")

    case object InsufficientFunds extends Reason("insufficient_funds")

    case object BankCannotProcess extends Reason("bank_cannot_process")

    case object DebitNotAuthorized extends Reason("debit_not_authorized")

    case object General extends Reason("general")

  }

  implicit val reasonFormats = EnumFormats.formats(Reason, insensitive = true)

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {

    val values = findValues

    case object WarningNeedsResponse extends Status("warning_needs_response")

    case object WarningUnderReview extends Status("warning_under_review")

    case object WarningClosed extends Status("warning_closed")

    case object NeedsResponse extends Status("needs_response")

    case object ResponseDisabled extends Status("response_disabled")

    case object UnderReview extends Status("under_review")

    case object ChargeRefunded extends Status("charge_refunded")

    case object Won extends Status("won")

    case object Lost extends Status("lost")

  }

  implicit val statusFormats = EnumFormats.formats(Status, insensitive = true)

  case class Dispute(id: String,
                     amount: BigDecimal,
                     balanceTransactions: List[BalanceTransaction],
                     charge: String,
                     created: DateTime,
                     currency: Currency,
                     evidence: DisputeEvidence,
                     evidenceDetails: EvidenceDetails,
                     isChargeRefundable: Boolean,
                     livemode: Boolean,
                     metadata: Option[Map[String, String]],
                     reason: Reason,
                     status: Status)

  implicit val disputeReads: Reads[Dispute] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "balance_transactions").read[List[BalanceTransaction]] ~
      (__ \ "charge").read[String] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "evidence").read[DisputeEvidence] ~
      (__ \ "evidence_details").read[EvidenceDetails] ~
      (__ \ "is_charge_refundable").read[Boolean] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "reason").read[Reason] ~
      (__ \ "status").read[Status]
    ).tupled.map((Dispute.apply _).tupled)

  implicit val disputeWrites: Writes[Dispute] =
    Writes((dispute: Dispute) =>
      Json.obj(
        "id" -> dispute.id,
        "object" -> "dispute",
        "amount" -> dispute.amount,
        "balance_transactions" -> dispute.balanceTransactions,
        "charge" -> dispute.charge,
        "created" -> Json.toJson(dispute.created)(stripeDateTimeWrites),
        "currency" -> dispute.currency,
        "evidence" -> dispute.evidence,
        "evidence_details" -> dispute.evidenceDetails,
        "is_charge_refundable" -> dispute.isChargeRefundable,
        "livemode" -> dispute.livemode,
        "metadata" -> dispute.metadata,
        "reason" -> dispute.reason,
        "status" -> dispute.status
      )
    )
}
