package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.Charges.FraudDetails.{StripeReport, UserReport}
import org.mdedetrich.stripe.v1.Charges.Source.Card
import org.mdedetrich.stripe.v1.Disputes._
import org.mdedetrich.stripe.v1.Errors._
import org.mdedetrich.stripe.v1.Refunds.RefundList
import org.mdedetrich.stripe.v1.Shippings.Shipping
import org.mdedetrich.stripe.v1.Sources.{MaskedCardSource, NumberCardSource}
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

object Charges extends LazyLogging {

  case class FraudDetails(
    userReport: Option[UserReport],
    stripeReport: Option[StripeReport]
  )

  object FraudDetails {

    sealed abstract class UserReport(val id: String) extends EnumEntry {
      override val entryName = id
    }

    object UserReport extends Enum[UserReport] with PlayJsonEnum[UserReport] {
      val values = findValues
      case object Safe extends UserReport("safe")
      case object Fraudulent extends UserReport("fraudulent")
    }

    sealed abstract class StripeReport(val id: String) extends EnumEntry {
      override val entryName = id
    }

    object StripeReport extends Enum[StripeReport] with PlayJsonEnum[StripeReport] {
      val values = findValues
      case object Fraudulent extends StripeReport("fraudulent")
    }
  }

  implicit val fraudDetailsReads: Reads[FraudDetails] = (
    (__ \ "user_report").readNullable[UserReport] ~
    (__ \ "stripe_report").readNullable[StripeReport]
  ).tupled.map((FraudDetails.apply _).tupled)

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {

    val values = findValues

    case object Succeeded extends Status("succeeded")

    case object Failed extends Status("failed")

    case object Pending extends Status("pending")
  }

  implicit val statusFormats = EnumFormats.formats(Status, insensitive = true)

  // Source

  sealed abstract class Source

  object Source {

    case class Customer(id: String) extends Source

    case class Card(expMonth: Int,
                    expYear: Int,
                    number: String,
                    cvc: Option[String],
                    addressCity: Option[String],
                    addressCountry: Option[String],
                    addressLine1: Option[String],
                    addressLine2: Option[String],
                    name: Option[String],
                    addressState: Option[String],
                    addressZip: Option[String])
      extends Source
        with NumberCardSource
  }

  implicit val sourceReads: Reads[Source] = {
    __.read[JsValue].flatMap {
      case jsObject: JsObject =>
        (
          (__ \ "exp_month").read[Int] ~
            (__ \ "exp_year").read[Int] ~
            (__ \ "number").read[String] ~
            (__ \ "cvc").readNullable[String] ~
            (__ \ "address_city").readNullable[String] ~
            (__ \ "address_country").readNullable[String] ~
            (__ \ "address_line1").readNullable[String] ~
            (__ \ "address_line2").readNullable[String] ~
            (__ \ "name").readNullable[String] ~
            (__ \ "address_state").readNullable[String] ~
            (__ \ "address_zip").readNullable[String]
          ).tupled.map((Source.Card.apply _).tupled)
      case jsString: JsString =>
        Reads[Source](_ => JsSuccess(Source.Customer(jsString.value)))
      case _ =>
        Reads[Source](_ => JsError(ValidationError("InvalidSource")))
    }
  }

  implicit val cardPostParams = new PostParams[Source.Card] {
    override def toMap(t: Card): Map[String, String] = {
      val mandatory = Map(
        "exp_month" -> t.expMonth.toString,
        "exp_year" -> t.expYear.toString,
        "number" -> t.number
      )
      val optional = Map(
        "cvc" -> t.cvc,
        "address_city" -> t.addressCity,
        "address_country" -> t.addressCountry,
        "address_line1" -> t.addressLine1,
        "address_line2" -> t.addressLine2,
        "name" -> t.name,
        "address_state" -> t.addressState,
        "address_zip" -> t.addressZip
      )
      mandatory ++ flatten(optional)
    }
  }

  implicit val sourceWrites: Writes[Source] = Writes((source: Source) => {
    source match {
      case Source.Customer(id) =>
        JsString(id)
      case Source.Card(expMonth,
      expYear,
      number,
      cvc,
      addressCity,
      addressCountry,
      addressLine1,
      addressLine2,
      name,
      addressState,
      addressZip) =>
        Json.obj(
          "exp_month" -> expMonth,
          "exp_year" -> expYear,
          "number" -> number,
          "object" -> "card",
          "cvc" -> cvc,
          "address_city" -> addressCity,
          "address_country" -> addressCountry,
          "address_line1" -> addressLine1,
          "address_line2" -> addressLine2,
          "name" -> name,
          "address_state" -> addressState,
          "address_zip" -> addressZip
        )
    }
  })

  // Masked card

  case class MaskedSource(
    id: String,
    last4: String,
    expMonth: Int,
    expYear: Int,
    cvc: Option[String],
    addressCountry: Option[String],
    addressLine1: Option[String],
    addressLine2: Option[String],
    name: Option[String],
    addressState: Option[String],
    addressZip: Option[String]
  ) extends MaskedCardSource

  implicit val maskedSourceReads: Reads[MaskedSource] = (
    (__ \ "id").read[String] ~
    (__ \ "last4").read[String] ~
    (__ \ "exp_month").read[Int] ~
    (__ \ "exp_year").read[Int] ~
    (__ \ "cvc").readNullable[String] ~
    (__ \ "address_country").readNullable[String] ~
    (__ \ "address_line1").readNullable[String] ~
    (__ \ "address_line2").readNullable[String] ~
    (__ \ "name").readNullable[String] ~
    (__ \ "address_state").readNullable[String] ~
    (__ \ "addressZip").readNullable[String]
  ).tupled.map((MaskedSource.apply _).tupled)

  /**
    * https://stripe.com/docs/api#charges
    *
    * @param id
    * @param amount              Amount charged in cents
    * @param amountRefunded      Amount in cents refunded (can be less than the
    *                            amount attribute on the charge if a partial
    *                            refund was issued).
    * @param applicationFee      The application fee (if any) for the charge.
    *                            See the Connect documentation for details.
    * @param balanceTransaction  ID of the balance transaction that describes the
    *                            impact of this charge on your account balance
    *                            (not including refunds or disputes).
    * @param captured            If the charge was created without capturing,
    *                            this boolean represents whether or not it is still
    *                            uncaptured or has since been captured.
    * @param created
    * @param currency            Three-letter ISO currency code representing
    *                            the currency in which the charge was made.
    * @param customer            ID of the customer this charge is for if one exists.
    * @param description
    * @param destination         The account (if any) the charge was made on behalf of.
    *                            See the Connect documentation for details.
    * @param dispute             Details about the dispute if the charge has been disputed.
    * @param failureCode         Error code explaining reason for charge failure if
    *                            available (see the errors section for a list of codes).
    * @param failureMessage      Message to user further explaining reason for charge
    *                            failure if available.
    * @param fraudDetails        Hash with information on fraud assessments for the charge.
    *                            Assessments reported by you have the key [[FraudDetails.UserReport]] and,
    *                            if set, possible values of [[FraudDetails.UserReport.Safe]] and
    *                            [[FraudDetails.UserReport.Fraudulent]]. Assessments
    *                            from Stripe have the key [[FraudDetails.StripeReport]] and, if set,
    *                            the value [[FraudDetails.StripeReport.Fraudulent]].
    * @param invoice             ID of the invoice this charge is for if one exists.
    * @param livemode
    * @param metadata            A set of key/value pairs that you can attach to a charge object.
    *                            It can be useful for storing additional information about the charge
    *                            in a structured format.
    * @param order               ID of the order this charge is for if one exists.
    * @param paid                true if the charge succeeded, or was successfully
    *                            authorized for later capture.
    * @param receiptEmail        This is the email address that the receipt for this charge was sent to.
    * @param receiptNumber       This is the transaction number that appears on email
    *                            receipts sent for this charge.
    * @param refunded            Whether or not the charge has been fully refunded.
    *                            If the charge is only partially refunded,
    *                            this attribute will still be false.
    * @param refunds             A list of refunds that have been applied to the charge.
    * @param shipping            Shipping information for the charge.
    * @param source              For most Stripe users, the source of every charge is a credit or debit card.
    *                            This hash is then the card object describing that card.
    * @param sourceTransfer      The transfer ID which created this charge. Only present if the
    *                            charge came from another Stripe account. See the Connect
    *                            documentation for details.
    * @param statementDescriptor Extra information about a charge. This will appear
    *                            on your customer’s credit card statement.
    * @param status              The status of the payment is either [[Status.Succeeded]],
    *                            [[Status.Pending]], or [[Status.Failed]].
    */
  case class Charge(id: String,
                    amount: BigDecimal,
                    amountRefunded: BigDecimal,
                    applicationFee: Option[String],
                    balanceTransaction: String,
                    captured: Boolean,
                    created: OffsetDateTime,
                    currency: Currency,
                    customer: Option[String],
                    description: Option[String],
                    destination: Option[String],
                    dispute: Option[Dispute],
                    failureCode: Option[Type],
                    failureMessage: Option[String],
                    fraudDetails: Option[FraudDetails],
                    invoice: Option[String],
                    livemode: Boolean,
                    metadata: Option[Map[String, String]],
                    order: Option[String],
                    paid: Boolean,
                    receiptEmail: Option[String],
                    receiptNumber: Option[String],
                    refunded: Boolean,
                    refunds: Option[RefundList],
                    shipping: Option[Shipping],
                    source: MaskedSource,
                    sourceTransfer: Option[String],
                    statementDescriptor: Option[String],
                    status: Status)

  private val chargeReadsOne = (
      (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "amount_refunded").read[BigDecimal] ~
      (__ \ "application_fee").readNullable[String] ~
      (__ \ "balance_transaction").read[String] ~
      (__ \ "captured").read[Boolean] ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "customer").readNullable[String] ~
      (__ \ "description").readNullable[String] ~
      (__ \ "destination").readNullable[String] ~
      (__ \ "dispute").readNullable[Dispute] ~
      (__ \ "failure_code").readNullable[Type] ~
      (__ \ "failure_message").readNullable[String] ~
      (__ \ "fraud_details").readNullableOrEmptyJsObject[FraudDetails] ~
      (__ \ "invoice").readNullable[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "order").readNullable[String] ~
      (__ \ "paid").read[Boolean] ~
      (__ \ "receipt_email").readNullable[String]
  ).tupled

  private val chargeReadsTwo = (
      (__ \ "receipt_number").readNullable[String] ~
      (__ \ "refunded").read[Boolean] ~
      (__ \ "refunds").readNullable[RefundList] ~
      (__ \ "shipping").readNullable[Shipping] ~
      (__ \ "source").read[MaskedSource] ~
      (__ \ "source_transfer").readNullable[String] ~
      (__ \ "statement_descriptor").readNullable[String] ~
      (__ \ "status").read[Status]
  ).tupled

  implicit val chargeReads: Reads[Charge] = (
      chargeReadsOne ~ chargeReadsTwo
  ) { (one, two) =>
    val (id,
         amount,
         amountRefunded,
         applicationFee,
         balanceTransaction,
         captured,
         created,
         currency,
         customer,
         description,
         destination,
         dispute,
         failureCode,
         failureMessage,
         fraudDetails,
         invoice,
         livemode,
         metadata,
         order,
         paid,
         receiptEmail) = one

    val (receiptNumber,
         refunded,
         refunds,
         shipping,
         source,
         sourceTransfer,
         statementDescriptor,
         status) = two

    Charge(id,
           amount,
           amountRefunded,
           applicationFee,
           balanceTransaction,
           captured,
           created,
           currency,
           customer,
           description,
           destination,
           dispute,
           failureCode,
           failureMessage,
           fraudDetails,
           invoice,
           livemode,
           metadata,
           order,
           paid,
           receiptEmail,
           receiptNumber,
           refunded,
           refunds,
           shipping,
           source,
           sourceTransfer,
           statementDescriptor,
           status)
  }

  /**
    * @see https://stripe.com/docs/api#create_charge
    * @param amount              A positive integer in the smallest currency unit (e.g 100 cents to charge $1.00,
    *                            or 1 to charge ¥1, a 0-decimal currency) representing how much to charge the card.
    *                            The minimum amount is $0.50 (or equivalent in charge currency).
    * @param currency            3-letter ISO code for currency.
    * @param applicationFee      A fee in cents that will be applied to the charge and transferred to
    *                            the application owner's Stripe account. To use an application fee,
    *                            the request must be made on behalf of another account, using the
    *                            Stripe-Account header, an OAuth key, or the [[Charge.destination]] parameter.
    *                            For more information, see the application fees documentation.
    * @param capture             Whether or not to immediately capture the charge. When false, the charge issues
    *                            an authorization (or pre-authorization), and will need to be captured later.
    *                            Uncaptured charges expire in 7 days. For more information, see authorizing charges and settling later.
    * @param description         An arbitrary string which you can attach to a charge object.
    *                            It is displayed when in the web interface alongside the charge.
    *                            Note that if you use Stripe to send automatic email receipts to your customers,
    *                            your receipt emails will include the [[description]] of the charge(s) that they are describing.
    * @param destination         An account to make the charge on behalf of. If specified, the charge will be attributed
    *                            to the destination account for tax reporting, and the funds from the charge will
    *                            be transferred to the destination account. The ID of the resulting transfer will be returned
    *                            in the transfer field of the response. See the documentation for details.
    * @param metadata            A set of key/value pairs that you can attach to a charge object. It can be useful for storing
    *                            additional information about the customer in a structured format. It's often a good idea
    *                            to store an email address in metadata for tracking later.
    * @param receiptEmail        The email address to send this charge's receipt to.
    *                            The receipt will not be sent until the charge is paid. If this charge is
    *                            for a customer, the email address specified here will override the customer's
    *                            email address. Receipts will not be sent for test mode charges. If [[receiptEmail]]
    *                            is specified for a charge in live mode, a receipt will be sent regardless of your email settings.
    * @param shipping            Shipping information for the charge. Helps prevent fraud on charges for physical goods.
    *                            For more information, see the Charge object documentation.
    * @param customer            The ID of an existing customer that will be charged in this request.
    * @param source              A payment source to be charged, such as a credit card. If you also pass a
    *                            customer ID, the source must be the ID of a source belonging to the customer.
    *                            Otherwise, if you do not pass a customer ID, the source you provide must
    *                            either be a token, like the ones returned by Stripe.js, or a dictionary
    *                            containing a user's credit card details, with the options described below.
    *                            Although not all information is required, the extra info helps prevent fraud.
    * @param statementDescriptor An arbitrary string to be displayed on your customer's credit
    *                            card statement. This may be up to 22 characters. As an example,
    *                            if your website is RunClub and the item you're charging for
    *                            is a race ticket, you may want to specify a [[statementDescriptor]]
    *                            of RunClub 5K race ticket. The statement description may not include `<>"'` characters,
    *                            and will appear on your customer's statement in capital letters.
    *                            Non-ASCII characters are automatically stripped. While most banks display
    *                            this information consistently, some may display it incorrectly or not at all.
    * @throws StatementDescriptorTooLong          - If [[statementDescriptor]] is longer than 22 characters
    * @throws StatementDescriptorInvalidCharacter - If [[statementDescriptor]] has an invalid character
    */
  case class ChargeInput(amount: BigDecimal,
                         currency: Currency,
                         applicationFee: Option[BigDecimal],
                         capture: Boolean,
                         description: Option[String],
                         destination: String,
                         metadata: Map[String, String],
                         receiptEmail: Option[String],
                         shipping: Option[Shipping],
                         customer: Option[Source.Customer],
                         source: Option[Source.Card],
                         statementDescriptor: Option[String])
      extends StripeObject {
    statementDescriptor match {
      case Some(sD) if sD.length > 22 =>
        throw StatementDescriptorTooLong(sD.length)
      case Some(sD) if sD.contains("<") =>
        throw StatementDescriptorInvalidCharacter("<")
      case Some(sD) if sD.contains(">") =>
        throw StatementDescriptorInvalidCharacter(">")
      case Some(sD) if sD.contains("\"") =>
        throw StatementDescriptorInvalidCharacter("\"")
      case Some(sD) if sD.contains("\'") =>
        throw StatementDescriptorInvalidCharacter("\'")
      case _ =>
    }
  }

  // ChargeInput

  object ChargeInput {
    def default(amount: BigDecimal,
                currency: Currency,
                capture: Boolean,
                destination: String,
                source: Source.Card): ChargeInput =
      ChargeInput(
        amount,
        currency,
        None,
        capture,
        None,
        destination,
        Map.empty,
        None,
        None,
        None,
        Some(source),
        None
      )

    def default(amount: BigDecimal,
                currency: Currency,
                capture: Boolean,
                destination: String,
                customer: Source.Customer): ChargeInput =
      ChargeInput(
        amount,
        currency,
        None,
        capture,
        None,
        destination,
        Map.empty,
        None,
        None,
        Some(customer),
        None,
        None
      )
  }

  implicit val chargeInputWrites: Writes[ChargeInput] = Writes(
      (chargeInput: ChargeInput) =>
        Json.obj(
            "amount" -> chargeInput.amount,
            "currency" -> chargeInput.currency,
            "application_fee" -> chargeInput.applicationFee,
            "capture" -> chargeInput.capture,
            "description" -> chargeInput.description,
            "destination" -> chargeInput.destination,
            "metadata" -> chargeInput.metadata,
            "receipt_email" -> chargeInput.receiptEmail,
            "shipping" -> chargeInput.shipping,
            "customer" -> chargeInput.customer,
            "source" -> chargeInput.source,
            "statement_descriptor" -> chargeInput.statementDescriptor
      ))

  implicit val chargeInputPostParams = new PostParams[ChargeInput] {
    override def toMap(chargeInput: ChargeInput): Map[String, String] =
      flatten(Map(
        "amount" -> Option(chargeInput.amount.toString),
        "currency" -> Option(chargeInput.currency.iso.toLowerCase),
        "application_fee" -> chargeInput.applicationFee.map(_.toString),
        "capture" -> Option(chargeInput.capture.toString),
        "description" -> chargeInput.description,
        "destination" -> Option(chargeInput.destination),
        "receipt_email" -> chargeInput.receiptEmail,
        "customer" -> chargeInput.customer.map(_.id),
        "statement_descriptor" -> chargeInput.statementDescriptor
      )) ++
        PostParams.toPostParams("metadata", chargeInput.metadata) ++
        PostParams.toPostParams("source", chargeInput.source)
  }

  // CRUD methods

  def create(chargeInput: ChargeInput)(
    idempotencyKey: Option[IdempotencyKey] = None)(
              implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[Charge]] = {

    val postFormParameters = PostParams.toPostParams(chargeInput)

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/charges"

    createRequestPOST[Charge](
      finalUrl, postFormParameters, idempotencyKey, logger)
  }
}
