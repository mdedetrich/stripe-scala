package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.stream.Materializer
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import defaults._
import enumeratum._
import io.circe._
import io.circe.syntax._
import org.mdedetrich.stripe.PostParams.flatten
import org.mdedetrich.stripe.v1.Charges.FraudDetails.{StripeReport, UserReport}
import org.mdedetrich.stripe.v1.Disputes._
import org.mdedetrich.stripe.v1.Errors._
import org.mdedetrich.stripe.v1.Refunds.RefundList
import org.mdedetrich.stripe.v1.Shippings.Shipping
import org.mdedetrich.stripe.v1.Sources.{MaskedCardSource, NumberCardSource}
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Charges extends LazyLogging {

  final case class FraudDetails(
      userReport: Option[UserReport],
      stripeReport: Option[StripeReport]
  )

  object FraudDetails {

    sealed abstract class UserReport(val id: String) extends EnumEntry {
      override val entryName = id
    }

    object UserReport extends Enum[UserReport] {
      val values = findValues

      case object Safe       extends UserReport("safe")
      case object Fraudulent extends UserReport("fraudulent")

      implicit val userReportDecoder: Decoder[UserReport] = enumeratum.Circe.decoder(UserReport)
      implicit val userReportEncoder: Encoder[UserReport] = enumeratum.Circe.encoder(UserReport)
    }

    sealed abstract class StripeReport(val id: String) extends EnumEntry {
      override val entryName = id
    }

    object StripeReport extends Enum[StripeReport] {
      val values = findValues

      case object Fraudulent extends StripeReport("fraudulent")

      implicit val stripeReportDecoder: Decoder[StripeReport] = enumeratum.Circe.decoder(StripeReport)
      implicit val stripeReportEncoder: Encoder[StripeReport] = enumeratum.Circe.encoder(StripeReport)
    }
  }

  implicit val fraudDetailsDecoder: Decoder[FraudDetails] = Decoder.forProduct2(
    "user_report",
    "stripe_report"
  )(FraudDetails.apply)

  implicit val fraudDetailsEncoder: Encoder[FraudDetails] = Encoder.forProduct2(
    "user_report",
    "stripe_report"
  )(x => FraudDetails.unapply(x).get)

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {
    val values = findValues

    case object Succeeded extends Status("succeeded")
    case object Failed    extends Status("failed")
    case object Pending   extends Status("pending")

    implicit val chargeStatusDecoder: Decoder[Status] = enumeratum.Circe.decoder(Status)
    implicit val chargeStatusEncoder: Encoder[Status] = enumeratum.Circe.encoder(Status)
  }

  // Source

  sealed abstract class SourceInput

  object SourceInput {

    final case class Customer(id: String) extends SourceInput

    final case class Card(
        expMonth: Int,
        expYear: Int,
        number: String,
        cvc: Option[String],
        addressCity: Option[String],
        addressCountry: Option[String],
        addressLine1: Option[String],
        addressLine2: Option[String],
        name: Option[String],
        addressState: Option[String],
        addressZip: Option[String]
    ) extends SourceInput
        with NumberCardSource

    final case class Token(id: String) extends SourceInput
  }

  implicit val sourceInputCustomerDecoder: Decoder[SourceInput.Customer] = Decoder[String].map(SourceInput.Customer)

  implicit val sourceInputCardDecoder: Decoder[SourceInput.Card] = Decoder.forProduct11(
    "exp_month",
    "exp_year",
    "number",
    "cvc",
    "address_city",
    "address_country",
    "address_line1",
    "address_line2",
    "name",
    "address_state",
    "address_zip"
  )(SourceInput.Card.apply)

  implicit val chargeSourceInputDecoder: Decoder[SourceInput] = Decoder.instance[SourceInput] { c =>
    for {
      json <- c.as[Json]
      result <- {
        if (json.isString) {
          implicitly[Decoder[SourceInput.Customer]].apply(c)
        } else if (json.isObject) {
          implicitly[Decoder[SourceInput.Card]].apply(c)
        } else {
          Left(DecodingFailure("Invalid Charge Source Input", c.history))
        }
      }
    } yield result
  }

  implicit val chargeSourceInputCustomerEncoder: Encoder[SourceInput.Customer] =
    Encoder.instance[SourceInput.Customer](_.id.asJson)

  implicit val chargeSourceInputTokenEncoder =
    Encoder.instance[SourceInput.Token](_.id.asJson)

  implicit val chargeSourceInputCustomerCard: Encoder[SourceInput.Card] = Encoder.forProduct12(
    "exp_month",
    "exp_year",
    "number",
    "object",
    "cvc",
    "address_city",
    "address_country",
    "address_line1",
    "address_line2",
    "name",
    "address_state",
    "address_zip"
  )(
    x =>
      (
        x.expMonth,
        x.expYear,
        x.number,
        "card",
        x.cvc,
        x.addressCity,
        x.addressCountry,
        x.addressLine1,
        x.addressLine2,
        x.name,
        x.addressState,
        x.addressCity
      )
  )

  implicit val chargeSourceInputEncoder: Encoder[SourceInput] = Encoder.instance[SourceInput] {
    case s: SourceInput.Customer => implicitly[Encoder[SourceInput.Customer]].apply(s)
    case s: SourceInput.Card     => implicitly[Encoder[SourceInput.Card]].apply(s)
    case s: SourceInput.Token    => implicitly[Encoder[SourceInput.Token]].apply(s)
  }

  implicit val cardPostParams: PostParams[SourceInput.Card] = PostParams.params[SourceInput.Card] { t =>
    val mandatory = Map(
      "exp_month" -> t.expMonth.toString,
      "exp_year"  -> t.expYear.toString,
      "number"    -> t.number
    )
    val optional = Map(
      "cvc"             -> t.cvc,
      "address_city"    -> t.addressCity,
      "address_country" -> t.addressCountry,
      "address_line1"   -> t.addressLine1,
      "address_line2"   -> t.addressLine2,
      "name"            -> t.name,
      "address_state"   -> t.addressState,
      "address_zip"     -> t.addressZip
    )
    mandatory ++ flatten(optional)
  }

  implicit val sourceInputPostParams = PostParams.params[SourceInput] {
    case t: SourceInput.Token           => Map("source" -> t.id)
    case customer: SourceInput.Customer => Map("customer" -> customer.id)
    case card: SourceInput.Card         => PostParams.toPostParams(card)
  }

  sealed abstract class Source

  object Source {

    // Masked card
    final case class MaskedCard(
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
        addressZip: Option[String],
        brand: String
    ) extends Source
        with MaskedCardSource

    final case class Account(id: String, applicationName: Option[String]) extends Source

    implicit val maskedCardSourceDecoder: Decoder[MaskedCard] = Decoder.forProduct12(
      "id",
      "last4",
      "exp_month",
      "exp_year",
      "cvc",
      "address_country",
      "address_line1",
      "address_line2",
      "name",
      "address_state",
      "address_zip",
      "brand"
    )(MaskedCard.apply)

    implicit val accountSourceDecoder: Decoder[Account] = Decoder.forProduct2(
      "id",
      "application_name"
    )(Account.apply)

    implicit val maskedCardSourceEncoder: Encoder[MaskedCard] = Encoder.forProduct13(
      "id",
      "object",
      "last4",
      "exp_month",
      "exp_year",
      "cvc",
      "address_country",
      "address_line1",
      "address_line2",
      "name",
      "address_state",
      "address_zip",
      "brand"
    )(
      x =>
        (
          x.id,
          "card",
          x.last4,
          x.expMonth,
          x.expYear,
          x.cvc,
          x.addressCountry,
          x.addressLine1,
          x.addressLine2,
          x.name,
          x.addressState,
          x.addressZip,
          x.brand
        )
    )

    implicit val accountSourceEncoder: Encoder[Account] = Encoder.forProduct3(
      "id",
      "object",
      "application_name"
    )(
      x =>
        (
          x.id,
          "account",
          x.applicationName
        )
    )

  }

  implicit val sourceDecoder: Decoder[Source] = Decoder.instance[Source] { c =>
    for {
      obj <- c.as[JsonObject]
      tpe <- obj.toMap("object").as[String]
      source <- tpe match {
        case "card" =>
          val decoder: Decoder[Source.MaskedCard] = implicitly[Decoder[Source.MaskedCard]]
          decoder.apply(c)
        case "account" =>
          val decoder: Decoder[Source.Account] = implicitly[Decoder[Source.Account]]
          decoder.apply(c)
        case _ =>
          Left(DecodingFailure("Expected valid object type", c.history))
      }
    } yield source
  }

  implicit val sourceEncoder: Encoder[Source] = Encoder.instance[Source] {
    case s: Source.MaskedCard => implicitly[Encoder[Source.MaskedCard]].apply(s)
    case s: Source.Account    => implicitly[Encoder[Source.Account]].apply(s)
  }

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
  final case class Charge(
      id: String,
      amount: BigDecimal,
      amountRefunded: BigDecimal,
      applicationFee: Option[String],
      balanceTransaction: Option[String],
      captured: Boolean,
      created: OffsetDateTime,
      currency: Currency,
      customer: Option[String],
      description: Option[String],
      destination: Option[String],
      dispute: Option[Dispute],
      failureCode: Option[Code],
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
      source: Source,
      sourceTransfer: Option[String],
      statementDescriptor: Option[String],
      status: Status
  ) extends StripeObject

  private val chargeDecoderOne = Decoder.forProduct22(
    "id",
    "amount",
    "amount_refunded",
    "application_fee",
    "balance_transaction",
    "captured",
    "created",
    "currency",
    "customer",
    "description",
    "destination",
    "dispute",
    "failure_code",
    "failure_message",
    "fraud_details",
    "invoice",
    "livemode",
    "metadata",
    "order",
    "paid",
    "receipt_email",
    "receipt_number"
  )(
    Tuple22.apply(
      _: String,
      _: BigDecimal,
      _: BigDecimal,
      _: Option[String],
      _: Option[String],
      _: Boolean,
      _: OffsetDateTime,
      _: Currency,
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[Dispute],
      _: Option[Code],
      _: Option[String],
      _: Option[FraudDetails],
      _: Option[String],
      _: Boolean,
      _: Option[Map[String, String]],
      _: Option[String],
      _: Boolean,
      _: Option[String],
      _: Option[String]
    )
  )

  private val chargeDecoderTwo = Decoder.forProduct7(
    "refunded",
    "refunds",
    "shipping",
    "source",
    "source_transfer",
    "statement_descriptor",
    "status"
  )(
    Tuple7.apply(
      _: Boolean,
      _: Option[RefundList],
      _: Option[Shipping],
      _: Source,
      _: Option[String],
      _: Option[String],
      _: Status
    )
  )

  implicit val chargeDecoder: Decoder[Charge] = Decoder.instance[Charge] { c =>
    for {
      one <- chargeDecoderOne.apply(c)
      two <- chargeDecoderTwo.apply(c)
    } yield {
      val (
        id,
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
        receiptNumber
      )                                                                                      = one
      val (refunded, refunds, shipping, source, sourceTransfer, statementDescriptor, status) = two
      Charge(
        id,
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
        status
      )
    }
  }

  private val chargeEncoderOne: Encoder[Charge] = Encoder.forProduct22(
    "id",
    "object",
    "amount",
    "amount_refunded",
    "application_fee",
    "balance_transaction",
    "captured",
    "created",
    "currency",
    "customer",
    "description",
    "destination",
    "dispute",
    "failure_code",
    "failure_message",
    "fraud_details",
    "invoice",
    "livemode",
    "metadata",
    "order",
    "paid",
    "receipt_email"
  )(
    x =>
      (
        x.id,
        "charge",
        x.amount,
        x.amountRefunded,
        x.applicationFee,
        x.balanceTransaction,
        x.captured,
        x.created,
        x.currency,
        x.customer,
        x.description,
        x.destination,
        x.dispute,
        x.failureCode,
        x.failureMessage,
        x.fraudDetails,
        x.invoice,
        x.livemode,
        x.metadata,
        x.order,
        x.paid,
        x.receiptEmail
      )
  )

  private val chargeEncoderTwo: Encoder[Charge] = Encoder.forProduct8(
    "receipt_number",
    "refunded",
    "refunds",
    "shipping",
    "source",
    "source_transfer",
    "statement_descriptor",
    "status"
  )(
    x =>
      (
        x.receiptNumber,
        x.refunded,
        x.refunds,
        x.shipping,
        x.source,
        x.sourceTransfer,
        x.statementDescriptor,
        x.status
      )
  )

  implicit val chargeEncoder: Encoder[Charge] = Encoder.instance[Charge] { x =>
    chargeEncoderOne.apply(x).deepMerge(chargeEncoderTwo.apply(x))
  }

  /**
    * @see https://stripe.com/docs/api#create_charge
    * @param amount              A positive integer in the smallest currency unit (e.g 100 cents to charge \$1.00,
    *                            or 1 to charge ¥1, a 0-decimal currency) representing how much to charge the card.
    *                            The minimum amount is \$0.50 (or equivalent in charge currency).
    * @param currency            3-letter ISO code for currency.
    * @param capture             Whether or not to immediately capture the charge. When false, the charge issues
    *                            an authorization (or pre-authorization), and will need to be captured later.
    *                            Uncaptured charges expire in 7 days. For more information, see authorizing charges and settling later.
    * @param applicationFee      A fee in cents that will be applied to the charge and transferred to
    *                            the application owner's Stripe account. To use an application fee,
    *                            the request must be made on behalf of another account, using the
    *                            Stripe-Account header, an OAuth key, or the [[Charge.destination]] parameter.
    *                            For more information, see the application fees documentation.
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
  final case class ChargeInput(
      amount: BigDecimal,
      currency: Currency,
      capture: Boolean,
      applicationFee: Option[BigDecimal] = None,
      description: Option[String] = None,
      destination: Option[String] = None,
      metadata: Map[String, String] = Map.empty,
      receiptEmail: Option[String] = None,
      shipping: Option[Shipping] = None,
      customer: Option[SourceInput.Customer] = None,
      source: Option[SourceInput] = None,
      statementDescriptor: Option[String] = None
  ) extends StripeObject {
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

  implicit val chargeInputDecoder: Decoder[ChargeInput] = Decoder.forProduct12(
    "amount",
    "currency",
    "capture",
    "application_fee",
    "description",
    "destination",
    "metadata",
    "receipt_email",
    "shipping",
    "customer",
    "source",
    "statement_descriptor"
  )(ChargeInput.apply)

  implicit val chargeInputPostParams: PostParams[ChargeInput] = PostParams.params[ChargeInput] { chargeInput =>
    flatten(
      Map(
        "amount"               -> Option(chargeInput.amount.toString),
        "currency"             -> Option(chargeInput.currency.iso.toLowerCase),
        "capture"              -> Option(chargeInput.capture.toString),
        "application_fee"      -> chargeInput.applicationFee.map(_.toString),
        "description"          -> chargeInput.description,
        "destination"          -> chargeInput.destination,
        "receipt_email"        -> chargeInput.receiptEmail,
        "customer"             -> chargeInput.customer.map(_.id),
        "statement_descriptor" -> chargeInput.statementDescriptor
      )
    ) ++
      PostParams.toPostParams("metadata", chargeInput.metadata) ++
      PostParams.toPostParams(chargeInput.source)
  }

  // CRUD methods

  def create(chargeInput: ChargeInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Charge]] = {

    val postFormParameters = PostParams.toPostParams(chargeInput)

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/charges"

    createRequestPOST[Charge](finalUrl, postFormParameters, idempotencyKey, logger)
  }
}
