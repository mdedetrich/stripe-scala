package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import dispatch.Defaults._
import dispatch._
import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.Cards._
import org.mdedetrich.stripe.v1.Disputes._
import org.mdedetrich.stripe.v1.Errors._
import org.mdedetrich.stripe.v1.Refunds.RefundList
import org.mdedetrich.stripe.v1.Shippings.Shipping
import org.mdedetrich.stripe.v1.Sources.BaseCardSource
import org.mdedetrich.stripe.{IdempotencyKey, ApiKey, Endpoint, InvalidJsonModelException}
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

object Charges extends LazyLogging {

  /**
    * @see https://stripe.com/docs/fraud
    * @param stripeReport
    */

  case class FraudDetails(stripeReport: String)

  implicit val fraudDetailsReads: Reads[FraudDetails] =
    (__ \ "stripe_report").read[String]
      .map { stripeReport => FraudDetails(stripeReport) }

  implicit val fraudDetailsWrites: Writes[FraudDetails] =
    Writes((fraudDetails: FraudDetails) =>
      Json.obj(
        "stripe_report" -> fraudDetails.stripeReport
      )
    )

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {

    val values = findValues

    case object Succeeded extends Status("succeeded")

    case object Failed extends Status("failed")

  }

  implicit val statusFormats = EnumFormats.formats(Status, insensitive = true)

  case class Charge(id: String,
                    amount: BigDecimal,
                    amountRefunded: BigDecimal,
                    applicationFee: Option[String],
                    balanceTransaction: String,
                    captured: Boolean,
                    created: DateTime,
                    currency: Currency,
                    customer: Option[String],
                    description: String,
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
                    source: Card,
                    statementDescriptor: Option[String],
                    status: Status)

  object Charge {
    def default(id: String,
                amount: BigDecimal,
                amountRefunded: BigDecimal,
                balanceTransaction: String,
                captured: Boolean,
                created: DateTime,
                currency: Currency,
                description: String,
                livemode: Boolean,
                paid: Boolean,
                refunded: Boolean,
                source: Card,
                status: Status
               ): Charge = Charge(
      id,
      amount,
      amountRefunded,
      None,
      balanceTransaction,
      captured,
      created,
      currency,
      None,
      description,
      None,
      None,
      None,
      None,
      None,
      None,
      livemode,
      None,
      None,
      paid,
      None,
      None,
      refunded,
      None,
      None,
      source,
      None,
      status
    )
  }

  private val chargeReadsOne = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "amount_refunded").read[BigDecimal] ~
      (__ \ "application_fee").readNullable[String] ~
      (__ \ "balance_transaction").read[String] ~
      (__ \ "captured").read[Boolean] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "customer").readNullable[String] ~
      (__ \ "description").read[String] ~
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
      (__ \ "source").read[Card] ~
      (__ \ "statement_descriptor").readNullable[String] ~
      (__ \ "status").read[Status]
    ).tupled

  implicit val chargeReads: Reads[Charge] = (
    chargeReadsOne ~ chargeReadsTwo
    ) { (one, two) =>
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
      receiptEmail) = one

    val (receiptNumber,
    refunded,
    refunds,
    shipping,
    source,
    statementDescriptor,
    status
      ) = two

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
      statementDescriptor,
      status
    )
  }

  implicit val chargeWrites: Writes[Charge] =
    Writes((charge: Charge) =>
      Json.obj(
        "id" -> charge.id,
        "object" -> "charge",
        "amount" -> charge.amount,
        "amount_refunded" -> charge.amountRefunded,
        "application_fee" -> charge.applicationFee,
        "balance_transaction" -> charge.balanceTransaction,
        "captured" -> charge.captured,
        "created" -> Json.toJson(charge.created)(stripeDateTimeWrites),
        "currency" -> charge.currency,
        "customer" -> charge.customer,
        "description" -> charge.description,
        "dispute" -> charge.dispute,
        "failure_code" -> charge.failureCode,
        "failure_message" -> charge.failureMessage,
        "fraud_details" -> charge.fraudDetails,
        "invoice" -> charge.invoice,
        "livemode" -> charge.livemode,
        "metadata" -> charge.metadata,
        "order" -> charge.order,
        "paid" -> charge.paid,
        "receipt_email" -> charge.receiptEmail
      )
    )

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
                    addressZip: Option[String]
                   ) extends Source with BaseCardSource

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
        __.read[String].map { customerId => Source.Customer(customerId) }
      case _ =>
        Reads[Source](_ => JsError(ValidationError("InvalidSource")))
    }
  }

  implicit val sourceWrites: Writes[Source] =
    Writes((source: Source) => {
      source match {
        case Source.Customer(id) =>
          JsString(id)
        case Source.Card
          (expMonth,
          expYear,
          number,
          cvc,
          addressCity,
          addressCountry,
          addressLine1,
          addressLine2,
          name,
          addressState,
          addressZip
          ) =>

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
    }
    )

  case class ChargeInput(amount: BigDecimal,
                         currency: Currency,
                         applicationFee: Option[BigDecimal],
                         capture: Boolean,
                         description: Option[String],
                         destination: String,
                         metadata: Option[Map[String, String]],
                         receiptEmail: Option[String],
                         shipping: Option[Shipping],
                         customer: Option[String],
                         source: Source,
                         statementDescriptor: Option[String]) extends StripeObject

  object ChargeInput {
    def default(amount: BigDecimal,
                currency: Currency,
                capture: Boolean,
                destination: String,
                source: Source): ChargeInput = ChargeInput(
      amount,
      currency,
      None,
      capture,
      None,
      destination,
      None,
      None,
      None,
      None,
      source,
      None
    )
  }

  implicit val chargeInputReads: Reads[ChargeInput] = (
    (__ \ "amount").read[BigDecimal] ~
      (__ \ "currency").read[Currency] ~
      (__ \ "application_fee").readNullable[BigDecimal] ~
      (__ \ "capture").read[Boolean] ~
      (__ \ "description").readNullable[String] ~
      (__ \ "destination").read[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "receipt_email").readNullable[String] ~
      (__ \ "shipping").readNullableOrEmptyJsObject[Shipping] ~
      (__ \ "customer").readNullable[String] ~
      (__ \ "source").read[Source] ~
      (__ \ "statement_descriptor").readNullable[String]
    ).tupled.map((ChargeInput.apply _).tupled)

  implicit val chargeInputWrites: Writes[ChargeInput] =
    Writes((chargeInput: ChargeInput) =>
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
      )
    )

  def create(chargeInput: ChargeInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Charge]] = {

    val postFormParameters: Map[String, String] = {
      Map(
        "amount" -> Option(chargeInput.amount.toString),
        "currency" -> Option(chargeInput.currency.iso.toLowerCase),
        "application_fee" -> chargeInput.applicationFee.map(_.toString),
        "capture" -> Option(chargeInput.capture.toString),
        "description" -> chargeInput.description,
        "destination" -> Option(chargeInput.destination),
        "receipt_email" -> chargeInput.receiptEmail,
        "customer" -> chargeInput.customer,
        "statement_descriptor" -> chargeInput.statementDescriptor
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(chargeInput.metadata, "metadata") ++ {
      chargeInput.source match {
        case Source.Customer(id) =>
          Map("source" -> id)
        case Source.Card
          (expMonth,
          expYear,
          number,
          cvc,
          addressCity,
          addressCountry,
          addressLine1,
          addressLine2,
          name,
          addressState,
          addressZip
          ) =>
          val map: Map[String, String] = Map(
            "exp_month" -> Option(expMonth.toString),
            "exp_year" -> Option(expYear.toString),
            "number" -> Option(number),
            "cvc" -> cvc,
            "address_city" -> addressCity,
            "address_country" -> addressCountry,
            "address_line1" -> addressLine1,
            "address_line2" -> addressLine2,
            "name" -> name,
            "address_state" -> addressState,
            "address_zip" -> addressZip
          ).collect {
            case (k, Some(v)) => (k, v)
          }

          mapToPostParams(Option(map), "card")
      }

    }

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/charges"

    createRequestPOST[Charge](finalUrl, postFormParameters, idempotencyKey, logger)
  }
}
