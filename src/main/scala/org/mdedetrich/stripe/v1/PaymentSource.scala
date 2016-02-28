package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import dispatch.Defaults._
import dispatch._
import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.stripe.{IdempotencyKey, InvalidJsonModelException, Endpoint, ApiKey}
import org.mdedetrich.stripe.v1.BitcoinReceivers.BitcoinReceiver
import org.mdedetrich.stripe.v1.Cards.Card
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

import scala.concurrent.Future
import scala.util.Try

sealed trait PaymentSource

object PaymentSource extends LazyLogging {

  implicit val paymentSourcesReads: Reads[PaymentSource] =
    __.read[JsObject].flatMap { o =>
      (__ \ "object").read[String].flatMap {
        case "card" => __.read[Card].map(x => x: PaymentSource)
        case "bitcoin_receiver" => __.read[BitcoinReceiver].map(x => x: PaymentSource)
        case _ => Reads[PaymentSource](_ => JsError(ValidationError("UnknownPaymentSource")))
      }
    }

  implicit val paymentSourceWrites: Writes[PaymentSource] =
    Writes((paymentSource: PaymentSource) =>
      paymentSource match {
        case c: Card => Json.toJson(c)
        case b: BitcoinReceiver => Json.toJson(b)
      }
    )
}

object Cards {

  sealed abstract class Brand(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Brand extends Enum[Brand] {

    val values = findValues

    case object Visa extends Brand("Visa")

    case object `American Express` extends Brand("American Express")

    case object MasterCard extends Brand("MasterCard")

    case object Discover extends Brand("Discover")

    case object JCB extends Brand("JCB")

    case object `Diners Club` extends Brand("Diners Club")

    case object Unknown extends Brand("Unknown")

  }

  implicit val brandFormats = EnumFormats.formats(Brand, insensitive = true)

  sealed abstract class Check(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Check extends Enum[Check] {
    val values = findValues

    case object Pass extends Check("pass")

    case object Fail extends Check("fail")

    case object Unavailable extends Check("unavailable")

    case object Unchecked extends Check("unchecked")

  }

  implicit val checkFormats = EnumFormats.formats(Check, insensitive = true)

  sealed abstract class Funding(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Funding extends Enum[Funding] {
    val values = findValues

    case object Credit extends Funding("credit")

    case object Debit extends Funding("debit")

    case object Prepaid extends Funding("prepaid")

    case object Unknown extends Funding("unknown")

  }

  implicit val fundingFormats = EnumFormats.formats(Funding, insensitive = true)

  sealed abstract class TokenizationMethod(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object TokenizationMethod extends Enum[TokenizationMethod] {

    val values = findValues

    case object ApplePay extends TokenizationMethod("apple_pay")

    case object AndroidPay extends TokenizationMethod("android_pay")

  }

  implicit val tokenizationMethodFormats = EnumFormats.formats(TokenizationMethod, insensitive = true)

  case class Card(id: String,
                  addressCity: Option[String],
                  addressCountry: Option[String],
                  addressLine1: Option[String],
                  addressLine1Check: Option[Check],
                  addressLine2: Option[String],
                  addressState: Option[String],
                  addressZip: Option[String],
                  addressZipCheck: Option[Check],
                  brand: Brand,
                  country: Option[String],
                  customer: Option[String],
                  cvcCheck: Option[Check],
                  dynamicLast4: Option[String],
                  expMonth: Int,
                  expYear: Int,
                  funding: Funding,
                  last4: String,
                  metadata: Option[Map[String, String]],
                  name: Option[String],
                  tokenizationMethod: Option[TokenizationMethod]) extends StripeObject with PaymentSource

  object Card {
    def default(id: String,
                brand: Brand,
                expMonth: Int,
                expYear: Int,
                funding: Funding,
                last4: String): Card = Card(
      id,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      brand,
      None,
      None,
      None,
      None,
      expMonth,
      expYear,
      funding,
      last4,
      None,
      None,
      None
    )
  }

  implicit val cardReads: Reads[Card] = (
    (__ \ "id").read[String] ~
      (__ \ "address_city").readNullable[String] ~
      (__ \ "address_country").readNullable[String] ~
      (__ \ "address_line1").readNullable[String] ~
      (__ \ "address_line1_check").readNullable[Check] ~
      (__ \ "address_line2").readNullable[String] ~
      (__ \ "address_state").readNullable[String] ~
      (__ \ "address_zip").readNullable[String] ~
      (__ \ "address_zip_check").readNullable[Check] ~
      (__ \ "brand").read[Brand] ~
      (__ \ "country").readNullable[String] ~
      (__ \ "customer").readNullable[String] ~
      (__ \ "cvc_check").readNullable[Check] ~
      (__ \ "dynamic_last4").readNullable[String] ~
      (__ \ "exp_month").read[Int] ~
      (__ \ "exp_year").read[Int] ~
      (__ \ "funding").read[Funding] ~
      (__ \ "last4").read[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "name").readNullable[String] ~
      (__ \ "tokenization_method").readNullable[TokenizationMethod]
    ).tupled.map((Card.apply _).tupled)

  implicit val cardWrites: Writes[Card] =
    Writes((card: Card) =>
      Json.obj(
        "id" -> card.id,
        "object" -> "card",
        "address_city" -> card.addressCity,
        "address_country" -> card.addressCountry,
        "address_line1" -> card.addressLine1,
        "address_line1_check" -> card.addressLine1Check,
        "address_line2" -> card.addressLine2,
        "address_state" -> card.addressState,
        "address_zip" -> card.addressZip,
        "address_zip_check" -> card.addressZipCheck,
        "brand" -> card.brand,
        "country" -> card.country,
        "customer" -> card.customer,
        "cvc_check" -> card.cvcCheck,
        "dynamic_last4" -> card.dynamicLast4,
        "exp_month" -> card.expMonth,
        "exp_year" -> card.expYear,
        "funding" -> card.funding,
        "last4" -> card.last4,
        "metadata" -> card.metadata,
        "name" -> card.name,
        "tokenization_method" -> card.tokenizationMethod
      )
    )

}

object BitcoinReceivers extends LazyLogging {

  case class Transaction(id: String,
                         amount: BigDecimal,
                         bitcoinAmount: BigDecimal,
                         created: DateTime,
                         currency: Currency,
                         receiver: String
                        )

  implicit val transactionReads: Reads[Transaction] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "bitcoin_amount").read[BigDecimal] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "receiver").read[String]
    ).tupled.map((Transaction.apply _).tupled)

  implicit val transactionWrites: Writes[Transaction] =
    Writes((transaction: Transaction) =>
      Json.obj(
        "id" -> transaction.id,
        "object" -> "list",
        "amount" -> transaction.amount,
        "bitcoin_amount" -> transaction.bitcoinAmount,
        "created" -> Json.toJson(transaction.created)(stripeDateTimeWrites),
        "currency" -> transaction.currency,
        "receiver" -> transaction.receiver
      )
    )

  case class BitcoinTransactions(data: List[Transaction],
                                 hasMore: Boolean,
                                 totalCount: Long,
                                 url: String
                                )

  implicit val bitcoinTransactionsReads: Reads[BitcoinTransactions] = (
    (__ \ "data").read[List[Transaction]] ~
      (__ \ "has_more").read[Boolean] ~
      (__ \ "total_count").read[Long] ~
      (__ \ "url").read[String]
    ).tupled.map((BitcoinTransactions.apply _).tupled)

  implicit val bitcoinTransactionsWrites: Writes[BitcoinTransactions] =
    Writes((bitcoinTransactions: BitcoinTransactions) =>
      Json.obj(
        "data" -> bitcoinTransactions.data,
        "has_more" -> bitcoinTransactions.hasMore,
        "total_count" -> bitcoinTransactions.totalCount,
        "url" -> bitcoinTransactions.url
      )
    )

  case class BitcoinReceiver(id: String,
                             active: Boolean,
                             amount: BigDecimal,
                             amountReceived: BigDecimal,
                             bitcoinAmount: BigDecimal,
                             bitcoinAmountReceived: BigDecimal,
                             bitcoinUri: String,
                             created: DateTime,
                             currency: Currency,
                             customer: String,
                             description: String,
                             email: String,
                             filled: Boolean,
                             inboundAddress: String,
                             livemode: Boolean,
                             metadata: Option[Map[String, String]],
                             payment: Option[String],
                             refundAddress: Option[String],
                             transactions: Option[BitcoinTransactions],
                             uncapturedFunds: Boolean,
                             usedForPayment: Boolean
                            ) extends StripeObject with PaymentSource

  object BitcoinReceiver {
    def default(id: String,
                active: Boolean,
                amount: BigDecimal,
                amountReceived: BigDecimal,
                bitcoinAmount: BigDecimal,
                bitcoinAmountReceived: BigDecimal,
                bitcoinUri: String,
                created: DateTime,
                currency: Currency,
                customer: String,
                description: String,
                email: String,
                filled: Boolean,
                inboundAddress: String,
                livemode: Boolean,
                uncapturedFunds: Boolean,
                usedForPayment: Boolean
               ): BitcoinReceiver = BitcoinReceiver(
      id,
      active,
      amount,
      amountReceived,
      bitcoinAmount,
      bitcoinAmountReceived,
      bitcoinUri,
      created,
      currency,
      customer,
      description,
      email,
      filled,
      inboundAddress,
      livemode,
      None,
      None,
      None,
      None,
      uncapturedFunds,
      usedForPayment
    )
  }

  implicit val bitcoinReceiverReads: Reads[BitcoinReceiver] = (
    (__ \ "id").read[String] ~
      (__ \ "active").read[Boolean] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "amount_received").read[BigDecimal] ~
      (__ \ "bitcoin_amount").read[BigDecimal] ~
      (__ \ "bitcoin_amount_received").read[BigDecimal] ~
      (__ \ "bitcoin_uri").read[String] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "customer").read[String] ~
      (__ \ "description").read[String] ~
      (__ \ "email").read[String] ~
      (__ \ "filled").read[Boolean] ~
      (__ \ "inbound_address").read[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "payment").readNullable[String] ~
      (__ \ "refund_address").readNullable[String] ~
      (__ \ "transactions").readNullable[BitcoinTransactions] ~
      (__ \ "uncaptured_funds").read[Boolean] ~
      (__ \ "used_for_payment").read[Boolean]
    ).tupled.map((BitcoinReceiver.apply _).tupled)

  implicit val bitcoinReceiverWrites: Writes[BitcoinReceiver] =
    Writes((bitcoinReceiver: BitcoinReceiver) =>
      Json.obj(
        "id" -> bitcoinReceiver.id,
        "object" -> "bitcoin_receiver",
        "active" -> bitcoinReceiver.active,
        "amount" -> bitcoinReceiver.amount,
        "amount_received" -> bitcoinReceiver.amountReceived,
        "bitcoin_amount" -> bitcoinReceiver.bitcoinAmount,
        "bitcoin_amount_received" -> bitcoinReceiver.bitcoinAmountReceived,
        "bitcoin_uri" -> bitcoinReceiver.bitcoinUri,
        "created" -> Json.toJson(bitcoinReceiver.created)(stripeDateTimeWrites),
        "currency" -> bitcoinReceiver.currency,
        "customer" -> bitcoinReceiver.customer,
        "description" -> bitcoinReceiver.description,
        "email" -> bitcoinReceiver.email,
        "filled" -> bitcoinReceiver.filled,
        "inbound_address" -> bitcoinReceiver.inboundAddress,
        "livemode" -> bitcoinReceiver.livemode,
        "metadata" -> bitcoinReceiver.metadata,
        "payment" -> bitcoinReceiver.payment,
        "refund_address" -> bitcoinReceiver.refundAddress,
        "transactions" -> bitcoinReceiver.transactions,
        "uncaptured_funds" -> bitcoinReceiver.uncapturedFunds,
        "used_for_payment" -> bitcoinReceiver.usedForPayment
      )
    )

  case class BitcoinReceiverInput(amount: BigDecimal,
                                  currency: Currency,
                                  email: String,
                                  description: Option[String],
                                  metadata: Option[Map[String, String]],
                                  refundMispayments: Option[Boolean]
                                 )

  object BitcoinReceiverInput {
    def default(amount: BigDecimal,
                currency: Currency,
                email: String): BitcoinReceiverInput = BitcoinReceiverInput(
      amount,
      currency,
      email,
      None,
      None,
      None
    )
  }

  implicit val bitcoinReceiverInputReads: Reads[BitcoinReceiverInput] = (
    (__ \ "amount").read[BigDecimal] ~
      (__ \ "currency").read[Currency] ~
      (__ \ "email").read[String] ~
      (__ \ "description").readNullable[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "refund_mispayments").readNullable[Boolean]
    ).tupled.map((BitcoinReceiverInput.apply _).tupled)

  implicit val bitcoinReceiverInputWrites: Writes[BitcoinReceiverInput] =
    Writes((bitcoinReceiverInput: BitcoinReceiverInput) =>
      Json.obj(
        "amount" -> bitcoinReceiverInput.amount,
        "currency" -> bitcoinReceiverInput.currency,
        "email" -> bitcoinReceiverInput.email,
        "description" -> bitcoinReceiverInput.description,
        "metadata" -> bitcoinReceiverInput.metadata,
        "refund_mispayments" -> bitcoinReceiverInput.refundMispayments
      )
    )

  def create(bitcoinReceiverInput: BitcoinReceiverInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[BitcoinReceiver]] = {

    val postFormParameters: Map[String, String] = {
      Map(
        "amount" -> Option(bitcoinReceiverInput.amount.toString()),
        "currency" -> Option(bitcoinReceiverInput.currency.iso.toLowerCase()),
        "email" -> Option(bitcoinReceiverInput.email),
        "description" -> bitcoinReceiverInput.description,
        "refund_mispayments" -> Option(bitcoinReceiverInput.refundMispayments.toString)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(bitcoinReceiverInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/bitcoin/receivers"

    val req = {
      val r = (
        url(finalUrl)
          .addHeader("Content-Type", "application/x-www-form-urlencoded")
          << postFormParameters
        ).POST.as(apiKey.apiKey, "")

      idempotencyKey match {
        case Some(key) =>
          r.addHeader(idempotencyKeyHeader, key.key)
        case None =>
          r
      }
    }

    Http(req).map { response =>

      parseStripeServerError(response, finalUrl, Option(postFormParameters), None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[BitcoinReceiver](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, Option(postFormParameters), None, jsValue, errors)
              }, bitcoinReceiver => bitcoinReceiver
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }

}