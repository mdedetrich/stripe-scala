package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import dispatch.Defaults._
import dispatch._
import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.stripe.v1.Collections.ListJsonMappers
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.{IdempotencyKey, InvalidJsonModelException, Endpoint, ApiKey}
import org.mdedetrich.stripe.v1.BitcoinReceivers.BitcoinReceiver
import org.mdedetrich.stripe.v1.Cards.Card
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

import scala.concurrent.Future
import scala.util.Try

/**
  * [[PaymentSource]] is a supertype of the different availabe Stripe
  * payment types.
  */

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

case class PaymentSourceList(override val url: String,
                             override val hasMore: Boolean,
                             override val data: List[PaymentSource],
                             override val totalCount: Option[Long]
                            )(implicit reads: Reads[JsValue]) extends Collections.List[PaymentSource](
  url, hasMore, data, totalCount
)

object PaymentSourceList extends ListJsonMappers[PaymentSource] {

  implicit val paymentSourceListReadsInstance: Reads[PaymentSourceList] =
    listReads.tupled.map((PaymentSourceList.apply _).tupled)

  implicit val paymentSourceListWritesInstance: Writes[PaymentSourceList] =
    Writes((paymentSourceList: PaymentSourceList) =>
      listWrites.writes(paymentSourceList)
    )
}

object Cards extends LazyLogging {

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

  sealed abstract class CardData

  object CardData {

    case class SourceObject(expMonth: Int,
                            expYear: Int,
                            number: String,
                            addressCity: Option[String],
                            addressCountry: Option[String],
                            addressLine1: Option[String],
                            addressLine2: Option[String],
                            addressState: Option[String],
                            addressZip: Option[String],
                            cvc: Option[String],
                            metadata: Option[Map[String, String]],
                            name: Option[String]
                           ) extends CardData

    object SourceObject {
      def default(expMonth: Int,
                  expYear: Int,
                  number: String): SourceObject = SourceObject(
        expMonth,
        expYear,
        number,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )
    }

    implicit val sourceObjectReads: Reads[SourceObject] = (
      (__ \ "exp_month").read[Int] ~
        (__ \ "exp_year").read[Int] ~
        (__ \ "number").read[String] ~
        (__ \ "address_city").readNullable[String] ~
        (__ \ "address_country").readNullable[String] ~
        (__ \ "address_line1").readNullable[String] ~
        (__ \ "address_line2").readNullable[String] ~
        (__ \ "address_state").readNullable[String] ~
        (__ \ "address_zip").readNullable[String] ~
        (__ \ "cvc").readNullable[String] ~
        (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
        (__ \ "name").readNullable[String]
      ).tupled.map((SourceObject.apply _).tupled)

    implicit val sourceObjectWrites: Writes[SourceObject] =
      Writes((source: SourceObject) =>
        Json.obj(
          "object" -> "card",
          "exp_month" -> source.expMonth,
          "exp_year" -> source.expYear,
          "number" -> source.number,
          "address_city" -> source.addressCity,
          "address_country" -> source.addressCountry,
          "address_line1" -> source.addressLine1,
          "address_line2" -> source.addressLine2,
          "address_state" -> source.addressState,
          "address_zip" -> source.addressState,
          "cvc" -> source.cvc,
          "metadata" -> source.metadata,
          "name" -> source.name
        )
      )

    case class SourceToken(id: String) extends CardData

    implicit val sourceTokenReads: Reads[SourceToken] = Reads.of[String].map(SourceToken)

    implicit val sourceTokenWrites: Writes[SourceToken] =
      Writes((token: SourceToken) =>
        JsString(token.id)
      )

    case class ExternalAccountObject(expMonth: Int,
                                     expYear: Int,
                                     number: String,
                                     addressCity: Option[String],
                                     addressCountry: Option[String],
                                     addressLine1: Option[String],
                                     addressLine2: Option[String],
                                     addressState: Option[String],
                                     addressZip: Option[String],
                                     currency: Option[Currency],
                                     cvc: Option[String],
                                     defaultForCurrency: Option[Currency],
                                     metadata: Option[Map[String, String]],
                                     name: Option[String]
                                    ) extends CardData

    object ExternalAccountObject {
      def default(expMonth: Int,
                  expYear: Int,
                  number: String): ExternalAccountObject = ExternalAccountObject(
        expMonth,
        expYear,
        number,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )
    }

    implicit val externalAccountObjectReads: Reads[ExternalAccountObject] = (
      (__ \ "exp_month").read[Int] ~
        (__ \ "exp_year").read[Int] ~
        (__ \ "number").read[String] ~
        (__ \ "address_city").readNullable[String] ~
        (__ \ "address_country").readNullable[String] ~
        (__ \ "address_line1").readNullable[String] ~
        (__ \ "address_line2").readNullable[String] ~
        (__ \ "address_state").readNullable[String] ~
        (__ \ "address_zip").readNullable[String] ~
        (__ \ "currency").readNullable[Currency] ~
        (__ \ "cvc").readNullable[String] ~
        (__ \ "default_for_currency").readNullable[Currency] ~
        (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
        (__ \ "name").readNullable[String]
      ).tupled.map((ExternalAccountObject.apply _).tupled)

    implicit val externalAccountObjectWrites: Writes[ExternalAccountObject] =
      Writes((externalAccount: ExternalAccountObject) =>
        Json.obj(
          "object" -> "card",
          "exp_month" -> externalAccount.expMonth,
          "exp_year" -> externalAccount.expYear,
          "number" -> externalAccount.number,
          "address_city" -> externalAccount.addressCity,
          "address_country" -> externalAccount.addressCountry,
          "address_line1" -> externalAccount.addressLine1,
          "address_line2" -> externalAccount.addressLine2,
          "address_state" -> externalAccount.addressState,
          "address_zip" -> externalAccount.addressState,
          "currency" -> externalAccount.currency,
          "cvc" -> externalAccount.cvc,
          "default_for_currency" -> externalAccount.defaultForCurrency,
          "metadata" -> externalAccount.metadata,
          "name" -> externalAccount.name
        )
      )

    case class ExternalAccountToken(id: String) extends CardData

    implicit val externalAccountTokenReads: Reads[ExternalAccountToken] = Reads.of[String].map(ExternalAccountToken)

    implicit val externalAccountTokenWrites: Writes[ExternalAccountToken] =
      Writes((token: ExternalAccountToken) =>
        JsString(token.id)
      )

  }


  implicit val cardDataWrites: Writes[CardData] =
    Writes { (cardData: CardData) =>
      cardData match {
        case cardData: CardData.SourceObject => Json.toJson(cardData)
        case cardData: CardData.ExternalAccountObject => Json.toJson(cardData)
        case cardData: CardData.SourceToken => Json.toJson(cardData)
        case cardData: CardData.ExternalAccountToken => Json.toJson(cardData)
      }
    }

  case class CardInput(cardData: CardData,
                       metadata: Option[Map[String, String]],
                       defaultForCurrency: Option[Boolean]
                      )

  object CardInput {
    def default(cardData: CardData): CardInput = CardInput(
      cardData,
      None,
      None
    )
  }

  implicit val cardInputWrites: Writes[CardInput] =
    Writes { (cardInput: CardInput) =>
      val cardData = cardInput.cardData match {
        case cardData: CardData.ExternalAccountToken =>
          Json.obj("external_account" -> cardData)
        case cardData: CardData.ExternalAccountObject =>
          Json.obj("external_account" -> cardData)
        case cardData: CardData.SourceObject =>
          Json.obj("source" -> cardData)
        case cardData: CardData.SourceToken =>
          Json.obj("source" -> cardData)
      }

      cardData ++ Json.obj(
        "metadata" -> cardInput.metadata,
        "default_for_currency" -> cardInput.defaultForCurrency
      )
    }

  implicit val cardInputReads: Reads[CardInput] = {
    val cardData = (__ \ "external_account").read[JsValue].flatMap {
      case JsObject(_) => (__ \ "external_account").read[CardData.ExternalAccountObject].map(x => x: CardData)
      case JsString(_) => (__ \ "external_account").read[CardData.ExternalAccountToken].map(x => x: CardData)
      case _ =>
        (__ \ "source").read[JsValue].flatMap {
          case JsObject(_) => (__ \ "source").read[CardData.SourceObject].map(x => x: CardData)
          case JsString(_) => (__ \ "source").read[CardData.SourceToken].map(x => x: CardData)
          case _ => Reads[CardData](_ => JsError(ValidationError("UnknownCardData")))
        }

    }

    (cardData ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "default_for_currency").readNullable[Boolean]
      ).tupled.map((CardInput.apply _).tupled)
  }


  def create(customerId: String, cardInput: CardInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Card]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "default_for_currency" -> cardInput.defaultForCurrency.map(_.toString)
      )
    }.collect {
      case (k, Some(v)) => (k, v)
    } ++ {
      cardInput.cardData match {
        case CardData.ExternalAccountToken(id) =>
          Map("external_account" -> id)
        case CardData.SourceToken(id) =>
          Map("source" -> id)
        case externalAccount: CardData.ExternalAccountObject =>
          val map = Map(
            "object" -> Option("card"),
            "exp_month" -> Option(externalAccount.expMonth.toString),
            "exp_year" -> Option(externalAccount.expYear.toString),
            "number" -> Option(externalAccount.number),
            "address_city" -> externalAccount.addressCity,
            "address_country" -> externalAccount.addressCountry,
            "address_line1" -> externalAccount.addressLine1,
            "address_line2" -> externalAccount.addressLine2,
            "address_state" -> externalAccount.addressState,
            "address_zip" -> externalAccount.addressState,
            "currency" -> externalAccount.currency.map(_.iso.toLowerCase),
            "cvc" -> externalAccount.cvc,
            "default_for_currency" -> externalAccount.defaultForCurrency.map(_.toString),
            "name" -> externalAccount.name
          ).collect {
            case (k, Some(v)) => (k, v)
          }
          mapToPostParams(Option(map), "external_account")
        case source: CardData.SourceObject =>
          val map = Map(
            "object" -> Option("card"),
            "exp_month" -> Option(source.expMonth.toString),
            "exp_year" -> Option(source.expYear.toString),
            "number" -> Option(source.number),
            "address_city" -> source.addressCity,
            "address_country" -> source.addressCountry,
            "address_line1" -> source.addressLine1,
            "address_line2" -> source.addressLine2,
            "address_state" -> source.addressState,
            "address_zip" -> source.addressState,
            "cvc" -> source.cvc,
            "name" -> source.name
          ).collect {
            case (k, Some(v)) => (k, v)
          }
          mapToPostParams(Option(map), "source")
      }
    }

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources"

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
            val jsResult = Json.fromJson[Card](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, Option(postFormParameters), None, jsValue, errors)
              }, card => card
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }

  }

  def get(customerId: String, cardId: String)
         (implicit apiKey: ApiKey,
          endpoint: Endpoint): Future[Try[Card]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources/$cardId"

    val req = url(finalUrl).GET.as(apiKey.apiKey, "")

    Http(req).map { response =>

      parseStripeServerError(response, finalUrl, None, None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[Card](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, None, None, jsValue, errors)
              }, card => card
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }

  }


  def delete(customerId: String, cardId: String)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[DeleteResponse]] = {

    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources/$cardId"

    val req = {
      val r = url(finalUrl).DELETE.as(apiKey.apiKey, "")

      idempotencyKey match {
        case Some(key) =>
          r.addHeader(idempotencyKeyHeader, key.key)
        case None =>
          r
      }
    }

    Http(req).map { response =>

      parseStripeServerError(response, finalUrl, None, None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[DeleteResponse](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, None, None, jsValue, errors)
              }, customer => customer
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }

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

  case class TransactionList(override val url: String,
                             override val hasMore: Boolean,
                             override val data: List[Transaction],
                             override val totalCount: Option[Long]
                            ) extends Collections.List[Transaction](
    url, hasMore, data, totalCount
  )

  object TransactionList extends Collections.ListJsonMappers[Transaction] {
    implicit val transactionsReads: Reads[TransactionList] =
      listReads.tupled.map((TransactionList.apply _).tupled)

    implicit val transactionsWrites: Writes[TransactionList] =
      listWrites
  }

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
                             transactions: Option[TransactionList],
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
      (__ \ "transactions").readNullable[TransactionList] ~
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