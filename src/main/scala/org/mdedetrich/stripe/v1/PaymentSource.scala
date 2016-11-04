package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime
import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.BankAccountsPaymentSource.BankAccount
import org.mdedetrich.stripe.v1.BitcoinReceivers.BitcoinReceiver
import org.mdedetrich.stripe.v1.Cards.Card
import org.mdedetrich.stripe.v1.Collections.ListJsonMappers
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey}
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/**
  * [[PaymentSource]] is a supertype of the different available Stripe
  * payment types.
  */
sealed trait PaymentSource

object PaymentSource extends LazyLogging {

  implicit val paymentSourcesReads: Reads[PaymentSource] =
    __.read[JsObject].flatMap { o =>
      (__ \ "object").read[String].flatMap {
        case "card"         => __.read[Card].map(x => x: PaymentSource)
        case "bank_account" => __.read[BankAccount].map(_.asInstanceOf[PaymentSource])
        case "bitcoin_receiver" =>
          __.read[BitcoinReceiver].map(x => x: PaymentSource)
        case unknown =>
          Reads[PaymentSource](_ => JsError(ValidationError(s"UnknownPaymentSource: $unknown")))
      }
    }

  implicit val paymentSourceWrites: Writes[PaymentSource] = Writes((paymentSource: PaymentSource) =>
    paymentSource match {
      case c: Card            => Json.toJson(c)(Cards.cardWrites)
      case b: BitcoinReceiver => Json.toJson(b)
      case ba: BankAccount    => Json.toJson(ba)(BankAccountsPaymentSource.bankAccountWrites)
  })
}

case class PaymentSourceList(override val url: String,
                             override val hasMore: Boolean,
                             override val data: List[PaymentSource],
                             override val totalCount: Option[Long])(implicit reads: Reads[JsValue])
    extends Collections.List[PaymentSource](
      url,
      hasMore,
      data,
      totalCount
    )

object PaymentSourceList extends ListJsonMappers[PaymentSource] {

  implicit val paymentSourceListReadsInstance: Reads[PaymentSourceList] =
    listReads.tupled.map((PaymentSourceList.apply _).tupled)

  implicit val paymentSourceListWritesInstance: Writes[PaymentSourceList] =
    Writes((paymentSourceList: PaymentSourceList) => listWrites.writes(paymentSourceList))
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

  implicit val fundingFormats =
    EnumFormats.formats(Funding, insensitive = true)

  sealed abstract class TokenizationMethod(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object TokenizationMethod extends Enum[TokenizationMethod] {

    val values = findValues

    case object ApplePay extends TokenizationMethod("apple_pay")

    case object AndroidPay extends TokenizationMethod("android_pay")
  }

  implicit val tokenizationMethodFormats =
    EnumFormats.formats(TokenizationMethod, insensitive = true)

  /**
    * @see https://stripe.com/docs/api#card_object
    * @param id                 ID of card (used in conjunction with a customer or recipient ID)
    * @param account            The account this card belongs to. This attribute will not be in the
    *                           card object if the card belongs to a customer or recipient instead.
    * @param addressCity
    * @param addressCountry     Billing address country, if provided when creating card
    * @param addressLine1
    * @param addressLine1Check  If [[addressLine1]] was provided, results of the check:
    *                           [[Check.Pass]], [[Check.Fail]], [[Check.Unavailable]], or
    *                           [[Check.Unchecked]].
    * @param addressLine2
    * @param addressState
    * @param addressZip
    * @param addressZipCheck    If [[addressZip]] was provided, results of the check:
    *                           [[Check.Pass]], [[Check.Fail]], [[Check.Unavailable]],
    *                           or [[Check.Unchecked]].
    * @param brand              Card brand. Can be [[Brand.Visa]], [[Brand.`American Express`]],
    *                           [[Brand.MasterCard]], [[Brand.Discover]],
    *                           [[Brand.JCB]], [[Brand.`Diners Club`]], or [[Brand.Unknown]]
    * @param country            Two-letter ISO code representing the country of the card.
    *                           You could use this attribute to get a sense of the international
    *                           breakdown of cards you’ve collected.
    * @param currency           Only applicable on accounts (not customers or recipients). The card
    *                           can be used as a transfer destination for funds in this currency.
    * @param customer           The customer that this card belongs to. This attribute will not be in
    *                           the card object if the card belongs to an account or recipient instead.
    * @param cvcCheck           If a CVC was provided, results of the check: [[Check.Pass]], [[Check.Fail]],
    *                           [[Check.Unavailable]], or [[Check.Unchecked]]
    * @param defaultForCurrency Only applicable on accounts (not customers or recipients). This indicates
    *                           whether or not this card is the default external account for its currency.
    * @param dynamicLast4       (For tokenized numbers only.) The last four digits of the device account number.
    * @param expMonth
    * @param expYear
    * @param fingerprint        Uniquely identifies this particular card number.
    *                           You can use this attribute to check whether two
    *                           customers who’ve signed up with you are using the
    *                           same card number, for example.
    * @param funding            Card funding type. Can be [[Funding.Credit]],
    *                           [[Funding.Debit]], [[Funding.Prepaid]], or
    *                           [[Funding.Unknown]]
    * @param last4
    * @param metadata           A set of key/value pairs that you can attach
    *                           to a card object. It can be useful for storing
    *                           additional information about the card in a
    *                           structured format.
    * @param name               Cardholder name
    * @param recipient          The recipient that this card belongs to.
    *                           This attribute will not be in the card object
    *                           if the card belongs to a customer or account instead.
    * @param tokenizationMethod If the card number is tokenized, this is the method
    *                           that was used. Can be [[TokenizationMethod.ApplePay]]
    *                           or [[TokenizationMethod.AndroidPay]].
    */
  case class Card(id: String,
                  account: Option[String],
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
                  currency: Option[Currency],
                  customer: Option[String],
                  cvcCheck: Option[Check],
                  defaultForCurrency: Option[Boolean],
                  dynamicLast4: Option[String],
                  expMonth: Int,
                  expYear: Int,
                  fingerprint: Option[String],
                  funding: Funding,
                  last4: String,
                  metadata: Option[Map[String, String]],
                  name: Option[String],
                  recipient: Option[String],
                  tokenizationMethod: Option[TokenizationMethod])
      extends StripeObject
      with PaymentSource

  object Card {
    def default(id: String, brand: Brand, expMonth: Int, expYear: Int, funding: Funding, last4: String): Card = Card(
      id,
      None,
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
      None,
      None,
      expMonth,
      expYear,
      None,
      funding,
      last4,
      None,
      None,
      None,
      None
    )
  }

  private[this] val cardReadsOne = (
    (__ \ "id").read[String] ~
      (__ \ "account").readNullable[String] ~
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
      (__ \ "currency").readNullable[Currency] ~
      (__ \ "customer").readNullable[String] ~
      (__ \ "cvc_check").readNullable[Check] ~
      (__ \ "default_for_currency").readNullable[Boolean] ~
      (__ \ "dynamic_last4").readNullable[String] ~
      (__ \ "exp_month").read[Int] ~
      (__ \ "exp_year").read[Int] ~
      (__ \ "fingerprint").readNullable[String] ~
      (__ \ "funding").read[Funding] ~
      (__ \ "last4").read[String]
  ).tupled

  private[this] val cardReadsTwo = (
    (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "name").readNullable[String] ~
      (__ \ "recipient").readNullable[String] ~
      (__ \ "tokenization_method").readNullable[TokenizationMethod]
  ).tupled

  implicit val cardReads: Reads[Card] = (
    cardReadsOne ~ cardReadsTwo
  ) { (one, two) =>
    val (id,
         account,
         addressCity,
         addressCountry,
         addressLine1,
         addressLine1Check,
         addressLine2,
         addressState,
         addressZip,
         addressZipCheck,
         brand,
         country,
         currency,
         customer,
         cvcCheck,
         defaultForCurrency,
         dynamicLast4,
         expMonth,
         expYear,
         fingerprint,
         funding,
         last4) = one

    val (metadata, name, recipient, tokenizationMethod) = two

    Card(id,
         account,
         addressCity,
         addressCountry,
         addressLine1,
         addressLine1Check,
         addressLine2,
         addressState,
         addressZip,
         addressZipCheck,
         brand,
         country,
         currency,
         customer,
         cvcCheck,
         defaultForCurrency,
         dynamicLast4,
         expMonth,
         expYear,
         fingerprint,
         funding,
         last4,
         metadata,
         name,
         recipient,
         tokenizationMethod)
  }

  implicit val cardWrites: Writes[Card] = Writes(
    (card: Card) =>
      Json.obj(
        "id"                   -> card.id,
        "object"               -> "card",
        "account"              -> card.account,
        "address_city"         -> card.addressCity,
        "address_country"      -> card.addressCountry,
        "address_line1"        -> card.addressLine1,
        "address_line1_check"  -> card.addressLine1Check,
        "address_line2"        -> card.addressLine2,
        "address_state"        -> card.addressState,
        "address_zip"          -> card.addressZip,
        "address_zip_check"    -> card.addressZipCheck,
        "brand"                -> card.brand,
        "country"              -> card.country,
        "currency"             -> card.currency,
        "customer"             -> card.customer,
        "cvc_check"            -> card.cvcCheck,
        "default_for_currency" -> card.defaultForCurrency,
        "dynamic_last4"        -> card.dynamicLast4,
        "exp_month"            -> card.expMonth,
        "exp_year"             -> card.expYear,
        "fingerprint"          -> card.fingerprint,
        "funding"              -> card.funding,
        "last4"                -> card.last4,
        "metadata"             -> card.metadata,
        "name"                 -> card.name,
        "recipient"            -> card.recipient,
        "tokenization_method"  -> card.tokenizationMethod
    ))

  sealed abstract class CardData

  object CardData {

    sealed abstract class Source extends CardData

    object Source {

      /**
        * @see https://stripe.com/docs/api#create_card
        * @param expMonth Two digit number representing
        *                 the card's expiration month.
        * @param expYear  Two or four digit number representing
        *                 the card's expiration year.
        * @param number   The card number, as a string without any separators.
        * @param addressCity
        * @param addressCountry
        * @param addressLine1
        * @param addressLine2
        * @param addressState
        * @param addressZip
        * @param cvc      Card security code. Required unless your account is
        *                 registered in Australia, Canada, or the United States.
        *                 Highly recommended to always include this value.
        * @param metadata A set of key/value pairs that you can attach to
        *                 a card object. It can be useful for storing
        *                 additional information about the card in a
        *                 structured format.
        * @param name     Cardholder's full name.
        */
      case class Object(expMonth: Int,
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
                        name: Option[String])
          extends Source

      object Object {
        def default(expMonth: Int, expYear: Int, number: String): Object = Object(
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

      implicit val sourceObjectReads: Reads[Object] = (
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
      ).tupled.map((Object.apply _).tupled)

      implicit val sourceObjectWrites: Writes[Object] = Writes(
        (`object`: Object) =>
          Json.obj(
            "object"          -> "card",
            "exp_month"       -> `object`.expMonth,
            "exp_year"        -> `object`.expYear,
            "number"          -> `object`.number,
            "address_city"    -> `object`.addressCity,
            "address_country" -> `object`.addressCountry,
            "address_line1"   -> `object`.addressLine1,
            "address_line2"   -> `object`.addressLine2,
            "address_state"   -> `object`.addressState,
            "address_zip"     -> `object`.addressState,
            "cvc"             -> `object`.cvc,
            "metadata"        -> `object`.metadata,
            "name"            -> `object`.name
        ))

      case class Token(id: String) extends Source

      implicit val sourceTokenReads: Reads[Token] = Reads.of[String].map(Token)

      implicit val sourceTokenWrites: Writes[Token] = Writes((token: Token) => JsString(token.id))
    }

    sealed abstract class ExternalAccount extends CardData

    object ExternalAccount {

      /**
        * @see https://stripe.com/docs/api#create_card
        * @param expMonth           Two digit number representing
        *                           the card's expiration month.
        * @param expYear            Two or four digit number representing
        *                           the card's expiration year.
        * @param number             The card number, as a string
        *                           without any separators.
        * @param addressCity
        * @param addressCountry
        * @param addressLine1
        * @param addressLine2
        * @param addressState
        * @param addressZip
        * @param currency           Required when adding a card to an
        *                           account (not applicable to a customers or recipients).
        *                           The card (which must be a debit card) can be
        *                           used as a transfer destination for funds in this currency.
        *                           Currently, the only supported currency for debit card
        *                           transfers is [[Currency.`United States Dollar`]].
        * @param cvc                Card security code. Required unless your account is
        *                           registered in Australia, Canada, or the United States.
        *                           Highly recommended to always include this value.
        * @param defaultForCurrency Only applicable on accounts
        *                           (not customers or recipients). If you set this to true
        *                           (or if this is the first external account being
        *                           added in this currency) this card will become the
        *                           default external account for its currency.
        * @param metadata           A set of key/value pairs that you can attach to a card object.
        *                           It can be useful for storing additional information about the
        *                           card in a structured format.
        * @param name               Cardholder's full name.
        */
      case class Object(expMonth: Int,
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
                        name: Option[String])
          extends ExternalAccount

      object Object {
        def default(expMonth: Int, expYear: Int, number: String): Object = Object(
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

      implicit val externalAccountObjectReads: Reads[Object] = (
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
      ).tupled.map((Object.apply _).tupled)

      implicit val externalAccountObjectWrites: Writes[Object] = Writes(
        (`object`: Object) =>
          Json.obj(
            "object"               -> "card",
            "exp_month"            -> `object`.expMonth,
            "exp_year"             -> `object`.expYear,
            "number"               -> `object`.number,
            "address_city"         -> `object`.addressCity,
            "address_country"      -> `object`.addressCountry,
            "address_line1"        -> `object`.addressLine1,
            "address_line2"        -> `object`.addressLine2,
            "address_state"        -> `object`.addressState,
            "address_zip"          -> `object`.addressState,
            "currency"             -> `object`.currency,
            "cvc"                  -> `object`.cvc,
            "default_for_currency" -> `object`.defaultForCurrency,
            "metadata"             -> `object`.metadata,
            "name"                 -> `object`.name
        ))

      case class Token(id: String) extends ExternalAccount

      implicit val externalAccountTokenReads: Reads[Token] =
        Reads.of[String].map(Token)

      implicit val externalAccountTokenWrites: Writes[Token] = Writes((token: Token) => JsString(token.id))
    }
  }

  implicit val cardDataWrites: Writes[CardData] = Writes { (cardData: CardData) =>
    cardData match {
      case cardData: CardData.Source.Object          => Json.toJson(cardData)
      case cardData: CardData.ExternalAccount.Object => Json.toJson(cardData)
      case cardData: CardData.Source.Token           => Json.toJson(cardData)
      case cardData: CardData.ExternalAccount.Token  => Json.toJson(cardData)
    }
  }

  /**
    * @see https://stripe.com/docs/api#create_card
    * @param cardData           When adding a card to a customer,
    *                           the parameter name is [[CardData.Source]]. When adding to an account,
    *                           the parameter name is [[CardData.ExternalAccount]]. The value can either
    *                           be a token, like the ones returned by our Stripe.js,
    *                           or a dictionary containing a user’s credit card details
    *                           (with the options shown below). Stripe will automatically
    *                           validate the card.
    * @param metadata           A set of key/value pairs that you can attach to a card
    *                           object. It can be useful for storing additional
    *                           information about the card in a structured format.
    * @param defaultForCurrency Only applicable on accounts (not customers or recipients). If you set this to true (or if this is the first external account being added in this currency) this card will become the default external account for its currency.
    */
  case class CardInput(cardData: CardData, metadata: Option[Map[String, String]], defaultForCurrency: Option[Boolean])

  object CardInput {
    def default(cardData: CardData): CardInput = CardInput(
      cardData,
      None,
      None
    )
  }

  implicit val cardInputWrites: Writes[CardInput] = Writes { (cardInput: CardInput) =>
    val cardData = cardInput.cardData match {
      case cardData: CardData.ExternalAccount.Token =>
        Json.obj("external_account" -> cardData)
      case cardData: CardData.ExternalAccount.Object =>
        Json.obj("external_account" -> cardData)
      case cardData: CardData.Source.Object =>
        Json.obj("source" -> cardData)
      case cardData: CardData.Source.Token =>
        Json.obj("source" -> cardData)
    }

    cardData ++ Json.obj(
      "metadata"             -> cardInput.metadata,
      "default_for_currency" -> cardInput.defaultForCurrency
    )
  }

  implicit val cardInputReads: Reads[CardInput] = {
    val cardData = (__ \ "external_account").read[JsValue].flatMap {
      case JsObject(_) =>
        (__ \ "external_account").read[CardData.ExternalAccount.Object].map(x => x: CardData)
      case JsString(_) =>
        (__ \ "external_account").read[CardData.ExternalAccount.Token].map(x => x: CardData)
      case _ =>
        (__ \ "source").read[JsValue].flatMap {
          case JsObject(_) =>
            (__ \ "source").read[CardData.Source.Object].map(x => x: CardData)
          case JsString(_) =>
            (__ \ "source").read[CardData.Source.Token].map(x => x: CardData)
          case _ =>
            Reads[CardData](_ => JsError(ValidationError("UnknownCardData")))
        }
    }

    (cardData ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "default_for_currency").readNullable[Boolean]).tupled.map((CardInput.apply _).tupled)
  }

  def create(customerId: String, cardInput: CardInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[Card]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "default_for_currency" -> cardInput.defaultForCurrency.map(_.toString)
      )
    }.collect {
      case (k, Some(v)) => (k, v)
    } ++ {
      cardInput.cardData match {
        case CardData.ExternalAccount.Token(id) =>
          Map("external_account" -> id)
        case CardData.Source.Token(id) =>
          Map("source" -> id)
        case externalAccount: CardData.ExternalAccount.Object =>
          val map = Map(
            "object"               -> Option("card"),
            "exp_month"            -> Option(externalAccount.expMonth.toString),
            "exp_year"             -> Option(externalAccount.expYear.toString),
            "number"               -> Option(externalAccount.number),
            "address_city"         -> externalAccount.addressCity,
            "address_country"      -> externalAccount.addressCountry,
            "address_line1"        -> externalAccount.addressLine1,
            "address_line2"        -> externalAccount.addressLine2,
            "address_state"        -> externalAccount.addressState,
            "address_zip"          -> externalAccount.addressState,
            "currency"             -> externalAccount.currency.map(_.iso.toLowerCase),
            "cvc"                  -> externalAccount.cvc,
            "default_for_currency" -> externalAccount.defaultForCurrency.map(_.iso.toLowerCase),
            "name"                 -> externalAccount.name
          ).collect {
            case (k, Some(v)) => (k, v)
          }
          mapToPostParams(Option(map), "external_account")
        case source: CardData.Source.Object =>
          val map = Map(
            "object"          -> Option("card"),
            "exp_month"       -> Option(source.expMonth.toString),
            "exp_year"        -> Option(source.expYear.toString),
            "number"          -> Option(source.number),
            "address_city"    -> source.addressCity,
            "address_country" -> source.addressCountry,
            "address_line1"   -> source.addressLine1,
            "address_line2"   -> source.addressLine2,
            "address_state"   -> source.addressState,
            "address_zip"     -> source.addressState,
            "cvc"             -> source.cvc,
            "name"            -> source.name
          ).collect {
            case (k, Some(v)) => (k, v)
          }
          mapToPostParams(Option(map), "source")
      }
    } ++ mapToPostParams(cardInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources"

    createRequestPOST[Card](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(customerId: String, cardId: String)(implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[Card]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources/$cardId"

    createRequestGET[Card](finalUrl, logger)
  }

  def delete(customerId: String, cardId: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[DeleteResponse]] = {

    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources/$cardId"

    createRequestDELETE(finalUrl, idempotencyKey, logger)
  }

  /**
    * @see https://stripe.com/docs/api#list_cards
    * @param endingBefore  A cursor for use in pagination. [[endingBefore]] is an
    *                      object ID that defines your place in the list.
    *                      For instance, if you make a list request and
    *                      receive 100 objects, starting with obj_bar,
    *                      your subsequent call can include [[endingBefore]]=obj_bar
    *                      in order to fetch the previous page of the list.
    * @param limit         A limit on the number of objects to be returned. Limit can range between 1 and 100 items.
    * @param startingAfter A cursor for use in pagination. [[startingAfter]] is an
    *                      object ID that defines your place in the list. For instance,
    *                      if you make a list request and receive 100 objects,
    *                      ending with obj_foo, your subsequent call can include [[startingAfter]]=obj_foo
    *                      in order to fetch the next page of the list.
    */
  case class CardListInput(endingBefore: Option[String], limit: Option[Long], startingAfter: Option[String])

  object CardListInput {
    def default: CardListInput = CardListInput(
      None,
      None,
      None
    )
  }

  case class CardList(override val url: String,
                      override val hasMore: Boolean,
                      override val data: List[Card],
                      override val totalCount: Option[Long])
      extends Collections.List[Card](url, hasMore, data, totalCount)

  object CardList extends Collections.ListJsonMappers[Card] {
    implicit val cardListReads: Reads[CardList] =
      listReads.tupled.map((CardList.apply _).tupled)

    implicit val cardListWrites: Writes[CardList] = listWrites
  }

  def list(customerId: String, cardListInput: CardListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[CardList]] = {
    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl =
        endpoint.url + s"/v1/customers/$customerId/sources$totalCountUrl"

      (baseUrl ?
        ("object"         -> "card") ?
        ("ending_before"  -> cardListInput.endingBefore) ?
        ("limit"          -> cardListInput.limit.map(_.toString)) ?
        ("starting_after" -> cardListInput.startingAfter)).toString()
    }

    createRequestGET[CardList](finalUrl, logger)
  }
}

object BankAccountsPaymentSource extends LazyLogging {
  case class BankAccount(
      id: String,
      accountHolderName: Option[String],
      last4: String,
      routingNumber: String,
      country: String,
      defaultForCurrency: Boolean
  ) extends StripeObject
      with PaymentSource

  implicit val bankAccountReads: Reads[BankAccount] = (
    (__ \ "id").read[String] ~
      (__ \ "account_holder_name").readNullable[String] ~
      (__ \ "last4").read[String] ~
      (__ \ "routing_number").read[String] ~
      (__ \ "country").read[String] ~
      (__ \ "default_for_currency").read[Boolean]
  ).tupled.map((BankAccount.apply _).tupled)
  implicit val bankAccountWrites: Writes[BankAccount] = Json.writes[BankAccount]
}

object BitcoinReceivers extends LazyLogging {

  /**
    * @see https://stripe.com/docs/api#bitcoin_receiver_object-transactions-data
    * @param id
    * @param amount        The amount of [[currency]] that the transaction
    *                      was converted to in real-time.
    * @param bitcoinAmount The amount of bitcoin contained
    *                      in the transaction.
    * @param created
    * @param currency      The currency to which this
    *                      transaction was converted.
    * @param receiver      The receiver to which this
    *                      transaction was sent.
    */
  case class Transaction(id: String,
                         amount: BigDecimal,
                         bitcoinAmount: BigDecimal,
                         created: OffsetDateTime,
                         currency: Currency,
                         receiver: String)

  implicit val transactionReads: Reads[Transaction] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "bitcoin_amount").read[BigDecimal] ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "receiver").read[String]
  ).tupled.map((Transaction.apply _).tupled)

  implicit val transactionWrites: Writes[Transaction] = Writes(
    (transaction: Transaction) =>
      Json.obj(
        "id"             -> transaction.id,
        "object"         -> "list",
        "amount"         -> transaction.amount,
        "bitcoin_amount" -> transaction.bitcoinAmount,
        "created"        -> Json.toJson(transaction.created)(stripeDateTimeWrites),
        "currency"       -> transaction.currency,
        "receiver"       -> transaction.receiver
    ))

  case class TransactionList(override val url: String,
                             override val hasMore: Boolean,
                             override val data: List[Transaction],
                             override val totalCount: Option[Long])
      extends Collections.List[Transaction](
        url,
        hasMore,
        data,
        totalCount
      )

  object TransactionList extends Collections.ListJsonMappers[Transaction] {
    implicit val transactionsReads: Reads[TransactionList] =
      listReads.tupled.map((TransactionList.apply _).tupled)

    implicit val transactionsWrites: Writes[TransactionList] = listWrites
  }

  /**
    * @see https://stripe.com/docs/api#bitcoin_receiver_object
    * @param id
    * @param active                True when this bitcoin receiver has
    *                              received a non-zero amount of bitcoin.
    * @param amount                The amount of [[currency]] that you are
    *                              collecting as payment.
    * @param amountReceived        The amount of [[currency]] to
    *                              which [[bitcoinAmountReceived]] has been converted.
    * @param bitcoinAmount         The amount of bitcoin that the customer should send
    *                              to fill the receiver. The [[bitcoinAmount]] is
    *                              denominated in Satoshi: there are `10^8` Satoshi
    *                              in one bitcoin.
    * @param bitcoinAmountReceived The amount of bitcoin that has been
    *                              sent by the customer to this receiver.
    * @param bitcoinUri            This URI can be displayed to the customer as a
    *                              clickable link (to activate their bitcoin client)
    *                              or as a QR code (for mobile wallets).
    * @param created
    * @param currency              Three-letter ISO currency code representing the
    *                              currency to which the bitcoin will be converted.
    * @param customer
    * @param description
    * @param email                 The customer’s email address,
    *                              set by the API call that creates the receiver.
    * @param filled                This flag is initially false and updates to true when
    *                              the customer sends the [[bitcoinAmount]] to this receiver.
    * @param inboundAddress        A bitcoin address that is specific to this receiver.
    *                              The customer can send bitcoin to this address to
    *                              fill the receiver.
    * @param livemode
    * @param metadata              A set of key/value pairs that you can attach to a
    *                              customer object. It can be useful for storing additional
    *                              information about the customer in a structured format.
    * @param payment               The ID of the payment created from the receiver, if any.
    *                              Hidden when viewing the receiver with a publishable key.
    * @param refundAddress         The refund address for these bitcoin, if
    *                              communicated by the customer.
    * @param transactions          A list with one entry for each time that the
    *                              customer sent bitcoin to the receiver. Hidden when
    *                              viewing the receiver with a publishable key.
    * @param uncapturedFunds       This receiver contains uncaptured funds that
    *                              can be used for a payment or refunded.
    * @param usedForPayment
    */
  case class BitcoinReceiver(id: String,
                             active: Boolean,
                             amount: BigDecimal,
                             amountReceived: BigDecimal,
                             bitcoinAmount: BigDecimal,
                             bitcoinAmountReceived: BigDecimal,
                             bitcoinUri: String,
                             created: OffsetDateTime,
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
                             usedForPayment: Boolean)
      extends StripeObject
      with PaymentSource

  object BitcoinReceiver {
    def default(id: String,
                active: Boolean,
                amount: BigDecimal,
                amountReceived: BigDecimal,
                bitcoinAmount: BigDecimal,
                bitcoinAmountReceived: BigDecimal,
                bitcoinUri: String,
                created: OffsetDateTime,
                currency: Currency,
                customer: String,
                description: String,
                email: String,
                filled: Boolean,
                inboundAddress: String,
                livemode: Boolean,
                uncapturedFunds: Boolean,
                usedForPayment: Boolean): BitcoinReceiver = BitcoinReceiver(
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
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
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

  implicit val bitcoinReceiverWrites: Writes[BitcoinReceiver] = Writes(
    (bitcoinReceiver: BitcoinReceiver) =>
      Json.obj(
        "id"                      -> bitcoinReceiver.id,
        "object"                  -> "bitcoin_receiver",
        "active"                  -> bitcoinReceiver.active,
        "amount"                  -> bitcoinReceiver.amount,
        "amount_received"         -> bitcoinReceiver.amountReceived,
        "bitcoin_amount"          -> bitcoinReceiver.bitcoinAmount,
        "bitcoin_amount_received" -> bitcoinReceiver.bitcoinAmountReceived,
        "bitcoin_uri"             -> bitcoinReceiver.bitcoinUri,
        "created"                 -> Json.toJson(bitcoinReceiver.created)(stripeDateTimeWrites),
        "currency"                -> bitcoinReceiver.currency,
        "customer"                -> bitcoinReceiver.customer,
        "description"             -> bitcoinReceiver.description,
        "email"                   -> bitcoinReceiver.email,
        "filled"                  -> bitcoinReceiver.filled,
        "inbound_address"         -> bitcoinReceiver.inboundAddress,
        "livemode"                -> bitcoinReceiver.livemode,
        "metadata"                -> bitcoinReceiver.metadata,
        "payment"                 -> bitcoinReceiver.payment,
        "refund_address"          -> bitcoinReceiver.refundAddress,
        "transactions"            -> bitcoinReceiver.transactions,
        "uncaptured_funds"        -> bitcoinReceiver.uncapturedFunds,
        "used_for_payment"        -> bitcoinReceiver.usedForPayment
    ))

  /**
    * @see https://stripe.com/docs/api#create_bitcoin_receiver
    * @param amount            The amount of [[currency]] that you will be paid.
    * @param currency          The currency to which the bitcoin will be converted.
    *                          You will be paid out in this currency.
    *                          Only [[Currency.`United States Dollar`]] is
    *                          currently supported.
    * @param email             The email address of the customer.
    * @param description
    * @param metadata          A set of key/value pairs that you can attach
    *                          to a customer object. It can be useful for
    *                          storing additional information about the
    *                          customer in a structured format. This will
    *                          be unset if you POST an empty value.
    * @param refundMispayments A flag that indicates whether you
    *                          would like Stripe to automatically handle
    *                          refunds for any mispayments to the
    *                          receiver.
    */
  case class BitcoinReceiverInput(amount: BigDecimal,
                                  currency: Currency,
                                  email: String,
                                  description: Option[String],
                                  metadata: Option[Map[String, String]],
                                  refundMispayments: Option[Boolean])

  object BitcoinReceiverInput {
    def default(amount: BigDecimal, currency: Currency, email: String): BitcoinReceiverInput = BitcoinReceiverInput(
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
    Writes(
      (bitcoinReceiverInput: BitcoinReceiverInput) =>
        Json.obj(
          "amount"             -> bitcoinReceiverInput.amount,
          "currency"           -> bitcoinReceiverInput.currency,
          "email"              -> bitcoinReceiverInput.email,
          "description"        -> bitcoinReceiverInput.description,
          "metadata"           -> bitcoinReceiverInput.metadata,
          "refund_mispayments" -> bitcoinReceiverInput.refundMispayments
      ))

  def create(bitcoinReceiverInput: BitcoinReceiverInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[BitcoinReceiver]] = {

    val postFormParameters: Map[String, String] = {
      Map(
        "amount"             -> Option(bitcoinReceiverInput.amount.toString()),
        "currency"           -> Option(bitcoinReceiverInput.currency.iso.toLowerCase()),
        "email"              -> Option(bitcoinReceiverInput.email),
        "description"        -> bitcoinReceiverInput.description,
        "refund_mispayments" -> Option(bitcoinReceiverInput.refundMispayments.toString)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(bitcoinReceiverInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/bitcoin/receivers"

    createRequestPOST[BitcoinReceiver](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(id: String)(implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[BitcoinReceiver]] = {
    val finalUrl = endpoint.url + s"/v1/bitcoin/receivers/$id"

    createRequestGET[BitcoinReceiver](finalUrl, logger)
  }

  /**
    * @see https://stripe.com/docs/api#list_bitcoin_receivers
    * @param active          Filter for active receivers.
    * @param endingBefore    A cursor for use in pagination. [[endingBefore]] is an
    *                        object ID that defines your place in the list.
    *                        For instance, if you make a list request and
    *                        receive 100 objects, starting with obj_bar, your
    *                        subsequent call can include [[endingBefore]]=obj_bar in
    *                        order to fetch the previous page of the list.
    * @param filled          Filter for filled receivers.
    * @param limit           A limit on the number of objects to be returned.
    *                        Limit can range between 1 and 100 items.
    * @param startingAfter   A cursor for use in pagination. [[startingAfter]] is an
    *                        object ID that defines your place in the list. For instance,
    *                        if you make a list request and receive 100 objects,
    *                        ending with obj_foo, your subsequent call can include
    *                        [[startingAfter]]=obj_foo in order to fetch the
    *                        next page of the list.
    * @param uncapturedFunds Filter for receivers with uncaptured funds.
    */
  case class BitcoinReceiverListInput(active: Option[Boolean],
                                      endingBefore: Option[String],
                                      filled: Option[Boolean],
                                      limit: Option[Long],
                                      startingAfter: Option[String],
                                      uncapturedFunds: Option[Boolean])

  object BitcoinReceiverListInput {
    def default: BitcoinReceiverListInput = BitcoinReceiverListInput(
      None,
      None,
      None,
      None,
      None,
      None
    )
  }

  case class BitcoinReceiverList(override val url: String,
                                 override val hasMore: Boolean,
                                 override val data: List[BitcoinReceiver],
                                 override val totalCount: Option[Long])
      extends Collections.List[BitcoinReceiver](url, hasMore, data, totalCount)

  object BitcoinReceiverList extends Collections.ListJsonMappers[BitcoinReceiver] {
    implicit val cardListReads: Reads[BitcoinReceiverList] =
      listReads.tupled.map((BitcoinReceiverList.apply _).tupled)

    implicit val cardListWrites: Writes[BitcoinReceiverList] = listWrites
  }

  def list(bitcoinReceiverListInput: BitcoinReceiverListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[BitcoinReceiverList]] = {
    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/bitcoin/receivers$totalCountUrl"

      (baseUrl ?
        ("active"           -> bitcoinReceiverListInput.active) ?
        ("ending_before"    -> bitcoinReceiverListInput.endingBefore) ?
        ("filled"           -> bitcoinReceiverListInput.filled) ?
        ("limit"            -> bitcoinReceiverListInput.limit.map(_.toString)) ?
        ("starting_after"   -> bitcoinReceiverListInput.startingAfter) ?
        ("uncaptured_funds" -> bitcoinReceiverListInput.uncapturedFunds)).toString()
    }

    createRequestGET[BitcoinReceiverList](finalUrl, logger)
  }
}
