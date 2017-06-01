package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Query
import akka.stream.Materializer
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import defaults._
import enumeratum._
import io.circe._
import io.circe.syntax._
import org.mdedetrich.stripe.v1.BankAccountsPaymentSource.BankAccount
import org.mdedetrich.stripe.v1.BitcoinReceivers.BitcoinReceiver
import org.mdedetrich.stripe.v1.Cards.Card
import org.mdedetrich.stripe.v1.Collections.ListJsonMappers
import org.mdedetrich.stripe.v1.defaults._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * [[PaymentSource]] is a supertype of the different available Stripe
  * payment types.
  */
sealed trait PaymentSource

object PaymentSource extends LazyLogging {

  implicit val paymentSourceDecoder: Decoder[PaymentSource] = Decoder.instance[PaymentSource] { c =>
    for {
      tpe <- c.downField("object").as[String]
      paymentSource <- tpe match {
        case "card"             => c.as[Card].map(x => x: PaymentSource)
        case "bank_account"     => c.as[BankAccount].map(x => x: PaymentSource)
        case "bitcoin_receiver" => c.as[BitcoinReceiver].map(x => x: PaymentSource)
        case _                  => Left(DecodingFailure("Unknown Payment Source", c.history))
      }
    } yield paymentSource
  }

  implicit val paymentSourceEncoder: Encoder[PaymentSource] = Encoder.instance[PaymentSource] {
    case c: Card            => implicitly[Encoder[Card]].apply(c)
    case b: BitcoinReceiver => implicitly[Encoder[BitcoinReceiver]].apply(b)
    case ba: BankAccount    => implicitly[Encoder[BankAccount]].apply(ba)
  }
}

case class PaymentSourceList(override val url: String,
                             override val hasMore: Boolean,
                             override val data: List[PaymentSource],
                             override val totalCount: Option[Long])
    extends Collections.List[PaymentSource](
      url,
      hasMore,
      data,
      totalCount
    )

object PaymentSourceList extends ListJsonMappers[PaymentSource] {
  implicit val paymentSourceListDecoder: Decoder[PaymentSourceList] =
    listDecoder(implicitly)(PaymentSourceList.apply)

  implicit val paymentSourceListEncoder: Encoder[PaymentSourceList] =
    listEncoder[PaymentSourceList]
}

object Cards extends LazyLogging {

  sealed abstract class Brand(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Brand extends Enum[Brand] {
    val values = findValues

    case object Visa               extends Brand("Visa")
    case object `American Express` extends Brand("American Express")
    case object MasterCard         extends Brand("MasterCard")
    case object Discover           extends Brand("Discover")
    case object JCB                extends Brand("JCB")
    case object `Diners Club`      extends Brand("Diners Club")
    case object Unknown            extends Brand("Unknown")

    implicit val cardsBrandDecoder: Decoder[Brand] = enumeratum.Circe.decoder(Brand)
    implicit val cardsBrandEncoder: Encoder[Brand] = enumeratum.Circe.encoder(Brand)
  }

  sealed abstract class Check(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Check extends Enum[Check] {
    val values = findValues

    case object Pass        extends Check("pass")
    case object Fail        extends Check("fail")
    case object Unavailable extends Check("unavailable")
    case object Unchecked   extends Check("unchecked")

    implicit val cardsCheckDecoder: Decoder[Check] = enumeratum.Circe.decoder(Check)
    implicit val cardsCheckEncoder: Encoder[Check] = enumeratum.Circe.encoder(Check)
  }

  sealed abstract class Funding(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Funding extends Enum[Funding] {
    val values = findValues

    case object Credit  extends Funding("credit")
    case object Debit   extends Funding("debit")
    case object Prepaid extends Funding("prepaid")
    case object Unknown extends Funding("unknown")

    implicit val cardsFundingDecoder: Decoder[Funding] = enumeratum.Circe.decoder(Funding)
    implicit val cardsFundingEncoder: Encoder[Funding] = enumeratum.Circe.encoder(Funding)
  }

  sealed abstract class TokenizationMethod(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object TokenizationMethod extends Enum[TokenizationMethod] {
    val values = findValues

    case object ApplePay   extends TokenizationMethod("apple_pay")
    case object AndroidPay extends TokenizationMethod("android_pay")

    implicit val cardsTokenizationMethodDecoder: Decoder[TokenizationMethod] =
      enumeratum.Circe.decoder(TokenizationMethod)

    implicit val cardsTokenizationMethodEncoder: Encoder[TokenizationMethod] =
      enumeratum.Circe.encoder(TokenizationMethod)
  }

  /**
    * @see https://stripe.com/docs/api#card_object
    * @param id                 ID of card (used in conjunction with a customer or recipient ID)
    * @param brand              Card brand. Can be [[Brand.Visa]], [[Brand.`American Express`]],
    *                           [[Brand.MasterCard]], [[Brand.Discover]],
    *                           [[Brand.JCB]], [[Brand.`Diners Club`]], or [[Brand.Unknown]]
    * @param expMonth
    * @param expYear
    * @param funding            Card funding type. Can be [[Funding.Credit]],
    *                           [[Funding.Debit]], [[Funding.Prepaid]], or
    *                           [[Funding.Unknown]]
    * @param last4
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
    * @param fingerprint        Uniquely identifies this particular card number.
    *                           You can use this attribute to check whether two
    *                           customers who’ve signed up with you are using the
    *                           same card number, for example.
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
                  brand: Brand,
                  expMonth: Int,
                  expYear: Int,
                  funding: Funding,
                  last4: String,
                  account: Option[String] = None,
                  addressCity: Option[String] = None,
                  addressCountry: Option[String] = None,
                  addressLine1: Option[String] = None,
                  addressLine1Check: Option[Check] = None,
                  addressLine2: Option[String] = None,
                  addressState: Option[String] = None,
                  addressZip: Option[String] = None,
                  addressZipCheck: Option[Check] = None,
                  country: Option[String] = None,
                  currency: Option[Currency] = None,
                  customer: Option[String] = None,
                  cvcCheck: Option[Check] = None,
                  defaultForCurrency: Option[Boolean] = None,
                  dynamicLast4: Option[String] = None,
                  fingerprint: Option[String] = None,
                  metadata: Option[Map[String, String]] = None,
                  name: Option[String] = None,
                  recipient: Option[String] = None,
                  tokenizationMethod: Option[TokenizationMethod] = None)
      extends StripeObject
      with PaymentSource

  private val cardDecoderOne = Decoder.forProduct22(
    "id",
    "brand",
    "exp_month",
    "exp_year",
    "funding",
    "last4",
    "account",
    "address_city",
    "address_country",
    "address_line1",
    "address_line1_check",
    "address_line2",
    "address_state",
    "address_zip",
    "address_zip_check",
    "country",
    "currency",
    "customer",
    "cvc_check",
    "default_for_currency",
    "dynamic_last4",
    "fingerprint"
  )(
    Tuple22.apply(
      _: String,
      _: Brand,
      _: Int,
      _: Int,
      _: Funding,
      _: String,
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[Check],
      _: Option[String],
      _: Option[String],
      _: Option[String],
      _: Option[Check],
      _: Option[String],
      _: Option[Currency],
      _: Option[String],
      _: Option[Check],
      _: Option[Boolean],
      _: Option[String],
      _: Option[String]
    ))

  private val cardDecoderTwo = Decoder.forProduct4(
    "metadata",
    "name",
    "recipient",
    "tokenization_method"
  )(
    Tuple4.apply(
      _: Option[Map[String, String]],
      _: Option[String],
      _: Option[String],
      _: Option[TokenizationMethod]
    ))

  implicit val cardDecoder: Decoder[Card] = Decoder.instance[Card] { c =>
    for {
      one <- cardDecoderOne(c)
      two <- cardDecoderTwo(c)
    } yield {
      val (id,
           brand,
           expMonth,
           expYear,
           funding,
           last4,
           account,
           addressCity,
           addressCountry,
           addressLine1,
           addressLine1Check,
           addressLine2,
           addressState,
           addressZip,
           addressZipCheck,
           country,
           currency,
           customer,
           cvcCheck,
           defaultForCurrency,
           dynamicLast4,
           fingerprint)                                   = one
      val (metadata, name, recipient, tokenizationMethod) = two
      Card(
        id,
        brand,
        expMonth,
        expYear,
        funding,
        last4,
        account,
        addressCity,
        addressCountry,
        addressLine1,
        addressLine1Check,
        addressLine2,
        addressState,
        addressZip,
        addressZipCheck,
        country,
        currency,
        customer,
        cvcCheck,
        defaultForCurrency,
        dynamicLast4,
        fingerprint,
        metadata,
        name,
        recipient,
        tokenizationMethod
      )
    }
  }

  private val cardEncoderOne: Encoder[Card] = Encoder.forProduct22(
    "id",
    "brand",
    "exp_month",
    "exp_year",
    "funding",
    "last4",
    "account",
    "address_city",
    "address_country",
    "address_line1",
    "address_line1_check",
    "address_line2",
    "address_state",
    "address_zip",
    "address_zip_check",
    "country",
    "currency",
    "customer",
    "cvc_check",
    "default_for_currency",
    "dynamic_last4",
    "fingerprint"
  )(
    x =>
      (x.id,
       "card",
       x.account,
       x.addressCity,
       x.addressCountry,
       x.addressLine1,
       x.addressLine1Check,
       x.addressLine2,
       x.addressState,
       x.addressZip,
       x.addressZipCheck,
       x.brand,
       x.country,
       x.currency,
       x.customer,
       x.cvcCheck,
       x.defaultForCurrency,
       x.dynamicLast4,
       x.expMonth,
       x.expYear,
       x.fingerprint,
       x.funding))

  private val cardEncoderTwo: Encoder[Card] = Encoder.forProduct5(
    "last4",
    "metadata",
    "name",
    "recipient",
    "tokenization_method"
  )(
    x =>
      (
        x.last4,
        x.metadata,
        x.name,
        x.recipient,
        x.tokenizationMethod
    ))

  implicit val cardEncoder: Encoder[Card] = Encoder.instance[Card](e => cardEncoderOne(e).deepMerge(cardEncoderTwo(e)))

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
                        addressCity: Option[String] = None,
                        addressCountry: Option[String] = None,
                        addressLine1: Option[String] = None,
                        addressLine2: Option[String] = None,
                        addressState: Option[String] = None,
                        addressZip: Option[String] = None,
                        cvc: Option[String] = None,
                        metadata: Option[Map[String, String]] = None,
                        name: Option[String] = None)
          extends Source

      implicit val sourceObjectDecoder: Decoder[Object] = Decoder.forProduct12(
        "exp_month",
        "exp_year",
        "number",
        "address_city",
        "address_country",
        "address_line1",
        "address_line2",
        "address_state",
        "address_zip",
        "cvc",
        "metadata",
        "name"
      )(Object.apply)

      implicit val sourceObjectEncoder: Encoder[Object] = Encoder.forProduct13(
        "object",
        "exp_month",
        "exp_year",
        "number",
        "address_city",
        "address_country",
        "address_line1",
        "address_line2",
        "address_state",
        "address_zip",
        "cvc",
        "metadata",
        "name"
      )(
        x =>
          (
            "card",
            x.expMonth,
            x.expYear,
            x.number,
            x.addressCity,
            x.addressCountry,
            x.addressLine1,
            x.addressLine2,
            x.addressState,
            x.addressZip,
            x.cvc,
            x.metadata,
            x.name
        ))

      case class Token(id: String) extends Source

      implicit val sourceTokenDecoder: Decoder[Token] = Decoder[String].map(Token)
      implicit val sourceTokenEncoder: Encoder[Token] = Encoder.instance[Token](_.id.asJson)
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
                        addressCity: Option[String] = None,
                        addressCountry: Option[String] = None,
                        addressLine1: Option[String] = None,
                        addressLine2: Option[String] = None,
                        addressState: Option[String] = None,
                        addressZip: Option[String] = None,
                        currency: Option[Currency] = None,
                        cvc: Option[String] = None,
                        defaultForCurrency: Option[Currency] = None,
                        metadata: Option[Map[String, String]] = None,
                        name: Option[String] = None)
          extends ExternalAccount

      implicit val externalAccountObjectDecoder: Decoder[Object] =
        Decoder.forProduct14(
          "exp_month",
          "exp_year",
          "number",
          "address_city",
          "address_country",
          "address_line1",
          "address_line2",
          "address_state",
          "address_zip",
          "currency",
          "cvc",
          "default_for_currency",
          "metadata",
          "name"
        )(Object.apply)

      implicit val externalAccountObjectEncoder: Encoder[Object] = Encoder.forProduct15(
        "object",
        "exp_month",
        "exp_year",
        "number",
        "address_city",
        "address_country",
        "address_line1",
        "address_line2",
        "address_state",
        "address_zip",
        "currency",
        "cvc",
        "default_for_currency",
        "metadata",
        "name"
      )(
        x =>
          (
            "card",
            x.expMonth,
            x.expYear,
            x.number,
            x.addressCity,
            x.addressCountry,
            x.addressLine1,
            x.addressLine2,
            x.addressState,
            x.addressZip,
            x.currency,
            x.cvc,
            x.defaultForCurrency,
            x.metadata,
            x.name
        ))

      case class Token(id: String) extends ExternalAccount

      implicit val externalAccountTokenDecoder: Decoder[Token] = Decoder[String].map(Token)
      implicit val externalAccountTokenEncoder: Encoder[Token] = Encoder.instance[Token](_.id.asJson)
    }
  }

  implicit val cardDataDecoder: Encoder[CardData] = Encoder.instance[CardData] {
    case cardData: CardData.Source.Object => implicitly[Encoder[CardData.Source.Object]].apply(cardData)
    case cardData: CardData.ExternalAccount.Object =>
      implicitly[Encoder[CardData.ExternalAccount.Object]].apply(cardData)
    case cardData: CardData.Source.Token => implicitly[Encoder[CardData.Source.Token]].apply(cardData)
    case cardData: CardData.ExternalAccount.Token =>
      implicitly[Encoder[CardData.ExternalAccount.Token]].apply(cardData)
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
  case class CardInput(cardData: CardData,
                       metadata: Option[Map[String, String]] = None,
                       defaultForCurrency: Option[Boolean] = None)

  implicit val cardInputEncoder: Encoder[CardInput] = Encoder.instance[CardInput] { cardInput =>
    val cardData = cardInput.cardData match {
      case cardData: CardData.ExternalAccount.Token =>
        Json.obj("external_account" -> cardData.asJson)
      case cardData: CardData.ExternalAccount.Object =>
        Json.obj("external_account" -> cardData.asJson)
      case cardData: CardData.Source.Object =>
        Json.obj("source" -> cardData.asJson)
      case cardData: CardData.Source.Token =>
        Json.obj("source" -> cardData.asJson)
    }

    cardData.deepMerge(
      Json.obj(
        "metadata"             -> cardInput.metadata.asJson,
        "default_for_currency" -> cardInput.defaultForCurrency.asJson
      ))
  }

  implicit val cardInputDecoder: Decoder[CardInput] = Decoder.instance[CardInput] { c =>
    val baseDecoder = Decoder.forProduct2(
      "metadata",
      "default_for_currency"
    )(Tuple2.apply(_: Option[Map[String, String]], _: Option[Boolean]))

    for {
      base  <- baseDecoder.apply(c)
      field <- c.downField("external_account").as[Json]
      cardData <- {
        if (field.isObject) {
          c.downField("external_account").as[CardData.ExternalAccount.Object].map(x => x: CardData)
        } else if (field.isString) {
          c.downField("external_account").as[CardData.ExternalAccount.Token].map(x => x: CardData)
        } else {
          for {
            fieldInner <- c.downField("source").as[Json]
            source <- {
              if (fieldInner.isObject) {
                c.downField("source").as[CardData.Source.Object].map(x => x: CardData)
              } else if (field.isString) {
                c.downField("source").as[CardData.Source.Token].map(x => x: CardData)
              } else {
                Left(DecodingFailure("Unknown Card Data", c.history))
              }
            }
          } yield source

        }
      }
    } yield
      CardInput(
        cardData,
        base._1,
        base._2
      )
  }

  def create(customerId: String, cardInput: CardInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[Card]] = {
    val postFormParameters = PostParams.flatten(
      Map(
        "default_for_currency" -> cardInput.defaultForCurrency.map(_.toString)
      )) ++ {
      cardInput.cardData match {
        case CardData.ExternalAccount.Token(id) =>
          Map("external_account" -> id)
        case CardData.Source.Token(id) =>
          Map("source" -> id)
        case externalAccount: CardData.ExternalAccount.Object =>
          val map = PostParams.flatten(
            Map(
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
            ))
          mapToPostParams(Option(map), "external_account")
        case source: CardData.Source.Object =>
          val map = PostParams.flatten(
            Map(
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
            ))
          mapToPostParams(Option(map), "source")
      }
    } ++ mapToPostParams(cardInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources"

    createRequestPOST[Card](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(customerId: String, cardId: String)(implicit apiKey: ApiKey,
                                              endpoint: Endpoint,
                                              client: HttpExt,
                                              materializer: Materializer,
                                              executionContext: ExecutionContext): Future[Try[Card]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources/$cardId"

    createRequestGET[Card](finalUrl, logger)
  }

  def delete(customerId: String, cardId: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[DeleteResponse]] = {

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
  case class CardListInput(endingBefore: Option[String] = None,
                           limit: Option[Long] = None,
                           startingAfter: Option[String] = None)

  case class CardList(override val url: String,
                      override val hasMore: Boolean,
                      override val data: List[Card],
                      override val totalCount: Option[Long])
      extends Collections.List[Card](url, hasMore, data, totalCount)

  object CardList extends Collections.ListJsonMappers[Card] {
    implicit val cardListDecoder: Decoder[CardList] =
      listDecoder(implicitly)(CardList.apply)

    implicit val cardListEncoder: Encoder[CardList] =
      listEncoder[CardList]
  }

  def list(customerId: String, cardListInput: CardListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[CardList]] = {
    val finalUrl = {
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl =
        endpoint.url + s"/v1/customers/$customerId/sources$totalCountUrl"

      val queries = PostParams.flatten(
        Map(
          "object"         -> Option("card"),
          "ending_before"  -> cardListInput.endingBefore,
          "limit"          -> cardListInput.limit.map(_.toString),
          "starting_after" -> cardListInput.startingAfter
        ))

      Uri(baseUrl).withQuery(Query(queries))
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

  implicit val bankAccountDecoder: Decoder[BankAccount] = Decoder.forProduct6(
    "id",
    "account_holder_name",
    "last4",
    "routing_number",
    "country",
    "default_for_currency"
  )(BankAccount.apply)

  implicit val bankAccountEncoder: Encoder[BankAccount] = Encoder.forProduct6(
    "id",
    "account_holder_name",
    "last4",
    "routing_number",
    "country",
    "default_for_currency"
  )(x => BankAccount.unapply(x).get)
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

  implicit val transactionDecoder: Decoder[Transaction] = Decoder.forProduct6(
    "id",
    "amount",
    "bitcoin_amount",
    "created",
    "currency",
    "receiver"
  )(Transaction.apply)

  implicit val transactionEncoder: Encoder[Transaction] = Encoder.forProduct7(
    "id",
    "object",
    "amount",
    "bitcoin_amount",
    "created",
    "currency",
    "receiver"
  )(x => (x.id, "list", x.amount, x.bitcoinAmount, x.created, x.currency, x.receiver))

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
    implicit val transactionListDecoder: Decoder[TransactionList] =
      listDecoder(implicitly)(TransactionList.apply)

    implicit val transactionListEncoder: Encoder[TransactionList] =
      listEncoder[TransactionList]
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
    * @param uncapturedFunds       This receiver contains uncaptured funds that
    *                              can be used for a payment or refunded.
    * @param usedForPayment
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
                             uncapturedFunds: Boolean,
                             usedForPayment: Boolean,
                             metadata: Option[Map[String, String]] = None,
                             payment: Option[String] = None,
                             refundAddress: Option[String] = None,
                             transactions: Option[TransactionList] = None)
      extends StripeObject
      with PaymentSource

  implicit val bitcoinReceiverDecoder: Decoder[BitcoinReceiver] = Decoder.forProduct21(
    "id",
    "active",
    "amount",
    "amount_received",
    "bitcoin_amount",
    "bitcoin_amount_received",
    "bitcoin_uri",
    "created",
    "currency",
    "customer",
    "description",
    "email",
    "filled",
    "inbound_address",
    "livemode",
    "uncaptured_funds",
    "used_for_payment",
    "metadata",
    "payment",
    "refund_address",
    "transactions"
  )(BitcoinReceiver.apply)

  implicit val bitcoinReceiverEncoder: Encoder[BitcoinReceiver] = Encoder.forProduct22(
    "id",
    "object",
    "active",
    "amount",
    "amount_received",
    "bitcoin_amount",
    "bitcoin_amount_received",
    "bitcoin_uri",
    "created",
    "currency",
    "customer",
    "description",
    "email",
    "filled",
    "inbound_address",
    "livemode",
    "uncaptured_funds",
    "used_for_payment",
    "metadata",
    "payment",
    "refund_address",
    "transactions"
  )(
    x =>
      (
        x.id,
        "bitcoin_receiver",
        x.active,
        x.amount,
        x.amountReceived,
        x.bitcoinAmount,
        x.bitcoinAmountReceived,
        x.bitcoinUri,
        x.created,
        x.currency,
        x.customer,
        x.description,
        x.email,
        x.filled,
        x.inboundAddress,
        x.livemode,
        x.metadata,
        x.payment,
        x.refundAddress,
        x.transactions,
        x.uncapturedFunds,
        x.usedForPayment
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
                                  description: Option[String] = None,
                                  metadata: Option[Map[String, String]] = None,
                                  refundMispayments: Option[Boolean] = None)

  implicit val bitcoinReceiverInputDecoder: Decoder[BitcoinReceiverInput] = Decoder.forProduct6(
    "amount",
    "currency",
    "email",
    "description",
    "metadata",
    "refund_mispayments"
  )(BitcoinReceiverInput.apply)

  implicit val bitcoinReceiverInputEncoder: Encoder[BitcoinReceiverInput] = Encoder.forProduct6(
    "amount",
    "currency",
    "email",
    "description",
    "metadata",
    "refund_mispayments"
  )(x => BitcoinReceiverInput.unapply(x).get)

  def create(bitcoinReceiverInput: BitcoinReceiverInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[BitcoinReceiver]] = {

    val postFormParameters = PostParams.flatten(
      Map(
        "amount"             -> Option(bitcoinReceiverInput.amount.toString()),
        "currency"           -> Option(bitcoinReceiverInput.currency.iso.toLowerCase()),
        "email"              -> Option(bitcoinReceiverInput.email),
        "description"        -> bitcoinReceiverInput.description,
        "refund_mispayments" -> Option(bitcoinReceiverInput.refundMispayments.toString)
      )) ++ mapToPostParams(bitcoinReceiverInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/bitcoin/receivers"

    createRequestPOST[BitcoinReceiver](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(id: String)(implicit apiKey: ApiKey,
                      endpoint: Endpoint,
                      client: HttpExt,
                      materializer: Materializer,
                      executionContext: ExecutionContext): Future[Try[BitcoinReceiver]] = {
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
  case class BitcoinReceiverListInput(active: Option[Boolean] = None,
                                      endingBefore: Option[String] = None,
                                      filled: Option[Boolean] = None,
                                      limit: Option[Long] = None,
                                      startingAfter: Option[String] = None,
                                      uncapturedFunds: Option[Boolean] = None)

  case class BitcoinReceiverList(override val url: String,
                                 override val hasMore: Boolean,
                                 override val data: List[BitcoinReceiver],
                                 override val totalCount: Option[Long])
      extends Collections.List[BitcoinReceiver](url, hasMore, data, totalCount)

  object BitcoinReceiverList extends Collections.ListJsonMappers[BitcoinReceiver] {
    implicit val bitcoinReceiverListDecoder: Decoder[BitcoinReceiverList] =
      listDecoder(implicitly)(BitcoinReceiverList.apply)

    implicit val bitcoinReceiverListEncoder: Encoder[BitcoinReceiverList] =
      listEncoder[BitcoinReceiverList]
  }

  def list(bitcoinReceiverListInput: BitcoinReceiverListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[BitcoinReceiverList]] = {
    val finalUrl = {
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/bitcoin/receivers$totalCountUrl"

      val queries = PostParams.flatten(
        Map(
          "active"           -> bitcoinReceiverListInput.active.map(_.toString),
          "ending_before"    -> bitcoinReceiverListInput.endingBefore,
          "filled"           -> bitcoinReceiverListInput.filled.map(_.toString),
          "limit"            -> bitcoinReceiverListInput.limit.map(_.toString),
          "starting_after"   -> bitcoinReceiverListInput.startingAfter,
          "uncaptured_funds" -> bitcoinReceiverListInput.uncapturedFunds.map(_.toString)
        ))

      Uri(baseUrl).withQuery(Query(queries))
    }

    createRequestGET[BitcoinReceiverList](finalUrl, logger)
  }
}
