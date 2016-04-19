package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime
import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/**
  * @see https://stripe.com/docs/api#tokens
  */

object Tokens extends LazyLogging {

  sealed abstract class Type(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Type extends Enum[Type] {

    val values = findValues

    case object Card extends Type("card")

    case object BankAccount extends Type("bank_account")

    case object Pii extends Type("pii")

  }

  implicit val typeFormats = EnumFormats.formats(Type, insensitive = true)

  /**
    * @see https://stripe.com/docs/api#retrieve_token
    * @param id
    * @param bankAccount Hash describing the bank account
    * @param card        Hash describing the card used to make the charge
    * @param clientIp    IP address of the client that generated the token
    * @param created
    * @param livemode
    * @param `type`      Type of the token: [[Type.Card]] or [[Type.BankAccount]]
    * @param used        Whether or not this token has already
    *                    been used (tokens can be used only once)
    */

  case class Token(id: String,
                   bankAccount: Option[BankAccounts.BankAccount],
                   card: Option[Cards.Card],
                   clientIp: Option[String],
                   created: OffsetDateTime,
                   livemode: Boolean,
                   `type`: Type,
                   used: Boolean
                  )

  object Token {
    def default(id: String,
                created: OffsetDateTime,
                livemode: Boolean,
                `type`: Type,
                used: Boolean): Token = Token(
      id,
      None,
      None,
      None,
      created,
      livemode,
      `type`,
      used
    )
  }

  implicit val tokenReads: Reads[Token] = (
    (__ \ "id").read[String] ~
      (__ \ "bank_account").readNullable[BankAccounts.BankAccount] ~
      (__ \ "card").readNullable[Cards.Card] ~
      (__ \ "client_ip").readNullable[String] ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "type").read[Type] ~
      (__ \ "used").read[Boolean]
    ).tupled.map((Token.apply _).tupled)

  implicit val tokenWrites: Writes[Token] =
    Writes((token: Token) =>
      Json.obj(
        "id" -> token.id,
        "bank_account" -> token.bankAccount,
        "card" -> token.card,
        "client_ip" -> token.clientIp,
        "created" -> Json.toJson(token.created)(stripeDateTimeWrites),
        "livemode" -> token.livemode,
        "type" -> token.`type`,
        "used" -> token.used
      )
    )

  sealed abstract class TokenData

  object TokenData {

    /** Creates a single use token that wraps the details of a credit card.
      * This token can be used in place of a credit card dictionary with
      * any API method. These tokens can only be used once: by creating a
      * new charge object, or attaching them to a customer.
      *
      * @see https://stripe.com/docs/api#create_card_token
      * @param expMonth Two digit number representing
      *                 the card's expiration month.
      * @param expYear  Two or four digit number representing
      *                 the card's expiration year.
      * @param number   The card number, as a string
      *                 without any separators.
      * @param addressCity
      * @param addressCountry
      * @param addressLine1
      * @param addressLine2
      * @param addressState
      * @param addressZip
      * @param currency Required to be able to add the card to an
      *                 account (in all other cases, this parameter is not used).
      *                 When added to an account, the card (which must
      *                 be a debit card) can be used as a transfer
      *                 destination for funds in this currency.
      *                 Currently, the only supported currency
      *                 for debit card transfers is usd.
      * @param cvc      Card security code. Required unless your
      *                 account is registered in Australia, Canada,
      *                 or the United States. Highly recommended to
      *                 always include this value.
      * @param name     Cardholder's full name.
      */

    case class Card(expMonth: Int,
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
                    name: Option[String]) extends TokenData

    object Card {
      def default(expMonth: Int,
                  expYear: Int,
                  number: String): Card = Card(
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

    implicit val cardReads: Reads[Card] = (
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
        (__ \ "name").readNullable[String]
      ).tupled.map((Card.apply _).tupled)

    implicit val cardWrites: Writes[Card] =
      Writes((card: Card) =>
        Json.obj(
          "exp_month" -> card.expMonth,
          "exp_year" -> card.expYear,
          "number" -> card.number,
          "address_city" -> card.addressCity,
          "address_country" -> card.addressCountry,
          "address_line1" -> card.addressLine1,
          "address_line2" -> card.addressLine2,
          "address_state" -> card.addressState,
          "address_zip" -> card.addressZip,
          "currency" -> card.currency,
          "cvc" -> card.cvc,
          "name" -> card.name
        )
      )

    /** Creates a single use token that wraps the details of a bank account.
      * This token can be used in place of a bank account dictionary with
      * any API method. These tokens can only be used once: by attaching
      * them to a recipient or managed account.
      *
      * @see https://stripe.com/docs/api#create_bank_account_token
      * @param accountNumber     The account number for the bank account
      *                          in string form. Must be a checking account.
      * @param country           The country the bank account is in.
      * @param currency          The currency the bank account is in. This
      *                          must be a country/currency pairing
      *                          that Stripe supports.
      * @param routingNumber     The routing number, sort code, or other
      *                          country-appropriate institution number for the
      *                          bank account. For US bank accounts, this is
      *                          required and should be the ACH routing number,
      *                          not the wire routing number. If you are providing
      *                          an IBAN for [[accountNumber]], this field is
      *                          not required.
      * @param accountHolderName The name of the person or business that owns
      *                          the bank account. This field is required when
      *                          attaching the bank account to a customer object.
      * @param accountHolderType The type of entity that holds the account. This
      *                          can be either "individual" or "company". This field
      *                          is required when attaching the bank account to
      *                          a customer object.
      */
    case class BankAccount(accountNumber: String,
                           country: String,
                           currency: Currency,
                           routingNumber: Option[String],
                           accountHolderName: Option[String],
                           accountHolderType: Option[BankAccounts.AccountHolderType]
                          ) extends TokenData

    object BankAccount {
      def default(accountNumber: String,
                  country: String,
                  currency: Currency): BankAccount = BankAccount(
        accountNumber,
        country,
        currency,
        None,
        None,
        None
      )
    }

    implicit val bankAccountReads: Reads[BankAccount] = (
      (__ \ "account_number").read[String] ~
        (__ \ "country").read[String] ~
        (__ \ "currency").read[Currency] ~
        (__ \ "routing_number").readNullable[String] ~
        (__ \ "account_holder_name").readNullable[String] ~
        (__ \ "account_holder_type").readNullable[BankAccounts.AccountHolderType]
      ).tupled.map((BankAccount.apply _).tupled)

    implicit val bankAccountWrites: Writes[BankAccount] =
      Writes((bankAccount: BankAccount) =>
        Json.obj(
          "account_number" -> bankAccount.accountNumber,
          "country" -> bankAccount.country,
          "currency" -> bankAccount.currency,
          "routing_number" -> bankAccount.routingNumber,
          "account_holder_name" -> bankAccount.accountHolderName,
          "account_holder_type" -> bankAccount.accountHolderType
        )
      )

    /** Creates a single use token that wraps the details of personally
      * identifiable information (PII). This token can be used in place
      * of a [[personalIdNumber]] in the Account Update API method. These
      * tokens can only be used once.
      *
      * @see https://stripe.com/docs/api#create_pii_token
      * @param pii              The PII this token will represent.
      * @param personalIdNumber The [[personalIdNumber]] for PII
      *                         in string form.
      */

    case class PII(pii: Option[String],
                   personalIdNumber: String) extends TokenData

    object PII {
      def default(personalIdNumber: String): PII = PII(
        None,
        personalIdNumber
      )
    }

    implicit val PIIReads: Reads[PII] = (
      (__ \ "pii").readNullable[String] ~
        (__ \ "personal_id_number").read[String]
      ).tupled.map((PII.apply _).tupled)

    implicit val PIIWrites: Writes[PII] =
      Writes((pii: PII) =>
        Json.obj(
          "pii" -> pii.pii,
          "personal_id_number" -> pii.personalIdNumber
        )
      )

  }

  case class TokenInput(tokenData: TokenData,
                        customer: Option[String]
                       )

  object TokenInput {
    def default(tokenData: TokenData): TokenInput = TokenInput(
      tokenData,
      None
    )
  }

  def create(tokenInput: TokenInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Token]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "customer" -> tokenInput.customer
      ).collect {
        case (k, Some(v)) => (k, v)
      } ++ {
        tokenInput.tokenData match {
          case card: TokenData.Card =>
            val map = Map(
              "exp_month" -> Option(card.expMonth.toString),
              "exp_year" -> Option(card.expYear.toString),
              "number" -> Option(card.number),
              "address_city" -> card.addressCity,
              "address_country" -> card.addressCountry,
              "address_line1" -> card.addressLine1,
              "address_line2" -> card.addressLine2,
              "address_state" -> card.addressState,
              "address_zip" -> card.addressZip,
              "currency" -> card.currency.map(_.iso.toLowerCase),
              "cvc" -> card.cvc,
              "name" -> card.name
            ).collect {
              case (k, Some(v)) => (k, v)
            }
            mapToPostParams(Option(map), "card")
          case bankAccount: TokenData.BankAccount =>
            val map = Map(
              "account_number" -> Option(bankAccount.accountNumber),
              "country" -> Option(bankAccount.country),
              "currency" -> Option(bankAccount.currency.iso.toLowerCase),
              "routing_number" -> bankAccount.routingNumber,
              "account_holder_name" -> bankAccount.accountHolderName,
              "account_holder_type" -> bankAccount.accountHolderType.map(_.id)
            ).collect {
              case (k, Some(v)) => (k, v)
            }
            mapToPostParams(Option(map), "bank_account")
          case pii: TokenData.PII =>
            val map = Map(
              "pii" -> pii.pii,
              "personal_id_number" -> Option(pii.personalIdNumber)
            ).collect {
              case (k, Some(v)) => (k, v)
            }
            mapToPostParams(Option(map), "pii")
        }
      }
    }

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + s"/v1/tokens"

    createRequestPOST[Token](finalUrl, postFormParameters, idempotencyKey, logger)

  }

  def get(id: String)
         (implicit apiKey: ApiKey,
          endpoint: Endpoint): Future[Try[Token]] = {
    val finalUrl = endpoint.url + s"/v1/tokens/$id"

    createRequestGET[Token](finalUrl, logger)

  }

}
