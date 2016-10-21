package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

object BankAccounts extends LazyLogging {

  sealed abstract class AccountHolderType(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object AccountHolderType extends Enum[AccountHolderType] {

    val values = findValues

    case object Individual extends AccountHolderType("individual")

    case object Company extends AccountHolderType("company")
  }

  implicit val accountHolderTypeFormats =
    EnumFormats.formats(AccountHolderType, insensitive = true)

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {

    val values = findValues

    case object New extends Status("new")

    case object Validated extends Status("validated")

    case object Verified extends Status("verified")

    case object VerificationFailed extends Status("verification_failed")

    case object Errored extends Status("errored")
  }

  implicit val statusFormats = EnumFormats.formats(Status, insensitive = true)

  /**
    *
    * @param id
    * @param account
    * @param accountHolderName  The name of the person or business that owns the bank account.
    * @param accountHolderType  The type of entity that holds the account. This can be either [[AccountHolderType.Individual]] or [[AccountHolderType.Company]]
    * @param bankName           Name of the bank associated with the routing number, e.g. WELLS FARGO
    * @param country            Two-letter ISO code representing the country the bank account is located in.
    * @param currency           Three-letter ISO currency code representing the currency paid out to the bank account.
    * @param defaultForCurrency This indicates whether or not this bank account is the default external account for its currency.
    * @param fingerprint        Uniquely identifies this particular bank account. You can use this attribute to check whether two bank accounts are the same.
    * @param last4
    * @param metadata           A set of key/value pairs that you can attach to a bank account object. It can be useful for storing additional information about the bank account in a structured format.
    * @param routingNumber      The routing transit number for the bank account.
    * @param status             Possible values are new, [[Status.Validated]], [[Status.Verified]], [[Status.VerificationFailed]], or errored.
    *                           A bank account that hasn’t had any activity or validation performed is new.
    *                           If Stripe can determine that the bank account exists, its status will be validated.
    *                           Note that there often isn’t enough information to know (e.g. for smaller credit unions),
    *                           and the validation is not always run. If customer bank account verification has succeeded,
    *                           the bank account status will be verified. If the verification failed for any reason, such
    *                           as microdeposit failure, the status will be [[Status.VerificationFailed]].
    *                           If a transfer sent to this bank account fails, we’ll set the status to errored and will
    *                           not continue to send transfers until the bank details are updated.
    */
  case class BankAccount(id: String,
                         account: Option[String],
                         accountHolderName: Option[String],
                         accountHolderType: Option[AccountHolderType],
                         bankName: String,
                         country: String,
                         currency: Currency,
                         defaultForCurrency: Option[Boolean],
                         fingerprint: String,
                         last4: String,
                         metadata: Option[Map[String, String]],
                         name: Option[String],
                         routingNumber: String,
                         status: Status)
      extends StripeObject

  implicit val bankAccountReads: Reads[BankAccount] = (
      (__ \ "id").read[String] ~
      (__ \ "account").readNullable[String] ~
      (__ \ "account_holder_name").readNullable[String] ~
      (__ \ "account_holder_type").readNullable[AccountHolderType] ~
      (__ \ "bank_name").read[String] ~
      (__ \ "country").read[String] ~
      (__ \ "currency").read[Currency] ~
      (__ \ "default_for_currency").readNullable[Boolean] ~
      (__ \ "fingerprint").read[String] ~
      (__ \ "last4").read[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "name").readNullable[String] ~
      (__ \ "routing_number").read[String] ~
      (__ \ "status").read[Status]
  ).tupled.map((BankAccount.apply _).tupled)

  implicit val bankAccountWrites: Writes[BankAccount] = Writes(
      (bankAccount: BankAccount) =>
        Json.obj(
            "id" -> bankAccount.id,
            "object" -> "bank_account",
            "account" -> bankAccount.account,
            "account_holder_name" -> bankAccount.accountHolderName,
            "account_holder_type" -> bankAccount.accountHolderType,
            "bank_name" -> bankAccount.bankName,
            "country" -> bankAccount.country,
            "currency" -> bankAccount.currency,
            "default_for_currency" -> bankAccount.defaultForCurrency,
            "fingerprint" -> bankAccount.fingerprint,
            "last4" -> bankAccount.last4,
            "metadata" -> bankAccount.metadata,
            "name" -> bankAccount.name,
            "routing_number" -> bankAccount.routingNumber,
            "status" -> bankAccount.status
      ))

  /**
    * @see https://stripe.com/docs/api#create_bank_account-source | external_account
    */
  sealed abstract class BankAccountData

  object BankAccountData {

    sealed abstract class Source extends BankAccountData

    object Source {

      /**
        * @param accountNumber     The account number for the bank account in string form. Must be a checking account.
        * @param country           The country the bank account is in.
        * @param currency          The currency the bank account is in. This must be a country/currency pairing that Stripe supports.
        * @param accountHolderName The name of the person or business that owns the bank account. This field is required when attaching the bank account to a customer object.
        * @param accountHolderType The type of entity that holds the account. This can be either [[AccountHolderType.Individual]] or [[AccountHolderType.Company]]. This field is
        *                          required when attaching the bank account to a customer object.
        * @param routingNumber     The routing number, sort code, or other country-appropriate institution number for the bank account.
        *                          For US bank accounts, this is required and should be the ACH routing number, not the wire
        *                          routing number. If you are providing an IBAN for [[accountNumber]], this field is not required.
        */
      case class Object(accountNumber: String,
                        country: String,
                        currency: Currency,
                        accountHolderName: Option[String],
                        accountHolderType: Option[AccountHolderType],
                        routingNumber: Option[String])
          extends Source

      object Object {

        def default(accountNumber: String,
                    country: String,
                    currency: Currency): Object = Object(
            accountNumber,
            country,
            currency,
            None,
            None,
            None
        )
      }

      implicit val sourceObjectReads: Reads[Object] = (
          (__ \ "account_number").read[String] ~
          (__ \ "country").read[String] ~
          (__ \ "currency").read[Currency] ~
          (__ \ "account_holder_name").readNullable[String] ~
          (__ \ "account_holder_type").readNullable[AccountHolderType] ~
          (__ \ "routing_number").readNullable[String]
      ).tupled.map((Object.apply _).tupled)

      implicit val sourceObjectWrites: Writes[Object] = Writes(
          (`object`: Object) =>
            Json.obj(
                "account_number" -> `object`.accountNumber,
                "country" -> `object`.country,
                "currency" -> `object`.currency,
                "account_holder_name" -> `object`.accountHolderName,
                "account_holder_type" -> `object`.accountHolderType,
                "routing_number" -> `object`.routingNumber
          ))

      implicit val sourceObjectParams = new PostParams[Object] {
        override def toMap(externalAccount: Object): Map[String, String] = Map(
          "object" -> "bank_account",
          "account_number" -> externalAccount.accountNumber,
          "country" -> externalAccount.country,
          "currency" -> externalAccount.currency.iso
        )
      }

      case class Token(id: String) extends Source

      implicit val sourceTokenReads: Reads[Token] = Reads.of[String].map(Token)

      implicit val sourceTokenWrites: Writes[Token] = Writes(
          (token: Token) => JsString(token.id))
    }

    sealed abstract class ExternalAccount extends BankAccountData

    object ExternalAccount {

      /**
        * @param accountNumber     The account number for the bank account in string form. Must be a checking account.
        * @param country           The country the bank account is in.
        * @param currency          The currency the bank account is in. This must be a country/currency pairing that Stripe supports.
        * @param accountHolderName The name of the person or business that owns the bank account. This field is required when attaching the bank account to a customer object.
        * @param accountHolderType The type of entity that holds the account. This can be either [[AccountHolderType.Individual]] or [[AccountHolderType.Company]]. This field is
        *                          required when attaching the bank account to a customer object.
        * @param routingNumber     The routing number, sort code, or other country-appropriate institution number for the bank account.
        *                          For US bank accounts, this is required and should be the ACH routing number, not the wire
        *                          routing number. If you are providing an IBAN for [[accountNumber]], this field is not required.
        */
      case class Object(accountNumber: String,
                        country: String,
                        currency: Currency,
                        accountHolderName: Option[String],
                        accountHolderType: Option[AccountHolderType],
                        routingNumber: Option[String])
          extends ExternalAccount

      object Object {
        def default(accountNumber: String,
                    country: String,
                    currency: Currency): Object = Object(
            accountNumber,
            country,
            currency,
            None,
            None,
            None
        )
      }

      implicit val externalAccountObjectReads: Reads[Object] = (
          (__ \ "account_number").read[String] ~
          (__ \ "country").read[String] ~
          (__ \ "currency").read[Currency] ~
          (__ \ "account_holder_name").readNullable[String] ~
          (__ \ "account_holder_type").readNullable[AccountHolderType] ~
          (__ \ "routing_number").readNullable[String]
      ).tupled.map((Object.apply _).tupled)

      implicit val externalAccountObject: Writes[Object] = Writes(
          (`object`: Object) =>
            Json.obj(
                "account_number" -> `object`.accountNumber,
                "country" -> `object`.country,
                "currency" -> `object`.currency,
                "account_holder_name" -> `object`.accountHolderName,
                "account_holder_type" -> `object`.accountHolderType,
                "routing_number" -> `object`.routingNumber
          ))

      case class Token(id: String) extends ExternalAccount

      implicit val externalAccountTokenReads: Reads[Token] =
        Reads.of[String].map(Token)

      implicit val externalAccountTokenWrites: Writes[Token] = Writes(
          (token: Token) => JsString(token.id))
    }
  }

  implicit val bankAccountDataWrites: Writes[BankAccountData] = Writes {
    (bankAccountData: BankAccountData) =>
      bankAccountData match {
        case bankAccountData: BankAccountData.Source.Object =>
          Json.toJson(bankAccountData)
        case bankAccountData: BankAccountData.ExternalAccount.Object =>
          Json.toJson(bankAccountData)
        case bankAccountData: BankAccountData.Source.Token =>
          Json.toJson(bankAccountData)
        case bankAccountData: BankAccountData.ExternalAccount.Token =>
          Json.toJson(bankAccountData)
      }
  }

  /**
    * @see https://stripe.com/docs/api#create_bank_account
    * @param bankAccountData    When adding a bank account to a customer, the parameter name is [[BankAccountData.Source]].
    *                           When adding to an account, the parameter name is [[BankAccountData.ExternalAccount]].
    *                           The value can either be a token, like the ones returned by Stripe.js,
    *                           or a dictionary containing a user’s bank account details (with the options shown below).
    * @param defaultForCurrency If you set this to true (or if this is the first external account being
    *                           added in this currency) this bank account will become the default
    *                           external account for its currency.
    * @param metadata           A set of key/value pairs that you can attach to an external account
    *                           object. It can be useful for storing additional information about the
    *                           external account in a structured format.
    */
  case class BankAccountInput(bankAccountData: BankAccountData,
                              defaultForCurrency: Option[Currency],
                              metadata: Option[Map[String, String]])

  def create(customerId: String, bankAccountInput: BankAccountInput)(
      idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[BankAccount]] = {
    val postFormParameters: Map[String, String] = {
      Map(
          "default_for_currency" -> bankAccountInput.defaultForCurrency.map(
              _.toString)
      )
    }.collect {
      case (k, Some(v)) => (k, v)
    } ++ {
      bankAccountInput.bankAccountData match {
        case BankAccountData.ExternalAccount.Token(id) =>
          Map("external_account" -> id)
        case BankAccountData.Source.Token(id) =>
          Map("source" -> id)
        case externalAccount: BankAccountData.ExternalAccount.Object =>
          val map = Map(
              "account_number" -> Option(externalAccount.accountNumber),
              "country" -> Option(externalAccount.country),
              "currency" -> Option(externalAccount.currency.iso.toLowerCase),
              "account_holder_name" -> externalAccount.accountHolderName,
              "account_holder_type" -> externalAccount.accountHolderType.map(
                  _.id),
              "routing_number" -> externalAccount.routingNumber
          ).collect {
            case (k, Some(v)) => (k, v)
          }
          mapToPostParams(Option(map), "external_account")
        case source: BankAccountData.Source.Object =>
          val map = Map(
              "account_number" -> Option(source.accountNumber),
              "country" -> Option(source.country),
              "currency" -> Option(source.currency.iso.toLowerCase),
              "account_holder_name" -> source.accountHolderName,
              "account_holder_type" -> source.accountHolderType.map(_.id),
              "routing_number" -> source.routingNumber
          ).collect {
            case (k, Some(v)) => (k, v)
          }
          mapToPostParams(Option(map), "source")
      }
    } ++ mapToPostParams(bankAccountInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources"

    createRequestPOST[BankAccount](
        finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(customerId: String, bankAccountId: String)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[BankAccount]] = {
    val finalUrl =
      endpoint.url + s"/v1/customers/$customerId/sources/$bankAccountId"

    createRequestGET[BankAccount](finalUrl, logger)
  }

  def delete(customerId: String, bankAccountId: String)(
      idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[DeleteResponse]] = {
    val finalUrl =
      endpoint.url + s"/v1/customers/$customerId/sources/$bankAccountId"

    createRequestDELETE(finalUrl, idempotencyKey, logger)
  }

  /**
    * @see https://stripe.com/docs/api#list_bank_accounts
    * @param endingBefore  A cursor for use in pagination. [[endingBefore]] is an object ID
    *                      that defines your place in the list. For instance, if you make a
    *                      list request and receive 100 objects, starting with obj_bar, your
    *                      subsequent call can include [[endingBefore]]=obj_bar in order to
    *                      fetch the previous page of the list.
    * @param limit         A limit on the number of objects to be returned. Limit can range between 1 and 100 items.
    * @param startingAfter A cursor for use in pagination. [[startingAfter]] is an object ID
    *                      that defines your place in the list. For instance, if you make a
    *                      list request and receive 100 objects, ending with obj_foo, your
    *                      subsequent call can include [[startingAfter]]=obj_foo in order
    *                      to fetch the next page of the list.
    */
  case class BankAccountListInput(endingBefore: Option[String],
                                  limit: Option[Long],
                                  startingAfter: Option[String])

  object BankAccountListInput {
    def default: BankAccountListInput = BankAccountListInput(
        None,
        None,
        None
    )
  }

  case class BankAccountList(override val url: String,
                             override val hasMore: Boolean,
                             override val data: List[BankAccount],
                             override val totalCount: Option[Long])
      extends Collections.List[BankAccount](url, hasMore, data, totalCount)

  object BankAccountList extends Collections.ListJsonMappers[BankAccount] {
    implicit val bankAccountListReads: Reads[BankAccountList] =
      listReads.tupled.map((BankAccountList.apply _).tupled)

    implicit val bankAccountListWrites: Writes[BankAccountList] = listWrites
  }

  def list(customerId: String,
           bankAccountListInput: BankAccountListInput,
           includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[BankAccountList]] = {
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
          ("object" -> "bank_account") ?
          ("ending_before" -> bankAccountListInput.endingBefore) ?
          ("limit" -> bankAccountListInput.limit.map(_.toString)) ?
          ("starting_after" -> bankAccountListInput.startingAfter)).toString()
    }

    createRequestGET[BankAccountList](finalUrl, logger)
  }
}
