package org.mdedetrich.stripe.v1

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Query
import akka.stream.Materializer
import com.typesafe.scalalogging.LazyLogging
import defaults._
import enumeratum._
import io.circe._
import io.circe.syntax._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object BankAccounts extends LazyLogging {

  sealed abstract class AccountHolderType(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object AccountHolderType extends Enum[AccountHolderType] {
    val values = findValues

    case object Individual extends AccountHolderType("individual")
    case object Company    extends AccountHolderType("company")

    implicit val accountHolderTypeDecoder: Decoder[AccountHolderType] =
      enumeratum.Circe.decoder(AccountHolderType)
    implicit val accountHolderTypeEncoder: Encoder[AccountHolderType] =
      enumeratum.Circe.encoder(AccountHolderType)
  }

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {
    val values = findValues

    case object New                extends Status("new")
    case object Validated          extends Status("validated")
    case object Verified           extends Status("verified")
    case object VerificationFailed extends Status("verification_failed")
    case object Errored            extends Status("errored")

    implicit val statusDecoder: Decoder[Status] = enumeratum.Circe.decoder(Status)
    implicit val statusEncoder: Encoder[Status] = enumeratum.Circe.encoder(Status)
  }

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

  implicit val bankAccountDecoder: Decoder[BankAccount] = Decoder.forProduct14(
    "id",
    "account",
    "account_holder_name",
    "account_holder_type",
    "bank_name",
    "country",
    "currency",
    "default_for_currency",
    "fingerprint",
    "last4",
    "metadata",
    "name",
    "routing_number",
    "status"
  )(BankAccount.apply)

  implicit val bankAccountEncoder: Encoder[BankAccount] = Encoder.forProduct15(
    "id",
    "object",
    "account",
    "account_holder_name",
    "account_holder_type",
    "bank_name",
    "country",
    "currency",
    "default_for_currency",
    "fingerprint",
    "last4",
    "metadata",
    "name",
    "routing_number",
    "status"
  )(
    x =>
      (
        x.id,
        "bank_account",
        x.account,
        x.accountHolderName,
        x.accountHolderType,
        x.bankName,
        x.country,
        x.currency,
        x.defaultForCurrency,
        x.fingerprint,
        x.last4,
        x.metadata,
        x.name,
        x.routingNumber,
        x.status
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

        def default(accountNumber: String, country: String, currency: Currency): Object = Object(
          accountNumber,
          country,
          currency,
          None,
          None,
          None
        )
      }

      implicit val sourceObjectDecoder: Decoder[Object] = Decoder.forProduct6(
        "account_number",
        "country",
        "currency",
        "account_holder_name",
        "account_holder_type",
        "routing_number"
      )(Object.apply)

      implicit val sourceObjectEncoder: Encoder[Object] = Encoder.forProduct6(
        "account_number",
        "country",
        "currency",
        "account_holder_name",
        "account_holder_type",
        "routing_number"
      )(x => Object.unapply(x).get)

      implicit val sourceObjectParams: PostParams[Object] = PostParams.params[Object] { externalAccount =>
        Map(
          "object"         -> "bank_account",
          "account_number" -> externalAccount.accountNumber,
          "country"        -> externalAccount.country,
          "currency"       -> externalAccount.currency.iso
        )
      }

      case class Token(id: String) extends Source

      implicit val sourceTokenDecoder: Decoder[Token] = Decoder[String].map(Token)
      implicit val sourceTokenEncoder: Encoder[Token] = Encoder.instance[Token](_.id.asJson)
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
        def default(accountNumber: String, country: String, currency: Currency): Object = Object(
          accountNumber,
          country,
          currency,
          None,
          None,
          None
        )
      }

      implicit val externalAccountObjectDecoder: Decoder[Object] = Decoder.forProduct6(
        "account_number",
        "country",
        "currency",
        "account_holder_name",
        "account_holder_type",
        "routing_number"
      )(Object.apply)

      implicit val externalAccountObjectEncoder: Encoder[Object] = Encoder.forProduct6(
        "account_number",
        "country",
        "currency",
        "account_holder_name",
        "account_holder_type",
        "routing_number"
      )(x => Object.unapply(x).get)

      case class Token(id: String) extends ExternalAccount

      implicit val externalAccountTokenDecoder: Decoder[Token] = Decoder[String].map(Token)
      implicit val externalAccountTokenEncoder: Encoder[Token] = Encoder.instance[Token](_.id.asJson)
    }
  }

  implicit val bankAccountDataEncoder: Encoder[BankAccountData] = Encoder.instance[BankAccountData] {
    case bankAccountData: BankAccountData.Source.Object          => bankAccountData.asJson
    case bankAccountData: BankAccountData.ExternalAccount.Object => bankAccountData.asJson
    case bankAccountData: BankAccountData.Source.Token           => bankAccountData.asJson
    case bankAccountData: BankAccountData.ExternalAccount.Token  => bankAccountData.asJson
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

  def create(customerId: String, bankAccountInput: BankAccountInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[BankAccount]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "default_for_currency" -> bankAccountInput.defaultForCurrency.map(_.toString)
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
            "account_number"      -> Option(externalAccount.accountNumber),
            "country"             -> Option(externalAccount.country),
            "currency"            -> Option(externalAccount.currency.iso.toLowerCase),
            "account_holder_name" -> externalAccount.accountHolderName,
            "account_holder_type" -> externalAccount.accountHolderType.map(_.id),
            "routing_number"      -> externalAccount.routingNumber
          ).collect {
            case (k, Some(v)) => (k, v)
          }
          mapToPostParams(Option(map), "external_account")
        case source: BankAccountData.Source.Object =>
          val map = Map(
            "account_number"      -> Option(source.accountNumber),
            "country"             -> Option(source.country),
            "currency"            -> Option(source.currency.iso.toLowerCase),
            "account_holder_name" -> source.accountHolderName,
            "account_holder_type" -> source.accountHolderType.map(_.id),
            "routing_number"      -> source.routingNumber
          ).collect {
            case (k, Some(v)) => (k, v)
          }
          mapToPostParams(Option(map), "source")
      }
    } ++ mapToPostParams(bankAccountInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources"

    createRequestPOST[BankAccount](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(customerId: String, bankAccountId: String)(implicit apiKey: ApiKey,
                                                     endpoint: Endpoint,
                                                     client: HttpExt,
                                                     materializer: Materializer,
                                                     executionContext: ExecutionContext): Future[Try[BankAccount]] = {
    val finalUrl =
      endpoint.url + s"/v1/customers/$customerId/sources/$bankAccountId"

    createRequestGET[BankAccount](finalUrl, logger)
  }

  def delete(customerId: String, bankAccountId: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[DeleteResponse]] = {
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
  case class BankAccountListInput(endingBefore: Option[String], limit: Option[Long], startingAfter: Option[String])

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
    implicit val bankAccountListDecoder: Decoder[BankAccountList] =
      listDecoder(implicitly)(BankAccountList.apply)
    implicit val bankAccountListEncoder: Encoder[BankAccountList] =
      listEncoder[BankAccountList]
  }

  def list(customerId: String, bankAccountListInput: BankAccountListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[BankAccountList]] = {
    val finalUrl = {
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl =
        endpoint.url + s"/v1/customers/$customerId/sources$totalCountUrl"

      val queries = Map(
        "object"         -> Option("bank_account"),
        "ending_before"  -> bankAccountListInput.endingBefore,
        "limit"          -> bankAccountListInput.limit.map(_.toString),
        "starting_after" -> bankAccountListInput.startingAfter
      ).collect { case (a, Some(b)) => (a, b) }

      Uri(baseUrl).withQuery(Query(queries))
    }

    createRequestGET[BankAccountList](finalUrl, logger)
  }
}
