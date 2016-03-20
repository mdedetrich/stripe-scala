package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import dispatch.Defaults._
import dispatch._
import enumeratum._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.Cards.CardData
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, InvalidJsonModelException}

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

  implicit val accountHolderTypeFormats = EnumFormats.formats(AccountHolderType, insensitive = true)

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

  case class BankAccount(id: String,
                         account: String,
                         accountHolderType: AccountHolderType,
                         bankName: String,
                         country: String,
                         currency: Currency,
                         defaultForCurrency: Boolean,
                         fingerprint: String,
                         last4: String,
                         metadata: Option[Map[String, String]],
                         name: String,
                         routingNumber: String,
                         status: Status) extends StripeObject

  implicit val bankAccountReads: Reads[BankAccount] = (
    (__ \ "id").read[String] ~
      (__ \ "account").read[String] ~
      (__ \ "account_holder_type").read[AccountHolderType] ~
      (__ \ "bank_name").read[String] ~
      (__ \ "country").read[String] ~
      (__ \ "currency").read[Currency] ~
      (__ \ "default_for_currency").read[Boolean] ~
      (__ \ "fingerprint").read[String] ~
      (__ \ "last4").read[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "name").read[String] ~
      (__ \ "routing_number").read[String] ~
      (__ \ "status").read[Status]
    ).tupled.map((BankAccount.apply _).tupled)


  implicit val bankAccountWrites: Writes[BankAccount] =
    Writes((bankAccount: BankAccount) =>
      Json.obj(
        "id" -> bankAccount.id,
        "object" -> "bank_account",
        "account" -> bankAccount.account,
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
      )
    )

  case class BankAccountList(override val url: String,
                             override val hasMore: Boolean,
                             override val data: List[BankAccount],
                             override val totalCount: Option[Long]
                            )
    extends Collections.List[BankAccount](url, hasMore, data, totalCount)

  object BankAccountList extends Collections.ListJsonMappers[BankAccount] {
    implicit val bankAccountListReads: Reads[BankAccountList] =
      listReads.tupled.map((BankAccountList.apply _).tupled)

    implicit val bankAccountListWrites: Writes[BankAccountList] =
      listWrites
  }

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

  sealed abstract class BankAccountData

  object BankAccountData {

    case class SourceObject(accountNumber: String,
                            country: String,
                            currency: Currency,
                            accountHolderName: Option[String],
                            accountHolderType: Option[AccountHolderType],
                            routingNumber: Option[String]) extends BankAccountData
    
    object SourceObject {
      
      def default(accountNumber: String,
                  country: String,
                  currency: Currency): SourceObject = SourceObject(
        accountNumber,
        country,
        currency,
        None,
        None,
        None
      )
    }

    implicit val sourceObjectReads: Reads[SourceObject] = (
      (__ \ "account_number").read[String] ~
        (__ \ "country").read[String] ~
        (__ \ "currency").read[Currency] ~
        (__ \ "account_holder_name").readNullable[String] ~
        (__ \ "account_holder_type").readNullable[AccountHolderType] ~
        (__ \ "routing_number").readNullable[String]
      ).tupled.map((SourceObject.apply _).tupled)
    
    implicit val sourceObjectWrites: Writes[SourceObject] =
      Writes ((sourceObject: SourceObject) =>
        Json.obj(
          "account_number" -> sourceObject.accountNumber,
          "country" -> sourceObject.country,
          "currency" -> sourceObject.currency,
          "account_holder_name" -> sourceObject.accountHolderName,
          "account_holder_type" -> sourceObject.accountHolderType,
          "routing_number" -> sourceObject.routingNumber
        )
      )

    case class ExternalAccountObject(accountNumber: String,
                                     country: String,
                                     currency: Currency,
                                     accountHolderName: Option[String],
                                     accountHolderType: Option[AccountHolderType],
                                     routingNumber: Option[String]) extends BankAccountData
    
    object ExternalAccountObject {
      def default(accountNumber: String,
                  country: String,
                  currency: Currency): ExternalAccountObject = ExternalAccountObject(
        accountNumber,
        country,
        currency,
        None,
        None,
        None
      )
    }

    implicit val externalAccountObjectReads: Reads[ExternalAccountObject] = (
      (__ \ "account_number").read[String] ~
        (__ \ "country").read[String] ~
        (__ \ "currency").read[Currency] ~
        (__ \ "account_holder_name").readNullable[String] ~
        (__ \ "account_holder_type").readNullable[AccountHolderType] ~
        (__ \ "routing_number").readNullable[String]
      ).tupled.map((ExternalAccountObject.apply _).tupled)

    implicit val externalAccountObject: Writes[ExternalAccountObject] =
      Writes ((externalAccountObject: ExternalAccountObject) =>
        Json.obj(
          "account_number" -> externalAccountObject.accountNumber,
          "country" -> externalAccountObject.country,
          "currency" -> externalAccountObject.currency,
          "account_holder_name" -> externalAccountObject.accountHolderName,
          "account_holder_type" -> externalAccountObject.accountHolderType,
          "routing_number" -> externalAccountObject.routingNumber
        )
      )

    case class SourceToken(id: String) extends BankAccountData

    implicit val sourceTokenReads: Reads[SourceToken] =
      Reads.of[String].map(SourceToken)

    implicit val sourceTokenWrites: Writes[SourceToken] =
      Writes((sourceToken: SourceToken) =>
        JsString(sourceToken.id)
      )
    
    case class ExternalAccountToken(id: String) extends BankAccountData
    
    implicit val externalAccountTokenReads: Reads[ExternalAccountToken] =
      Reads.of[String].map(ExternalAccountToken)

    implicit val externalAccountTokenWrites: Writes[ExternalAccountToken] =
      Writes((externalAccountToken: ExternalAccountToken) =>
        JsString(externalAccountToken.id)
      )

  }
  
  implicit val bankAccountDataWrites: Writes[BankAccountData] =
    Writes { (bankAccountData: BankAccountData) =>
      bankAccountData match {
        case bankAccountData: BankAccountData.SourceObject => Json.toJson(bankAccountData)
        case bankAccountData: BankAccountData.ExternalAccountObject => Json.toJson(bankAccountData)
        case bankAccountData: BankAccountData.SourceToken => Json.toJson(bankAccountData)
        case bankAccountData: BankAccountData.ExternalAccountToken => Json.toJson(bankAccountData)
      }
    }

  case class BankAccountInput(bankAccountData: BankAccountData,
                              defaultForCurrency: Option[Currency],
                              metadata: Option[Map[String, String]])

    def create(customerId: String, bankAccountInput: BankAccountInput)
              (idempotencyKey: Option[IdempotencyKey] = None)
              (implicit apiKey: ApiKey,
               endpoint: Endpoint): Future[Try[BankAccount]] = {
      val postFormParameters: Map[String, String] = {
        Map(
          "default_for_currency" -> bankAccountInput.defaultForCurrency.map(_.toString)
        )
      }.collect {
        case (k, Some(v)) => (k, v)
      } ++ {
        bankAccountInput.bankAccountData match {
          case BankAccountData.ExternalAccountToken(id) =>
            Map("external_account" -> id)
          case BankAccountData.SourceToken(id) =>
            Map("source" -> id)
          case externalAccount: BankAccountData.ExternalAccountObject =>
            val map = Map(
              "account_number" -> Option(externalAccount.accountNumber),
              "country" -> Option(externalAccount.country),
              "currency" -> Option(externalAccount.currency.iso.toLowerCase),
              "account_holder_name" -> externalAccount.accountHolderName,
              "account_holder_type" -> externalAccount.accountHolderType.map(_.id),
              "routing_number" -> externalAccount.routingNumber
            ).collect {
              case (k, Some(v)) => (k, v)
            }
            mapToPostParams(Option(map), "external_account")
          case source: BankAccountData.SourceObject =>
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
      }  ++ mapToPostParams(bankAccountInput.metadata, "metadata")

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
              val jsResult = Json.fromJson[BankAccount](jsValue)
              jsResult.fold(
                errors => {
                  throw InvalidJsonModelException(response.getStatusCode, finalUrl, Option(postFormParameters), None, jsValue, errors)
                }, bankAccount => bankAccount
              )
            }
          case Left(error) =>
            scala.util.Failure(error)
        }
      }
      
    }
  
  def get(customerId: String,
          bankAccountId: String)
         (implicit apiKey: ApiKey,
          endpoint: Endpoint): Future[Try[BankAccount]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources/$bankAccountId"

    val req = url(finalUrl).GET.as(apiKey.apiKey, "")

    Http(req).map { response =>

      parseStripeServerError(response, finalUrl, None, None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[BankAccount](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, None, None, jsValue, errors)
              }, bankAccount => bankAccount
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }

  def delete(customerId: String,
             bankAccountId: String)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[DeleteResponse]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$customerId/sources/$bankAccountId"

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
              }, deleteResponse => deleteResponse
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }

  def list(customerId: String,
           bankAccountListInput: BankAccountListInput,
           includeTotalCount: Boolean)
          (implicit apiKey: ApiKey,
           endpoint: Endpoint): Future[Try[BankAccountList]] = {
    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl = if (includeTotalCount)
        "/include[]=total_count"
      else
        ""

      val baseUrl = endpoint.url + s"/v1/customers/$customerId/sources$totalCountUrl"

      (baseUrl ?
        ("object" -> "bank_account") ?
        ("ending_before" -> bankAccountListInput.endingBefore) ?
        ("limit" -> bankAccountListInput.limit.map(_.toString)) ?
        ("starting_after" -> bankAccountListInput.startingAfter)
        ).toString()
    }

    val req = url(finalUrl).GET.as(apiKey.apiKey, "")

    Http(req).map { response =>

      parseStripeServerError(response, finalUrl, None, None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[BankAccountList](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, None, None, jsValue, errors)
              }, bankAccountList => bankAccountList
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }

  }

}
