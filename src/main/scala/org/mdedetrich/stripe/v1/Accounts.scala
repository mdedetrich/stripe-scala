package org.mdedetrich.stripe.v1

import java.time.{LocalDate, OffsetDateTime}

import com.typesafe.scalalogging.LazyLogging
import enumeratum.{Enum, EnumEntry, EnumFormats}
import org.mdedetrich.stripe.v1.BankAccounts.BankAccountData
import org.mdedetrich.stripe.v1.BankAccounts.BankAccountData.ExternalAccount
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.v1.Shippings.Address
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

object Accounts extends LazyLogging {

  // TOS acceptance
  case class TosAcceptance(
    date: Option[OffsetDateTime],
    ip: Option[String]
  )

  implicit val tosAcceptanceReads: Reads[TosAcceptance] = (
      (__ \ "date").readNullable[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "ip").readNullable[String]
    ).tupled.map((TosAcceptance.apply _).tupled)

  implicit val tosAcceptanceWrites: Writes[TosAcceptance] = Json.writes[TosAcceptance]


  // Legal entity type
  sealed abstract class LegalEntityType(val value: String) extends EnumEntry {
    override val entryName = value
  }

  object LegalEntityType extends Enum[LegalEntityType] {
    val values = findValues
    case object Individual extends LegalEntityType("individual")
    case object Company extends LegalEntityType("company")
    implicit val legalEntityTypeFormats = EnumFormats.formats(LegalEntityType, insensitive = true)
  }

  // DOB
  implicit val stripeLocalDateReads = new Reads[Option[LocalDate]] {
    override def reads(json: JsValue): JsResult[Option[LocalDate]] = json match {
      case JsObject(map) =>
        val filtered = map.collect({ case (key, JsNumber(value)) => (key, value.intValue()) })
        val localDate = for {
          year <- filtered.get("year")
          month <- filtered.get("month")
          day <- filtered.get("day")
        } yield {
          LocalDate.of(year, month, day)
        }
        JsSuccess(localDate)
      case _ => JsError(s"Cannot read $json into a date.")
    }
  }

  // Legal entity
  case class LegalEntity(
    address: Address,
    `type`: Option[LegalEntityType],
    businessName: Option[String],
    firstName: Option[String],
    lastName: Option[String],
    dob: Option[LocalDate],
    tosAggreement: Option[TosAcceptance]
  )

  object LegalEntity {
    def default = LegalEntity(Address.default, None, None, None, None, None, None)
  }

  implicit val legalEntityReads: Reads[LegalEntity] = (
      (__ \ "address").read[Address] ~
      (__ \ "type").readNullable[LegalEntityType] ~
      (__ \ "business_name").readNullable[String] ~
      (__ \ "first_name").readNullable[String] ~
      (__ \ "last_name").readNullable[String] ~
      (__ \ "dob").read[Option[LocalDate]](stripeLocalDateReads) ~
      (__ \ "tos_acceptance").readNullable[TosAcceptance]
    ).tupled.map((LegalEntity.apply _).tupled)

  implicit val legalEntityWrites: Writes[LegalEntity] = Json.writes[LegalEntity]

  //
  // Verification
  //
  case class Verification(
    disabledReason: Option[String],
    dueBy: Option[OffsetDateTime],
    fieldsNeeded: Seq[String]
  )

  implicit val verificationReads: Reads[Verification] = (
      (__ \ "disabled_reason").readNullable[String] ~
      (__ \ "due_by").readNullable[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "fields_needed").read[Seq[String]]
    ).tupled.map((Verification.apply _).tupled)

  implicit val verificationWrites: Writes[Verification] = Json.writes[Verification]

  //
  // Account
  //
  case class Account( id: String,
                      chargesEnabled: Boolean,
                      country: String,
                      debitNegativeBalances: Boolean,
                      transfersEnabled: Boolean,
                      defaultCurrency: Currency,
                      detailsSubmitted: Boolean,
                      externalAccounts: PaymentSourceList,
                      legalEntity: LegalEntity,
                      verification: Verification
                    ) extends StripeObject

  implicit val accountReads: Reads[Account] = (
      (__ \ "id").read[String] ~
      (__ \ "charges_enabled").read[Boolean] ~
      (__ \ "country").read[String] ~
      (__ \ "debit_negative_balances").read[Boolean] ~
      (__ \ "transfers_enabled").read[Boolean] ~
      (__ \ "default_currency").read[Currency] ~
      (__ \ "details_submitted").read[Boolean] ~
      (__ \ "external_accounts").read[PaymentSourceList] ~
      (__ \ "legal_entity").read[LegalEntity] ~
      (__ \ "verification").read[Verification]
    ).tupled.map((Account.apply _).tupled)

  implicit val accountWrites: Writes[Account] = Json.writes[Account]

  //
  // Account input
  //
  case class AccountInput(
    managed: Boolean
  )

  object AccountInput {
    def default: AccountInput = AccountInput(
        false
    )
  }

  //
  // Account update
  //
  case class AccountUpdate(
    legalEntity: Option[LegalEntity],
    externalAccount: Option[BankAccountData.Source.Object],
    defaultCurrency: Option[Currency],
    tosAcceptance: Option[TosAcceptance]
  )

  object AccountUpdate {
    def default = AccountUpdate(None, None, None, None)
  }

  implicit val accountUpdatePostParams = new PostParams[AccountUpdate] {
    override def toMap(update: AccountUpdate): Map[String, String] = {
      val postParams: Map[String, Option[String]] = Map(
        "default_currency" -> update.defaultCurrency.map(_.iso),
        "legal_entity[first_name]" -> update.legalEntity.flatMap(_.firstName),
        "legal_entity[last_name]" -> update.legalEntity.flatMap(_.lastName),
        "legal_entity[type]" -> update.legalEntity.flatMap(_.`type`.map(_.entryName)),
        "legal_entity[address][line1]" -> update.legalEntity.flatMap(_.address.line1),
        "legal_entity[address][line2]" -> update.legalEntity.flatMap(_.address.line2),
        "legal_entity[address][postal_code]" -> update.legalEntity.flatMap(_.address.postalCode),
        "legal_entity[address][city]" -> update.legalEntity.flatMap(_.address.city),
        "legal_entity[address][country]" -> update.legalEntity.flatMap(_.address.country),
        "legal_entity[dob][year]" -> update.legalEntity.flatMap(_.dob.map(_.getYear.toString)),
        "legal_entity[dob][month]" -> update.legalEntity.flatMap(_.dob.map(_.getMonthValue.toString)),
        "legal_entity[dob][day]" -> update.legalEntity.flatMap(_.dob.map(_.getDayOfMonth.toString)),
        "tos_acceptance[date]" -> update.tosAcceptance.flatMap(_.date.map(d => d.toEpochSecond.toString)),
        "tos_acceptance[ip]" -> update.tosAcceptance.flatMap(_.ip),
        "external_account[object]" -> update.externalAccount.map(_ => "bank_account"),
        "external_account[account_number]" -> update.externalAccount.map(_.accountNumber),
        "external_account[country]" -> update.externalAccount.map(_.country),
        "external_account[currency]" -> update.externalAccount.map(_.currency.iso)
      )
      postParams.collect({ case (key, Some(value)) => (key, value) })
    }
  }

  //
  // Operations
  //

  def create(accountInput: AccountInput)(
      idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[Account]] = {

    val postFormParameters: Map[String, String] = Map(
      "managed" -> accountInput.managed.toString
    )

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/accounts"

    createRequestPOST[Account](
        finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def update(id: String, update: AccountUpdate)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[Account]] = {
    val finalUrl = endpoint.url + s"/v1/accounts/$id"

    val params = PostParams.toPostParams(update)

    createRequestPOST[Account](finalUrl, params, idempotencyKey, logger)
  }

  def get(id: String)(
      implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[Account]] = {
    val finalUrl = endpoint.url + s"/v1/accounts/$id"

    createRequestGET[Account](finalUrl, logger)
  }

  def delete(id: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[DeleteResponse]] = {
    val finalUrl = endpoint.url + s"/v1/accounts/$id"

    createRequestDELETE(finalUrl, idempotencyKey, logger)
  }
}
