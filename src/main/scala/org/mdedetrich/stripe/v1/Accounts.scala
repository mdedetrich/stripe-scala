package org.mdedetrich.stripe.v1

import java.time.{LocalDate, OffsetDateTime}

import com.typesafe.scalalogging.LazyLogging
import enumeratum.{Enum, EnumEntry, EnumFormats}
import org.mdedetrich.stripe.v1.BankAccounts.BankAccountData
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

  implicit val tosAcceptancePostParams: PostParams[TosAcceptance] = new PostParams[TosAcceptance] {
    override def toMap(t: TosAcceptance): Map[String, String] = flatten(Map(
      "date" -> t.date.map(d => d.toEpochSecond.toString),
      "ip" -> t.ip
    ))
  }

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
                      metadata: Map[String, String],
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
      (__ \ "metadata").read[Map[String, String]] ~
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

  implicit val legalEntityPostParams = new PostParams[LegalEntity] {
    override def toMap(legalEntity: LegalEntity): Map[String, String] = {
      val postParams = Map(
        "first_name" -> legalEntity.firstName,
        "last_name" -> legalEntity.lastName,
        "type" -> legalEntity.`type`.map(_.entryName),
        "address[line1]" -> legalEntity.address.line1,
        "address[line2]" -> legalEntity.address.line2,
        "address[postal_code]" -> legalEntity.address.postalCode,
        "address[city]" -> legalEntity.address.city,
        "address[country]" -> legalEntity.address.country,
        "dob[year]" -> legalEntity.dob.map(_.getYear.toString),
        "dob[month]" -> legalEntity.dob.map(_.getMonthValue.toString),
        "dob[day]" -> legalEntity.dob.map(_.getDayOfMonth.toString)
      )
      flatten(postParams)
    }
  }

  //
  // Account input
  //
  case class AccountInput(
    managed: Boolean,
    metadata: Map[String, String],
    legalEntity: Option[LegalEntity],
    tosAcceptance: Option[TosAcceptance]
  )

  object AccountInput {
    def default: AccountInput = AccountInput(
      false,
      Map.empty,
      None,
      None
    )
  }

  implicit val accountInputPostParams = new PostParams[AccountInput] {
    override def toMap(update: AccountInput): Map[String, String] =
      Map(
        "managed" -> update.managed.toString
      ) ++
        PostParams.toPostParams("metadata", update.metadata) ++
        PostParams.toPostParams("tos_acceptance", update.tosAcceptance) ++
        PostParams.toPostParams("legal_entity", update.legalEntity)
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
      val postParams = Map(
        "default_currency" -> update.defaultCurrency.map(_.iso),
        "external_account[object]" -> update.externalAccount.map(_ => "bank_account"),
        "external_account[account_number]" -> update.externalAccount.map(_.accountNumber),
        "external_account[country]" -> update.externalAccount.map(_.country),
        "external_account[currency]" -> update.externalAccount.map(_.currency.iso)
      )
      flatten(postParams) ++
        PostParams.toPostParams("tos_acceptance", update.tosAcceptance) ++
        PostParams.toPostParams("legal_entity", update.legalEntity)
    }
  }

  //
  // Operations
  //

  def create(accountInput: AccountInput)(
      idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[Account]] = {

    val postParams: Map[String, String] = PostParams.toPostParams(accountInput)

    logger.debug(s"Generated POST form parameters is $postParams")

    val finalUrl = endpoint.url + "/v1/accounts"

    createRequestPOST[Account](finalUrl, postParams, idempotencyKey, logger)
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
