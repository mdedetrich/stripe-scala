package org.mdedetrich.stripe.v1

import java.time.format.TextStyle
import java.time.{DayOfWeek, LocalDate, OffsetDateTime}
import java.util.Locale

import akka.http.scaladsl.HttpExt
import akka.stream.Materializer
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import enumeratum.{Enum, EnumEntry}
import io.circe._
import io.circe.syntax._
import org.mdedetrich.stripe.PostParams.flatten
import org.mdedetrich.stripe.v1.BankAccounts.BankAccountData
import org.mdedetrich.stripe.v1.Shippings.Address
import org.mdedetrich.stripe.v1.defaults._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * Represents a Stripe Connect Managed Account
  *
  * @see https://stripe.com/docs/api#account
  */
object Accounts extends LazyLogging {

  final case class TosAcceptance(
      date: Option[OffsetDateTime],
      ip: Option[String]
  )

  implicit val tosAcceptancePostParams: PostParams[TosAcceptance] = PostParams.params[TosAcceptance] { t =>
    flatten(
      Map(
        "date" -> t.date.map(d => d.toEpochSecond.toString),
        "ip"   -> t.ip
      )
    )
  }

  implicit val tosAcceptanceDecoder: Decoder[TosAcceptance] =
    Decoder.forProduct2("date", "ip")(TosAcceptance.apply)
  implicit val tosAcceptanceEncoder: Encoder[TosAcceptance] =
    Encoder.forProduct2("date", "ip")(x => TosAcceptance.unapply(x).get)

  // Legal entity type
  sealed abstract class LegalEntityType(val value: String) extends EnumEntry {
    override val entryName = value
  }

  object LegalEntityType extends Enum[LegalEntityType] {
    val values = findValues

    case object Individual extends LegalEntityType("individual")
    case object Company    extends LegalEntityType("company")

    implicit val legalEntityDecoder: Decoder[LegalEntityType] = enumeratum.Circe.decoder(LegalEntityType)
    implicit val legalEntityEncoder: Encoder[LegalEntityType] = enumeratum.Circe.encoder(LegalEntityType)
  }

  /*
    This is a special case Encoder/Decoder that only applies for this Account mode. Json of the form LocalDate
    is always optional and in the form of

    "dob": {
      "day": null,
      "month": null,
      "year": null
    }

    If the value doesn't exist, else if the value exists its in the form

    "dob": {
      "day": 5,
      "month": 2,
      "year": 1979
    }

   */

  private implicit val stripeLocalDateDecoder: Decoder[Option[LocalDate]] = Decoder.instance[Option[LocalDate]] { c =>
    val baseLocalDate = for {
      obj <- c.as[JsonObject]
      filtered = obj.toMap
        .map { case (k, v) => (k, v.as[JsonNumber].toOption.flatMap(_.toInt)) }
        .collect { case (k, Some(v)) => (k, v) }
      localDate = for {
        year  <- filtered.get("year")
        month <- filtered.get("month")
        day   <- filtered.get("day")
      } yield LocalDate.of(year, month, day)
    } yield localDate

    baseLocalDate
  }

  private implicit val stripeLocalDateEncoder: Encoder[Option[LocalDate]] = Encoder.instance[Option[LocalDate]] {
    case Some(localDate) =>
      Json.obj(
        "year"  -> localDate.getYear.asJson,
        "month" -> localDate.getMonthValue.asJson,
        "day"   -> localDate.getDayOfMonth.asJson
      )
    case None =>
      Json.obj(
        "year"  -> Json.Null,
        "month" -> Json.Null,
        "day"   -> Json.Null
      )
  }

  // Legal entity
  final case class LegalEntity(
      address: Address = Address(),
      `type`: Option[LegalEntityType] = None,
      businessName: Option[String] = None,
      firstName: Option[String] = None,
      lastName: Option[String] = None,
      dob: Option[LocalDate] = None,
      tosAcceptance: Option[TosAcceptance] = None
  )

  implicit val legalEntityDecoder: Decoder[LegalEntity] = Decoder.forProduct7(
    "address",
    "type",
    "business_name",
    "first_name",
    "last_name",
    "dob",
    "tos_acceptance"
  )(LegalEntity.apply)

  implicit val legalEntityEncoder: Encoder[LegalEntity] = Encoder.forProduct7(
    "address",
    "type",
    "business_name",
    "first_name",
    "last_name",
    "dob",
    "tos_acceptance"
  )(x => LegalEntity.unapply(x).get)

  //
  // Verification
  //
  final case class Verification(
      disabledReason: Option[String],
      dueBy: Option[OffsetDateTime],
      fieldsNeeded: Seq[String]
  )

  implicit val verificationDecoder: Decoder[Verification] = Decoder.forProduct3(
    "disabled_reason",
    "due_by",
    "fields_needed"
  )(Verification.apply)

  implicit val verificationEncoder: Encoder[Verification] = Encoder.forProduct3(
    "disabled_reason",
    "due_by",
    "fields_needed"
  )(x => Verification.unapply(x).get)

  /**
    * @see https://stripe.com/docs/connect/bank-transfers#payout-information
    */
  sealed abstract class TransferInterval(value: String) extends EnumEntry {
    override val entryName = value
  }

  object TransferInterval extends Enum[TransferInterval] {
    val values = findValues

    case object Manual  extends TransferInterval("manual")
    case object Daily   extends TransferInterval("daily")
    case object Weekly  extends TransferInterval("weekly")
    case object Monthly extends TransferInterval("monthly")

    implicit val transferIntervalEncoder: Encoder[TransferInterval] = enumeratum.Circe.encoder(TransferInterval)
    implicit val transferIntervalDecoder: Decoder[TransferInterval] = enumeratum.Circe.decoder(TransferInterval)
  }

  final case class TransferSchedule(
      interval: Option[TransferInterval],
      monthlyAnchor: Option[Int],
      weeklyAnchor: Option[DayOfWeek]
  )

  implicit val dayOfWeekDecoder: Decoder[DayOfWeek] = Decoder.instance[DayOfWeek] { c =>
    for {
      day <- c.as[String]
      asDayOfWeek <- util
        .Try(DayOfWeek.valueOf(day.toUpperCase))
        .toOption
        .toRight(DecodingFailure("Could not convert to day of week", c.history))
    } yield asDayOfWeek
  }

  implicit val dayOfWeekEncoder: Encoder[DayOfWeek] =
    Encoder.instance[DayOfWeek](_.getDisplayName(TextStyle.FULL, Locale.ENGLISH).toLowerCase.asJson)

  implicit val transferScheduleDecoder: Decoder[TransferSchedule] = Decoder.forProduct3(
    "interval",
    "monthly_anchor",
    "weekly_anchor"
  )(TransferSchedule.apply)

  implicit val transferScheduleEncoder: Encoder[TransferSchedule] = Encoder.forProduct3(
    "interval",
    "monthly_anchor",
    "weekly_anchor"
  )(x => TransferSchedule.unapply(x).get)

  implicit val transferSchedulePostParams: PostParams[TransferSchedule] = PostParams.params[TransferSchedule] {
    transferSchedule =>
      flatten(
        Map(
          "interval"       -> transferSchedule.interval.map(_.entryName),
          "monthly_anchor" -> transferSchedule.monthlyAnchor.map(_.toString),
          "weekly_anchor" -> transferSchedule.weeklyAnchor
            .map(_.getDisplayName(TextStyle.FULL, Locale.ENGLISH).toLowerCase)
        )
      )
  }

  //
  // Account
  //
  final case class Account(
      id: String,
      metadata: Map[String, String],
      chargesEnabled: Boolean,
      country: String,
      debitNegativeBalances: Boolean,
      transfersEnabled: Boolean,
      transferSchedule: TransferSchedule,
      defaultCurrency: Currency,
      detailsSubmitted: Boolean,
      externalAccounts: PaymentSourceList,
      legalEntity: LegalEntity,
      verification: Verification
  ) extends StripeObject

  implicit val accountDecoder: Decoder[Account] = Decoder.forProduct12(
    "id",
    "metadata",
    "charges_enabled",
    "country",
    "debit_negative_balances",
    "transfers_enabled",
    "transfer_schedule",
    "default_currency",
    "details_submitted",
    "external_accounts",
    "legal_entity",
    "verification"
  )(Account.apply)

  implicit val accountEncoder: Encoder[Account] = Encoder.forProduct13(
    "id",
    "object",
    "metadata",
    "charges_enabled",
    "country",
    "debit_negative_balances",
    "transfers_enabled",
    "transfer_schedule",
    "default_currency",
    "details_submitted",
    "external_accounts",
    "legal_entity",
    "verification"
  )(
    x =>
      (
        x.id,
        "account",
        x.metadata,
        x.chargesEnabled,
        x.country,
        x.debitNegativeBalances,
        x.transfersEnabled,
        x.transferSchedule,
        x.defaultCurrency,
        x.detailsSubmitted,
        x.externalAccounts,
        x.legalEntity,
        x.verification
      )
  )

  implicit val legalEntityPostParams: PostParams[LegalEntity] = PostParams.params[LegalEntity] { legalEntity =>
    val postParams = Map(
      "first_name"           -> legalEntity.firstName,
      "last_name"            -> legalEntity.lastName,
      "type"                 -> legalEntity.`type`.map(_.entryName),
      "address[line1]"       -> legalEntity.address.line1,
      "address[line2]"       -> legalEntity.address.line2,
      "address[postal_code]" -> legalEntity.address.postalCode,
      "address[city]"        -> legalEntity.address.city,
      "address[country]"     -> legalEntity.address.country,
      "dob[year]"            -> legalEntity.dob.map(_.getYear.toString),
      "dob[month]"           -> legalEntity.dob.map(_.getMonthValue.toString),
      "dob[day]"             -> legalEntity.dob.map(_.getDayOfMonth.toString)
    )
    flatten(postParams)
  }

  //
  // Account input
  //
  final case class AccountInput(
      managed: Boolean = false,
      metadata: Map[String, String] = Map.empty,
      legalEntity: Option[LegalEntity] = None,
      transferSchedule: Option[TransferSchedule] = None,
      tosAcceptance: Option[TosAcceptance] = None
  )

  implicit val accountInputPostParams: PostParams[AccountInput] = PostParams.params[AccountInput] { update =>
    Map(
      "managed" -> update.managed.toString
    ) ++
      PostParams.toPostParams("metadata", update.metadata) ++
      PostParams.toPostParams("transfer_schedule", update.transferSchedule) ++
      PostParams.toPostParams("tos_acceptance", update.tosAcceptance) ++
      PostParams.toPostParams("legal_entity", update.legalEntity)
  }

  //
  // Account update
  //
  final case class AccountUpdate(
      legalEntity: Option[LegalEntity] = None,
      externalAccount: Option[BankAccountData.Source] = None,
      defaultCurrency: Option[Currency] = None,
      tosAcceptance: Option[TosAcceptance] = None,
      transferSchedule: Option[TransferSchedule] = None
  )

  implicit val accountUpdatePostParams: PostParams[AccountUpdate] = PostParams.params[AccountUpdate] { update =>
    val defaultCurrency = Map("default_currency" -> update.defaultCurrency.map(_.iso))

    val externalAccount = update.externalAccount
      .map({
        case o: BankAccountData.Source.Object    => PostParams.toPostParams("external_account", o)
        case token: BankAccountData.Source.Token => Map("external_account" -> token.id)
      })
      .getOrElse(Map.empty)

    flatten(defaultCurrency) ++ externalAccount ++
      PostParams.toPostParams("tos_acceptance", update.tosAcceptance) ++
      PostParams.toPostParams("legal_entity", update.legalEntity)
  }

  //
  // Operations
  //

  def create(accountInput: AccountInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Account]] = {

    val postParams: Map[String, String] = PostParams.toPostParams(accountInput)

    logger.debug(s"Generated POST form parameters is $postParams")

    val finalUrl = endpoint.url + "/v1/accounts"

    createRequestPOST[Account](finalUrl, postParams, idempotencyKey, logger)
  }

  def update(id: String, update: AccountUpdate)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Account]] = {
    val finalUrl = endpoint.url + s"/v1/accounts/$id"

    val params = PostParams.toPostParams(update)

    createRequestPOST[Account](finalUrl, params, idempotencyKey, logger)
  }

  def get(id: String)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Account]] = {
    val finalUrl = endpoint.url + s"/v1/accounts/$id"

    createRequestGET[Account](finalUrl, logger)
  }

  def delete(id: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[DeleteResponse]] = {
    val finalUrl = endpoint.url + s"/v1/accounts/$id"

    createRequestDELETE(finalUrl, idempotencyKey, logger)
  }
}
