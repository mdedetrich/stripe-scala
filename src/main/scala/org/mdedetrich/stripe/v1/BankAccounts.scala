package org.mdedetrich.stripe.v1

import enumeratum._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

/**
  * Created by matthewdedetrich on 4/02/2016.
  */
object BankAccounts {

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
}
