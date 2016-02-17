package org.mdedetrich.stripe.v1

import org.mdedetrich.utforsca.SealedContents
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

/**
  * Created by matthewdedetrich on 4/02/2016.
  */
object BankAccounts {

  sealed abstract class AccountHolderType(val id: String)

  case class UnknownAccountHolderType(val id: String) extends Exception {
    override def getMessage = s"Unknown Account Holder Type, received $id"
  }

  object AccountHolderType {

    case object Individual extends AccountHolderType("individual")

    case object Company extends AccountHolderType("company")

    lazy val all: Set[AccountHolderType] = SealedContents.values[AccountHolderType]
  }

  implicit val accountHolderTypeReads: Reads[AccountHolderType] = Reads.of[String].map { accountHolderTypeId =>
    AccountHolderType.all.find(_.id == accountHolderTypeId).getOrElse {
      throw UnknownAccountHolderType(accountHolderTypeId)
    }
  }

  implicit val accountHolderTypeWrites: Writes[AccountHolderType] =
    Writes((accountHolderType: AccountHolderType) => JsString(accountHolderType.id))

  sealed abstract class Status(val id: String)

  case class UnknownStatus(val id: String) extends Exception {
    override def getMessage = s"Unknown Bank Account Status, received $id"
  }

  object Status {

    case object New extends Status("new")

    case object Validated extends Status("validated")

    case object Verified extends Status("verified")

    case object VerificationFailed extends Status("verification_failed")

    case object Errored extends Status("errored")

    lazy val all: Set[Status] = SealedContents.values[Status]
  }

  implicit val statusReads: Reads[Status] = Reads.of[String].map { statusId =>
    Status.all.find(_.id == statusId).getOrElse {
      throw UnknownStatus(statusId)
    }
  }

  implicit val statusWrites: Writes[Status] =
    Writes((status: Status) => JsString(status.id))

  case class BankAccount(id: String,
                         account: String,
                         accountHolderType: AccountHolderType,
                         bankName: String,
                         country: String,
                         currency: Currency,
                         defaultForCurrency: Boolean,
                         fingerprint: String,
                         last4: String,
                         metadata: Option[Map[String,String]],
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
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String,String]] ~
      (__ \ "name").read[String] ~
      (__ \ "routing_number").read[String] ~
      (__ \ "status").read[Status]
    ).tupled.map(BankAccount.tupled)


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
