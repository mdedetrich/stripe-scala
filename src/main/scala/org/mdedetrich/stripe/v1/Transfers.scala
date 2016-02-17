package org.mdedetrich.stripe.v1

import org.mdedetrich.utforsca.SealedContents
import org.joda.time.DateTime
import org.mdedetrich.stripe.v1.TransferReversals._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import BankAccounts._
import org.mdedetrich.playjson.Utils._

object Transfers {

  case class Reversals(data: List[TransferReversal],
                       hasMore: Boolean,
                       totalCount: Long,
                       url: String)

  implicit val reversalReads: Reads[Reversals] = (
    (__ \ "data").read[List[TransferReversal]] ~
      (__ \ "has_more").read[Boolean] ~
      (__ \ "total_count").read[Long] ~
      (__ \ "url").read[String]
    ) ((data, hasMore, totalCount, url) =>
    Reversals(data, hasMore, totalCount, url)
  )

  implicit val reversalWrites: Writes[Reversals] =
    Writes((reversal: Reversals) =>
      Json.obj(
        "data" -> reversal.data,
        "has_more" -> reversal.hasMore,
        "total_count" -> reversal.totalCount,
        "url" -> reversal.url
      )
    )

  sealed abstract class Type(val id: String)

  case class UnknownType(val id: String) extends Exception {
    override def getMessage = s"Unknown Transfer Type, received $id"
  }

  object Type {

    case object Card extends Type("card")

    case object BankAccount extends Type("bank_account")

    case object StripeAccount extends Type("stripe_account")

    lazy val all: Set[Type] = SealedContents.values[Type]
  }

  implicit val typeReads: Reads[Type] = Reads.of[String].map { typeId =>
    Type.all.find(_.id == typeId).getOrElse {
      throw UnknownType(typeId)
    }
  }

  implicit val typeWrites: Writes[Type] =
    Writes((`type`: Type) => JsString(`type`.id))

  sealed abstract class Status(val id: String)

  case class UnknownStatus(val id: String) extends Exception {
    override def getMessage = s"Unknown Transfer Status, received $id"
  }

  object Status {

    case object Paid extends Status("paid")

    case object Pending extends Status("pending")

    case object InTransit extends Status("in_transit")

    case object Canceled extends Status("canceled")

    case object Failed extends Status("failed")

    lazy val all: Set[Status] = SealedContents.values[Status]
  }

  implicit val statusReads: Reads[Status] = Reads.of[String].map { statusId =>
    Status.all.find(_.id == statusId).getOrElse {
      throw UnknownStatus(statusId)
    }
  }

  implicit val statusWrites: Writes[Status] =
    Writes((status: Status) => JsString(status.id))

  /**
    * Taken from https://stripe.com/docs/api#transfer_failures
    *
    * @param id
    */

  sealed abstract class FailureCode(val id: String)

  case class UnknownFailureCode(val id: String) extends Exception {
    override def getMessage = s"Unknown Failure Code, received $id"
  }

  object FailureCode {

    case object InsufficientFunds extends FailureCode("insufficient_funds")

    case object AccountClosed extends FailureCode("account_closed")

    case object NoAccount extends FailureCode("no_account")

    case object InvalidAccountNumber extends FailureCode("invalid_account_number")

    case object DebitNotAuthorized extends FailureCode("debit_not_authorized")

    case object BankOwnershipChanged extends FailureCode("bank_ownership_changed")

    case object AccountFrozen extends FailureCode("account_frozen")

    case object CouldNotProcess extends FailureCode("could_not_process")

    case object BankAccountRestricted extends FailureCode("bank_account_restricted")

    case object InvalidCurrency extends FailureCode("invalid_currency")

    lazy val all: Set[FailureCode] = SealedContents.values[FailureCode]

  }

  implicit val failureCodeReads: Reads[FailureCode] = Reads.of[String].map { failureCodeId =>
    FailureCode.all.find(_.id == failureCodeId).getOrElse {
      throw UnknownFailureCode(failureCodeId)
    }
  }

  implicit val failureCodeWrites: Writes[FailureCode] =
    Writes((failureCode: FailureCode) => JsString(failureCode.id))

  sealed abstract class SourceType(val id: String)

  case class UnknownSourceType(val id: String) extends Exception {
    override def getMessage = s"Unknown Source Type, received $id"
  }

  object SourceType {

    case object Card extends SourceType("card")

    case object AlipayAccount extends SourceType("alipay_account")

    case object BitcoinReceiver extends SourceType("bitcoin_receiver")

    lazy val all: Set[SourceType] = SealedContents.values[SourceType]
  }

  implicit val sourceTypeReads: Reads[SourceType] = Reads.of[String].map { sourceTypeId =>
    SourceType.all.find(_.id == sourceTypeId).getOrElse {
      throw UnknownSourceType(sourceTypeId)
    }
  }

  implicit val sourceTypeWrites: Writes[SourceType] =
    Writes((sourceType: SourceType) => JsString(sourceType.id))

  case class Transfer(id: String,
                      amount: BigDecimal,
                      amountReversed: BigDecimal,
                      applicationFee: BigDecimal,
                      balanceTransaction: String,
                      bankAccount: BankAccount,
                      created: DateTime,
                      currency: Currency,
                      date: DateTime,
                      description: String,
                      destination: String,
                      destinationPayment: Option[String],
                      failureCode: Option[FailureCode],
                      failureMessage: Option[String],
                      livemode: Boolean,
                      metadata: Option[Map[String, String]],
                      recipient: String,
                      reversals: Reversals,
                      reversed: Boolean,
                      sourceTransaction: String,
                      sourceType: SourceType,
                      statementDescriptor: String,
                      status: Status,
                      `type`: Type)

  // This is due to http://stackoverflow.com/questions/28167971/scala-case-having-22-fields-but-having-issue-with-play-json-in-scala-2-11-5

  private val transferReadsOne = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "amount_reversed").read[BigDecimal] ~
      (__ \ "application_fee").read[BigDecimal] ~
      (__ \ "balance_transaction").read[String] ~
      (__ \ "bank_account").read[BankAccount] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").read[Currency] ~
      (__ \ "date").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "description").read[String] ~
      (__ \ "destination").read[String] ~
      (__ \ "destination_payment").readNullable[String] ~
      (__ \ "failure_code").readNullable[FailureCode] ~
      (__ \ "failure_message").readNullable[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "recipient").read[String] ~
      (__ \ "reversals").read[Reversals] ~
      (__ \ "reversed").read[Boolean] ~
      (__ \ "source_transaction").read[String] ~
      (__ \ "source_type").read[SourceType]
    ).tupled

  private val transferReadsTwo = (
    (__ \ "statement_descriptor").read[String] ~
      (__ \ "status").read[Status] ~
      (__ \ "type").read[Type]
    ).tupled


  implicit val transferReads: Reads[Transfer] = (
    transferReadsOne ~ transferReadsTwo
    ) { (one, two) =>
    val (id, amount, amountReversed, applicationFee, balanceTransaction, bankAccount, created, currency, date, description, destination, destinationPayment, failureCode, failureMessage, livemode, metadata, recipient, reversals, reversed, sourceTransaction, sourceType) = one
    val (statementDescriptor, status, type_) = two

    Transfer(id, amount, amountReversed, applicationFee, balanceTransaction, bankAccount, created, currency, date, description, destination, destinationPayment, failureCode, failureMessage, livemode, metadata, recipient, reversals, reversed, sourceTransaction, sourceType, statementDescriptor, status, type_)
  }

  implicit val transferWrites: Writes[Transfer] =
    Writes((transfer: Transfer) =>
      Json.obj(
        "id" -> transfer.id,
        "object" -> "transfer",
        "amount" -> transfer.amount,
        "amount_reversed" -> transfer.amountReversed,
        "application_fee" -> transfer.applicationFee,
        "balance_transaction" -> transfer.balanceTransaction,
        "bank_account" -> transfer.bankAccount,
        "created" -> transfer.created.getMillis / 1000,
        "currency" -> transfer.currency,
        "date" -> transfer.date.getMillis / 1000,
        "description" -> transfer.description,
        "destination" -> transfer.destination,
        "destination_payment" -> transfer.destinationPayment,
        "failure_code" -> transfer.failureCode,
        "failure_message" -> transfer.failureMessage,
        "livemode" -> transfer.livemode,
        "metadata" -> transfer.metadata,
        "recipient" -> transfer.recipient,
        "reversals" -> transfer.reversals,
        "reversed" -> transfer.reversed,
        "source_transaction" -> transfer.sourceTransaction,
        "source_type" -> transfer.sourceType
      )
    )

  case class TransferInput(amount: BigDecimal,
                           currency: Currency,
                           destination: String,
                           description: Option[String],
                           metadata: Option[Map[String, String]],
                           sourceTransaction: Option[String],
                           statementDescriptor: Option[String],
                           sourceType: Option[SourceType]
                          )

}
