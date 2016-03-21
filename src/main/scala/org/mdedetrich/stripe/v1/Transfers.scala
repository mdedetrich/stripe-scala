package org.mdedetrich.stripe.v1

import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.stripe.v1.TransferReversals._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import BankAccounts._
import org.mdedetrich.playjson.Utils._

object Transfers {

  case class TransferReversalList(override val url: String,
                                  override val hasMore: Boolean,
                                  override val data: List[TransferReversal],
                                  override val totalCount: Option[Long]
                                 ) extends Collections.List[TransferReversal](
    url, hasMore, data, totalCount
  )

  object TransferReversalList extends Collections.ListJsonMappers[TransferReversal] {
    implicit val transferReversalListReads: Reads[TransferReversalList] =
      listReads.tupled.map((TransferReversalList.apply _).tupled)

    implicit val transferReversalWrites: Writes[TransferReversalList] =
      listWrites
  }

  sealed abstract class Type(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Type extends Enum[Type] {

    val values = findValues

    case object Card extends Type("card")

    case object BankAccount extends Type("bank_account")

    case object StripeAccount extends Type("stripe_account")

  }

  implicit val typeFormats = EnumFormats.formats(Type, insensitive = true)

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {

    val values = findValues

    case object Paid extends Status("paid")

    case object Pending extends Status("pending")

    case object InTransit extends Status("in_transit")

    case object Canceled extends Status("canceled")

    case object Failed extends Status("failed")

  }

  implicit val statusFormats = EnumFormats.formats(Status, insensitive = true)

  /**
    * @see https://stripe.com/docs/api#transfer_failures
    *
    * @param id
    */

  sealed abstract class FailureCode(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object FailureCode extends Enum[FailureCode] {

    val values = findValues

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

  }

  implicit val failureCodeFormats = EnumFormats.formats(FailureCode, insensitive = true)

  sealed abstract class SourceType(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object SourceType extends Enum[SourceType] {

    val values = findValues

    case object Card extends SourceType("card")

    case object AlipayAccount extends SourceType("alipay_account")

    case object BitcoinReceiver extends SourceType("bitcoin_receiver")

  }

  implicit val sourceTypeFormats = EnumFormats.formats(SourceType, insensitive = true)

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
                      reversals: TransferReversalList,
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
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "date").read[DateTime](stripeDateTimeReads) ~
      (__ \ "description").read[String] ~
      (__ \ "destination").read[String] ~
      (__ \ "destination_payment").readNullable[String] ~
      (__ \ "failure_code").readNullable[FailureCode] ~
      (__ \ "failure_message").readNullable[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "recipient").read[String] ~
      (__ \ "reversals").read[TransferReversalList] ~
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
        "created" -> Json.toJson(transfer.created)(stripeDateTimeWrites),
        "currency" -> transfer.currency,
        "date" -> Json.toJson(transfer.date)(stripeDateTimeWrites),
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


  case class TransferList(override val url: String,
                          override val hasMore: Boolean,
                          override val data: List[Transfers.Transfer],
                          override val totalCount: Option[Long]
                         ) extends Collections.List[Transfer](
    url, hasMore, data, totalCount
  )

  object TransferList extends Collections.ListJsonMappers[Transfer] {
    implicit val transferListReads: Reads[TransferList] =
      listReads.tupled.map((TransferList.apply _).tupled)

    implicit val transferWrites: Writes[TransferList] =
      listWrites
  }

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
