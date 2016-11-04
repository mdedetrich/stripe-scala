package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime
import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.mdedetrich.stripe.{ApiKey, Endpoint}
import org.mdedetrich.stripe.v1.Transfers._
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.concurrent.Future
import scala.util.Try

object Balances extends LazyLogging {

  sealed abstract class FeeType(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object FeeType extends Enum[FeeType] {

    val values = findValues

    case object ApplicationFee extends FeeType("application_fee")

    case object StripeFee extends FeeType("stripe_fee")

    case object Tax extends FeeType("tax")
  }

  implicit val feeTypeFormats =
    EnumFormats.formats(FeeType, insensitive = true)

  case class FeeDetails(amount: BigDecimal,
                        application: String,
                        currency: Currency,
                        description: String,
                        `type`: FeeType)

  implicit val feeDetailReads: Reads[FeeDetails] = (
    (__ \ "amount").read[BigDecimal] ~
      (__ \ "application").read[String] ~
      (__ \ "currency").read[Currency] ~
      (__ \ "description").read[String] ~
      (__ \ "type").read[FeeType]
  )((amount, application, currency, description, `type`) =>
    FeeDetails(amount, application, currency, description, `type`))

  implicit val feeDetailWrites: Writes[FeeDetails] = Writes(
    (feeDetails: FeeDetails) =>
      Json.obj(
        "amount"      -> feeDetails.amount,
        "application" -> feeDetails.application,
        "currency"    -> feeDetails.currency,
        "description" -> feeDetails.description,
        "type"        -> feeDetails.`type`
    ))

  sealed abstract class Type(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Type extends Enum[Type] {

    val values = findValues

    case object Charge extends Type("charge")

    case object Refund extends Type("refund")

    case object Adjustment extends Type("adjustment")

    case object ApplicationFee extends Type("application_fee")

    case object ApplicationFeeRefund extends Type("application_fee_refund")

    case object Transfer extends Type("transfer")

    case object TransferCancel extends Type("transfer_cancel")

    case object TransferRefund extends Type("transfer_refund")

    case object TransferFailure extends Type("transfer_failure")
  }

  implicit val typeFormats = EnumFormats.formats(Type, insensitive = true)

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {

    val values = findValues

    case object Available extends Status("available")

    case object Pending extends Status("pending")
  }

  implicit val statusFormats = EnumFormats.formats(Status, insensitive = true)

  /**
    * @see https://stripe.com/docs/api#balance_transaction_object
    * @param id
    * @param amount           Gross amount of the transaction, in cents.
    * @param availableOn      The date the transaction’s net funds will become available in the Stripe balance.
    * @param created
    * @param currency
    * @param description
    * @param fee              Fees (in cents) paid for this transaction
    * @param feeDetails       Detailed breakdown of fees (in cents) paid for this transaction
    * @param net              Net amount of the transaction, in cents.
    * @param source           The Stripe object this transaction is related to.
    * @param sourcedTransfers The transfers (if any) for which source is a sourceTransaction.
    * @param status           If the transaction’s net funds are available in the Stripe balance yet.
    *                         Either [[Status.Available]] or [[Status.Pending]].
    * @param `type`           Transaction type: [[Type]]
    */
  case class BalanceTransaction(id: String,
                                amount: BigDecimal,
                                availableOn: OffsetDateTime,
                                created: OffsetDateTime,
                                currency: Currency,
                                description: String,
                                fee: BigDecimal,
                                feeDetails: List[FeeDetails],
                                net: BigDecimal,
                                source: String,
                                sourcedTransfers: TransferList,
                                status: Option[Status],
                                `type`: Type)
      extends StripeObject

  implicit val balanceTransactionReads: Reads[BalanceTransaction] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "available_on").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "description").read[String] ~
      (__ \ "fee").read[BigDecimal] ~
      (__ \ "fee_details").read[List[FeeDetails]] ~
      (__ \ "net").read[BigDecimal] ~
      (__ \ "source").read[String] ~
      (__ \ "sourced_transfers").read[TransferList] ~
      (__ \ "status").readNullable[Status] ~
      (__ \ "type").read[Type]
  ).tupled.map((BalanceTransaction.apply _).tupled)

  implicit val balanceTransactionWrites: Writes[BalanceTransaction] = Writes(
    (balanceTransaction: BalanceTransaction) =>
      Json.obj(
        "id"                -> balanceTransaction.id,
        "object"            -> "balance_transaction",
        "amount"            -> balanceTransaction.amount,
        "currency"          -> balanceTransaction.currency,
        "available_on"      -> Json.toJson(balanceTransaction.availableOn)(stripeDateTimeWrites),
        "created"           -> Json.toJson(balanceTransaction.created)(stripeDateTimeWrites),
        "description"       -> balanceTransaction.description,
        "fee"               -> balanceTransaction.fee,
        "fee_details"       -> balanceTransaction.feeDetails,
        "net"               -> balanceTransaction.net,
        "source"            -> balanceTransaction.source,
        "sourced_transfers" -> balanceTransaction.sourcedTransfers,
        "status"            -> balanceTransaction.status,
        "type"              -> balanceTransaction.`type`
    ))

  case class SourceTypes(card: BigDecimal, bankAccount: BigDecimal, bitcoinReceiver: BigDecimal)

  implicit val sourceTypesReads: Reads[SourceTypes] = (
    (__ \ "card").read[BigDecimal] ~
      (__ \ "bank_account").read[BigDecimal] ~
      (__ \ "bitcoin_receiver").read[BigDecimal]
  ).tupled.map((SourceTypes.apply _).tupled)

  implicit val sourceTypesWrites: Writes[SourceTypes] = Writes(
    (sourceTypes: SourceTypes) =>
      Json.obj(
        "card"             -> sourceTypes.card,
        "bank_account"     -> sourceTypes.bankAccount,
        "bitcoin_receiver" -> sourceTypes.bitcoinReceiver
    ))

  case class BalanceFund(currency: Currency, amount: BigDecimal, sourceTypes: SourceTypes)

  implicit val balanceFundReads: Reads[BalanceFund] = (
    (__ \ "currency").read[Currency] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "source_types").read[SourceTypes]
  ).tupled.map((BalanceFund.apply _).tupled)

  implicit val balanceFundWrites: Writes[BalanceFund] = Writes(
    (balanceFund: BalanceFund) =>
      Json.obj(
        "currency"     -> balanceFund.currency,
        "amount"       -> balanceFund.amount,
        "source_types" -> balanceFund.sourceTypes
    ))

  /**
    * @see https://stripe.com/docs/api#balance_object
    * @param available Funds that are available to be paid out automatically by Stripe or explicitly via the transfers API.
    *                  The available balance for each currency and payment type can be found in the sourceTypes property.
    * @param livemode
    * @param pending   Funds that are not available in the balance yet, due to the 7-day rolling pay cycle.
    *                  The pending balance for each currency and payment type can be found in the sourceTypes property
    */
  case class Balance(available: List[BalanceFund], livemode: Boolean, pending: List[BalanceFund])

  implicit val balanceReads: Reads[Balance] = (
    (__ \ "available").read[List[BalanceFund]] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "pending").read[List[BalanceFund]]
  ).tupled.map((Balance.apply _).tupled)

  implicit val balanceWrites: Writes[Balance] = Writes(
    (balance: Balance) =>
      Json.obj(
        "available" -> balance.available,
        "livemode"  -> balance.livemode,
        "pending"   -> balance.pending
    ))

  /**
    * @see https://stripe.com/docs/api#retrieve_balance
    * @param apiKey
    * @param endpoint
    * @return
    */
  def get(implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[Balance]] = {
    val finalUrl = endpoint.url + s"/v1/balance"

    createRequestGET[Balance](finalUrl, logger)
  }

  /**
    * @see https://stripe.com/docs/api#retrieve_balance_transaction
    * @param id
    * @param apiKey
    * @param endpoint
    * @return
    */
  def getBalanceTransaction(id: String)(implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[BalanceTransaction]] = {
    val finalUrl = endpoint.url + s"/v1/balance/history/$id"

    createRequestGET[BalanceTransaction](finalUrl, logger)
  }

  /**
    * @see https://stripe.com/docs/api#balance_history
    * @param availableOn   A filter on the list based on the object [[availableOn]] field.
    *                      The value can be a string with an integer Unix timestamp,
    *                      or it can be a dictionary with the following options:
    * @param created       A filter on the list based on the object [[created]] field.
    *                      The value can be a string with an integer Unix timestamp,
    *                      or it can be a dictionary with the following options:
    * @param currency
    * @param endingBefore  A cursor for use in pagination. [[endingBefore]] is an
    *                      object ID that defines your place in the list. For instance,
    *                      if you make a list request and receive 100 objects,
    *                      starting with obj_bar, your subsequent call can include
    *                      ending_before=obj_bar in order to fetch the previous page of the list.
    * @param limit         A limit on the number of objects to be returned.
    *                      Limit can range between 1 and 100 items.
    * @param source        Only returns transactions that are related to the specified Stripe
    *                      object ID (e.g. filtering by a charge ID will return all related charge transactions).
    * @param startingAfter A cursor for use in pagination. [[startingAfter]] is an
    *                      object ID that defines your place in the list. For instance, if you make a
    *                      list request and receive 100 objects, ending with obj_foo, your subsequent
    *                      call can include [[startingAfter]]=obj_foo in order to fetch the next page of the list.
    * @param transfer      For automatic Stripe transfers only, only returns transactions that were transferred out on
    *                      the specified transfer ID.
    * @param `type`        Only returns transactions of the given type.
    *                      One of: [[Type.Charge]], [[Type.Adjustment]], [[Type.ApplicationFee]],
    *                      [[Type.ApplicationFeeRefund]], [[Type.Transfer]], or [[Type.TransferFailure]]
    */
  case class BalanceHistoryListInput(availableOn: Option[ListFilterInput],
                                     created: Option[ListFilterInput],
                                     currency: Option[Currency],
                                     endingBefore: Option[String],
                                     limit: Option[Long],
                                     source: Option[String],
                                     startingAfter: Option[String],
                                     transfer: Option[Boolean],
                                     `type`: Option[Type])

  object BalanceHistoryListInput {
    def default: BalanceHistoryListInput = BalanceHistoryListInput(
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )
  }

  case class BalanceTransactionList(override val url: String,
                                    override val hasMore: Boolean,
                                    override val data: List[BalanceTransaction],
                                    override val totalCount: Option[Long])
      extends Collections.List[BalanceTransaction](url, hasMore, data, totalCount)

  object BalanceTransactionList extends Collections.ListJsonMappers[BalanceTransaction] {
    implicit val balanceTransactionListReads: Reads[BalanceTransactionList] =
      listReads.tupled.map((BalanceTransactionList.apply _).tupled)

    implicit val balanceTransactionListWrites: Writes[BalanceTransactionList] =
      listWrites
  }

  /**
    * @see https://stripe.com/docs/api#balance_history
    */
  def listBalanceHistory(balanceHistoryListInput: BalanceHistoryListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[BalanceTransactionList]] = {
    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/balance/history$totalCountUrl"

      val created: com.netaporter.uri.Uri =
        (balanceHistoryListInput.created, balanceHistoryListInput.availableOn) match {
          case (Some(createdInput), Some(availableOnInput)) =>
            listFilterInputToUri(availableOnInput,
                                 listFilterInputToUri(createdInput, baseUrl, "created"),
                                 "available_on")
          case (Some(createdInput), None) =>
            listFilterInputToUri(createdInput, baseUrl, "created")
          case (None, Some(availableInput)) =>
            listFilterInputToUri(availableInput, baseUrl, "available_on")
          case _ => baseUrl
        }

      (created ?
        ("currency"       -> balanceHistoryListInput.currency.map(_.iso.toLowerCase)) ?
        ("ending_before"  -> balanceHistoryListInput.endingBefore) ?
        ("limit"          -> balanceHistoryListInput.limit.map(_.toString)) ?
        ("source"         -> balanceHistoryListInput.source) ?
        ("starting_after" -> balanceHistoryListInput.startingAfter) ?
        ("transfer"       -> balanceHistoryListInput.transfer) ?
        ("type"           -> balanceHistoryListInput.`type`.map(_.id))).toString()
    }

    createRequestGET[BalanceTransactionList](finalUrl, logger)
  }
}
