package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import com.typesafe.scalalogging.LazyLogging
import defaults._
import enumeratum._
import io.circe.{Decoder, Encoder}
import org.mdedetrich.stripe.v1.Transfers._
import org.mdedetrich.stripe.v1.defaults._
import org.mdedetrich.stripe.{ApiKey, Endpoint, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Balances extends LazyLogging {

  sealed abstract class FeeType(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object FeeType extends Enum[FeeType] {

    val values = findValues

    case object ApplicationFee extends FeeType("application_fee")
    case object StripeFee      extends FeeType("stripe_fee")
    case object Tax            extends FeeType("tax")

    implicit val feeTypeDecoder: Decoder[FeeType] = enumeratum.Circe.decoder(FeeType)
    implicit val feeTypeEncoder: Encoder[FeeType] = enumeratum.Circe.encoder(FeeType)
  }

  case class FeeDetails(amount: BigDecimal,
                        application: String,
                        currency: Currency,
                        description: String,
                        `type`: FeeType)

  implicit val feeDetailsDecoder: Decoder[FeeDetails] = Decoder.forProduct5(
    "amount",
    "application",
    "currency",
    "description",
    "type"
  )(FeeDetails.apply)

  implicit val feeDetailsEncoder: Encoder[FeeDetails] = Encoder.forProduct5(
    "amount",
    "application",
    "currency",
    "description",
    "type"
  )(x => FeeDetails.unapply(x).get)

  sealed abstract class Type(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Type extends Enum[Type] {
    val values = findValues

    case object Charge               extends Type("charge")
    case object Refund               extends Type("refund")
    case object Adjustment           extends Type("adjustment")
    case object ApplicationFee       extends Type("application_fee")
    case object ApplicationFeeRefund extends Type("application_fee_refund")
    case object Transfer             extends Type("transfer")
    case object TransferCancel       extends Type("transfer_cancel")
    case object TransferRefund       extends Type("transfer_refund")
    case object TransferFailure      extends Type("transfer_failure")

    implicit val balanceTypeDecoder: Decoder[Type] = enumeratum.Circe.decoder(Type)
    implicit val balanceTypeEncoder: Encoder[Type] = enumeratum.Circe.encoder(Type)
  }

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {
    val values = findValues

    case object Available extends Status("available")
    case object Pending   extends Status("pending")

    implicit val balanceStatusDecoder: Decoder[Status] = enumeratum.Circe.decoder(Status)
    implicit val balanceStatusEncoder: Encoder[Status] = enumeratum.Circe.encoder(Status)
  }

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

  implicit val balanceTransactionDecoder: Decoder[BalanceTransaction] = Decoder.forProduct13(
    "id",
    "amount",
    "available_on",
    "created",
    "currency",
    "description",
    "fee",
    "fee_details",
    "net",
    "source",
    "sourced_transfers",
    "status",
    "type"
  )(BalanceTransaction.apply)

  implicit val balanceTransactionEncoder: Encoder[BalanceTransaction] = Encoder.forProduct14(
    "id",
    "object",
    "amount",
    "available_on",
    "created",
    "currency",
    "description",
    "fee",
    "fee_details",
    "net",
    "source",
    "sourced_transfers",
    "status",
    "type"
  )(
    x =>
      (x.id,
       "balance_transaction",
       x.amount,
       x.availableOn,
       x.created,
       x.currency,
       x.description,
       x.fee,
       x.feeDetails,
       x.net,
       x.source,
       x.sourcedTransfers,
       x.status,
       x.`type`))

  case class SourceTypes(card: Option[BigDecimal],
                         bankAccount: Option[BigDecimal],
                         bitcoinReceiver: Option[BigDecimal])

  implicit val sourceTypesDecoder: Decoder[SourceTypes] = Decoder.forProduct3(
    "card",
    "bank_account",
    "bitcoin_receiver"
  )(SourceTypes.apply)

  implicit val sourceTypesEncoder: Encoder[SourceTypes] = Encoder.forProduct3(
    "card",
    "bank_account",
    "bitcoin_receiver"
  )(x => SourceTypes.unapply(x).get)

  case class BalanceFund(currency: Currency, amount: BigDecimal, sourceTypes: SourceTypes)

  implicit val balanceFundDecoder: Decoder[BalanceFund] = Decoder.forProduct3(
    "currency",
    "amount",
    "source_types"
  )(BalanceFund.apply)

  implicit val balanceFundEncoder: Encoder[BalanceFund] = Encoder.forProduct3(
    "currency",
    "amount",
    "source_types"
  )(x => BalanceFund.unapply(x).get)

  /**
    * @see https://stripe.com/docs/api#balance_object
    * @param available Funds that are available to be paid out automatically by Stripe or explicitly via the transfers API.
    *                  The available balance for each currency and payment type can be found in the sourceTypes property.
    * @param livemode
    * @param pending   Funds that are not available in the balance yet, due to the 7-day rolling pay cycle.
    *                  The pending balance for each currency and payment type can be found in the sourceTypes property
    */
  case class Balance(available: List[BalanceFund], livemode: Boolean, pending: List[BalanceFund]) extends StripeObject

  implicit val balanceDecoder: Decoder[Balance] = Decoder.forProduct3(
    "available",
    "livemode",
    "pending"
  )(Balance.apply)

  implicit val balanceEncoder: Encoder[Balance] = Encoder.forProduct4(
    "object",
    "available",
    "livemode",
    "pending"
  )(x => ("balance", x.available, x.livemode, x.pending))

  /**
    * @see https://stripe.com/docs/api#retrieve_balance
    * @param apiKey
    * @param endpoint
    * @return
    */
  def get(implicit apiKey: ApiKey,
          endpoint: Endpoint,
          client: HttpExt,
          materializer: Materializer,
          executionContext: ExecutionContext): Future[Try[Balance]] = {
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
  def getBalanceTransaction(id: String)(implicit apiKey: ApiKey,
                                        endpoint: Endpoint,
                                        client: HttpExt,
                                        materializer: Materializer,
                                        executionContext: ExecutionContext): Future[Try[BalanceTransaction]] = {
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
  case class BalanceHistoryListInput(availableOn: Option[ListFilterInput] = None,
                                     created: Option[ListFilterInput] = None,
                                     currency: Option[Currency] = None,
                                     endingBefore: Option[String] = None,
                                     limit: Option[Long] = None,
                                     source: Option[String] = None,
                                     startingAfter: Option[String] = None,
                                     transfer: Option[Boolean] = None,
                                     `type`: Option[Type] = None)

  case class BalanceTransactionList(override val url: String,
                                    override val hasMore: Boolean,
                                    override val data: List[BalanceTransaction],
                                    override val totalCount: Option[Long])
      extends Collections.List[BalanceTransaction](url, hasMore, data, totalCount)

  object BalanceTransactionList extends Collections.ListJsonMappers[BalanceTransaction] {
    implicit val balanceTransactionListDecoder: Decoder[BalanceTransactionList] =
      listDecoder(implicitly)(BalanceTransactionList.apply)

    implicit val balanceTransactionListEncoder: Encoder[BalanceTransactionList] =
      listEncoder[BalanceTransactionList]
  }

  /**
    * @see https://stripe.com/docs/api#balance_history
    */
  def listBalanceHistory(balanceHistoryListInput: BalanceHistoryListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[BalanceTransactionList]] = {
    val finalUrl = {
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/balance/history$totalCountUrl"

      val created: Uri =
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

      val queries = PostParams.flatten(
        List(
          "currency"       -> balanceHistoryListInput.currency.map(_.iso.toLowerCase),
          "ending_before"  -> balanceHistoryListInput.endingBefore,
          "limit"          -> balanceHistoryListInput.limit.map(_.toString),
          "source"         -> balanceHistoryListInput.source,
          "starting_after" -> balanceHistoryListInput.startingAfter,
          "transfer"       -> balanceHistoryListInput.transfer.map(_.toString),
          "type"           -> balanceHistoryListInput.`type`.map(_.id)
        ))

      val query = queries.foldLeft(created.query())((a, b) => b +: a)
      created.withQuery(query)
    }

    createRequestGET[BalanceTransactionList](finalUrl, logger)
  }
}
