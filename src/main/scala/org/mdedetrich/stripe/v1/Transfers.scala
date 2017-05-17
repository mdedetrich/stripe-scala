package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import defaults._
import enumeratum._
import io.circe.{Decoder, Encoder}
import org.mdedetrich.stripe.v1.BankAccounts._
import org.mdedetrich.stripe.v1.TransferReversals._
import org.mdedetrich.stripe.v1.defaults._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Transfers extends LazyLogging {

  case class TransferReversalList(override val url: String,
                                  override val hasMore: Boolean,
                                  override val data: List[TransferReversal],
                                  override val totalCount: Option[Long])
      extends Collections.List[TransferReversal](
        url,
        hasMore,
        data,
        totalCount
      )

  object TransferReversalList extends Collections.ListJsonMappers[TransferReversal] {
    implicit val transferReversalListDecoder: Decoder[TransferReversalList] =
      listDecoder(implicitly)(TransferReversalList.apply)

    implicit val transferReversalListEncoder: Encoder[TransferReversalList] =
      listEncoder[TransferReversalList]
  }

  sealed abstract class Type(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Type extends Enum[Type] {
    val values = findValues

    case object Card          extends Type("card")
    case object BankAccount   extends Type("bank_account")
    case object StripeAccount extends Type("stripe_account")

    implicit val transferTypeDecoder: Decoder[Type] = enumeratum.Circe.decoder(Type)
    implicit val transferTypeEncoder: Encoder[Type] = enumeratum.Circe.encoder(Type)
  }

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {
    val values = findValues

    case object Paid      extends Status("paid")
    case object Pending   extends Status("pending")
    case object InTransit extends Status("in_transit")
    case object Canceled  extends Status("canceled")
    case object Failed    extends Status("failed")

    implicit val transferStatusDecoder: Decoder[Status] = enumeratum.Circe.decoder(Status)
    implicit val transferStatusEncoder: Encoder[Status] = enumeratum.Circe.encoder(Status)
  }

  /**
    * @see https://stripe.com/docs/api#transfer_failures
    * @param id
    */
  sealed abstract class FailureCode(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object FailureCode extends Enum[FailureCode] {
    val values = findValues

    case object InsufficientFunds     extends FailureCode("insufficient_funds")
    case object AccountClosed         extends FailureCode("account_closed")
    case object NoAccount             extends FailureCode("no_account")
    case object InvalidAccountNumber  extends FailureCode("invalid_account_number")
    case object DebitNotAuthorized    extends FailureCode("debit_not_authorized")
    case object BankOwnershipChanged  extends FailureCode("bank_ownership_changed")
    case object AccountFrozen         extends FailureCode("account_frozen")
    case object CouldNotProcess       extends FailureCode("could_not_process")
    case object BankAccountRestricted extends FailureCode("bank_account_restricted")
    case object InvalidCurrency       extends FailureCode("invalid_currency")

    implicit val transferStatusFailureCodeDecoder: Decoder[FailureCode] = enumeratum.Circe.decoder(FailureCode)
    implicit val transferStatusFailureCodeEncoder: Encoder[FailureCode] = enumeratum.Circe.encoder(FailureCode)

  }

  sealed abstract class SourceType(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object SourceType extends Enum[SourceType] {
    val values = findValues

    case object Card            extends SourceType("card")
    case object AlipayAccount   extends SourceType("alipay_account")
    case object BitcoinReceiver extends SourceType("bitcoin_receiver")
    case object BankAccount     extends SourceType("bank_account")

    implicit val sourceTypeDecoder: Decoder[SourceType] = enumeratum.Circe.decoder(SourceType)
    implicit val sourceTypeEncoder: Encoder[SourceType] = enumeratum.Circe.encoder(SourceType)
  }

  case class Transfer(id: String,
                      amount: BigDecimal,
                      amountReversed: BigDecimal,
                      applicationFee: Option[BigDecimal],
                      balanceTransaction: String,
                      bankAccount: Option[BankAccount],
                      created: OffsetDateTime,
                      currency: Currency,
                      date: OffsetDateTime,
                      description: Option[String],
                      destination: String,
                      destinationPayment: Option[String],
                      failureCode: Option[FailureCode],
                      failureMessage: Option[String],
                      livemode: Boolean,
                      metadata: Option[Map[String, String]],
                      recipient: Option[String],
                      reversals: TransferReversalList,
                      reversed: Boolean,
                      sourceTransaction: Option[String],
                      sourceType: SourceType,
                      statementDescriptor: Option[String],
                      status: Status,
                      `type`: Type)
      extends StripeObject

  private val transferDecoderOne = Decoder.forProduct22(
    "id",
    "amount",
    "amount_reversed",
    "application_fee",
    "balance_transaction",
    "bank_account",
    "created",
    "currency",
    "date",
    "description",
    "destination",
    "destination_payment",
    "failure_code",
    "failure_message",
    "livemode",
    "metadata",
    "recipient",
    "reversals",
    "reversed",
    "source_transaction",
    "source_type",
    "statement_descriptor"
  )(
    Tuple22.apply(
      _: String,
      _: BigDecimal,
      _: BigDecimal,
      _: Option[BigDecimal],
      _: String,
      _: Option[BankAccount],
      _: OffsetDateTime,
      _: Currency,
      _: OffsetDateTime,
      _: Option[String],
      _: String,
      _: Option[String],
      _: Option[FailureCode],
      _: Option[String],
      _: Boolean,
      _: Option[Map[String, String]],
      _: Option[String],
      _: TransferReversalList,
      _: Boolean,
      _: Option[String],
      _: SourceType,
      _: Option[String]
    ))

  private val transferDecoderTwo = Decoder.forProduct2("status", "type")(
    Tuple2.apply(
      _: Status,
      _: Type
    ))

  implicit val transferDecoder: Decoder[Transfer] = Decoder.instance[Transfer] { c =>
    for {
      one <- transferDecoderOne.apply(c)
      two <- transferDecoderTwo.apply(c)
    } yield {
      val (id,
           amount,
           amountReversed,
           applicationFee,
           balanceTransaction,
           bankAccount,
           created,
           currency,
           date,
           description,
           destination,
           destinationPayment,
           failureCode,
           failureMessage,
           livemode,
           metadata,
           recipient,
           reversals,
           reversed,
           sourceTransaction,
           sourceType,
           statementDescriptor) = one
      val (status, type_)       = two
      Transfer(
        id,
        amount,
        amountReversed,
        applicationFee,
        balanceTransaction,
        bankAccount,
        created,
        currency,
        date,
        description,
        destination,
        destinationPayment,
        failureCode,
        failureMessage,
        livemode,
        metadata,
        recipient,
        reversals,
        reversed,
        sourceTransaction,
        sourceType,
        statementDescriptor,
        status,
        type_
      )
    }
  }

  private val transferEncoderOne: Encoder[Transfer] = Encoder.forProduct22(
    "id",
    "object",
    "amount",
    "amount_reversed",
    "application_fee",
    "balance_transaction",
    "bank_account",
    "created",
    "currency",
    "date",
    "description",
    "destination",
    "destination_payment",
    "failure_code",
    "failure_message",
    "livemode",
    "metadata",
    "recipient",
    "reversals",
    "reversed",
    "source_transaction",
    "source_type"
  )(
    x =>
      (x.id,
       "transfer",
       x.amount,
       x.amountReversed,
       x.applicationFee,
       x.balanceTransaction,
       x.bankAccount,
       x.created,
       x.currency,
       x.date,
       x.description,
       x.destination,
       x.destinationPayment,
       x.failureCode,
       x.failureMessage,
       x.livemode,
       x.metadata,
       x.recipient,
       x.reversals,
       x.reversed,
       x.sourceTransaction,
       x.sourceType))

  private val transferEncoderTwo: Encoder[Transfer] = Encoder.forProduct3(
    "statement_descriptor",
    "status",
    "type"
  )(x => (x.statementDescriptor, x.status, x.`type`))

  implicit val transferEncoder: Encoder[Transfer] = Encoder.instance[Transfer] { t =>
    transferEncoderOne.apply(t).deepMerge(transferEncoderTwo.apply(t))
  }

  /**
    * @see https://stripe.com/docs/api#create_transfer
    * @param amount              A positive integer in cents representing how much to transfer.
    * @param currency            3-letter ISO code for currency.
    * @param destination         The id of a bank account or a card to send the transfer to,
    *                            or the string default_for_currency to use the default
    *                            external account for the specified currency. If you use
    *                            Stripe Connect, this can be the the id of a connected Stripe
    *                            account; see the details about when such transfers are permitted.
    * @param description         An arbitrary string which you can attach to a transfer object.
    *                            It is displayed when in the web interface alongside the transfer.
    * @param metadata            A set of key/value pairs that you can attach to a transfer object.
    *                            It can be useful for storing additional information about the
    *                            transfer in a structured format.
    * @param sourceTransaction   You can use this parameter to transfer funds
    *                            from a charge (or other transaction) before they
    *                            are added to your available balance. A pending balance
    *                            will transfer immediately but the funds will not become
    *                            available until the original charge becomes available.
    *                            See the Connect documentation for details.
    * @param statementDescriptor A string to be displayed on the recipient's bank or card statement.
    *                            This may be at most 22 characters. Attempting to use a [[statementDescriptor]]
    *                            longer than 22 characters will return an error.
    *                            Note: Most banks will truncate this information and/or display it inconsistently.
    *                            Some may not display it at all.
    * @param stripeAccount       The Stripe Connect managed account on whose behalf the transfer should be initiated.
    * @param sourceType          The source balance to draw this transfer from.
    *                            Balances for different payment sources are kept separately.
    *                            You can find the amounts with the balances API. Valid options are:
    *                            [[SourceType.AlipayAccount]], [[SourceType.BankAccount]], [[SourceType.BitcoinReceiver]], and
    *                            [[SourceType.Card]].
    * @throws StatementDescriptorTooLong          - If [[statementDescriptor]] is longer than 22 characters
    * @throws StatementDescriptorInvalidCharacter - If [[statementDescriptor]] has an invalid character
    */
  case class TransferInput(amount: BigDecimal,
                           currency: Currency,
                           destination: String,
                           description: Option[String],
                           metadata: Option[Map[String, String]],
                           sourceTransaction: Option[String],
                           statementDescriptor: Option[String],
                           stripeAccount: Option[String],
                           sourceType: Option[SourceType]) {
    statementDescriptor match {
      case Some(sD) if sD.length > 22 =>
        throw StatementDescriptorTooLong(sD.length)
      case Some(sD) if sD.contains("<") =>
        throw StatementDescriptorInvalidCharacter("<")
      case Some(sD) if sD.contains(">") =>
        throw StatementDescriptorInvalidCharacter(">")
      case Some(sD) if sD.contains("\"") =>
        throw StatementDescriptorInvalidCharacter("\"")
      case Some(sD) if sD.contains("\'") =>
        throw StatementDescriptorInvalidCharacter("\'")
      case _ =>
    }
  }

  object TransferInput {
    def default(amount: BigDecimal,
                currency: Currency,
                destination: String,
                description: Option[String] = None,
                metadata: Option[Map[String, String]] = None,
                sourceTransaction: Option[String] = None,
                statementDescriptor: Option[String] = None,
                stripeAccount: Option[String] = None,
                sourceType: Option[SourceType] = None) =
      TransferInput(amount,
                    currency,
                    destination,
                    description,
                    metadata,
                    sourceTransaction,
                    statementDescriptor,
                    stripeAccount,
                    sourceType)

  }

  def create(transferInput: TransferInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[Transfer]] = {
    val postFormParameters = PostParams.flatten(
      Map(
        "amount"               -> Option(transferInput.amount.toString()),
        "currency"             -> Option(transferInput.currency.iso.toLowerCase()),
        "destination"          -> Option(transferInput.destination),
        "description"          -> transferInput.description,
        "source_transaction"   -> transferInput.sourceTransaction,
        "statement_descriptor" -> transferInput.statementDescriptor,
        "source_type"          -> transferInput.sourceType.map(_.id)
      )) ++ mapToPostParams(transferInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/transfers"

    createRequestPOST[Transfer](finalUrl,
                                postFormParameters,
                                idempotencyKey,
                                logger,
                                stripeAccount = transferInput.stripeAccount)
  }

  def get(id: String)(implicit apiKey: ApiKey,
                      endpoint: Endpoint,
                      client: HttpExt,
                      materializer: Materializer,
                      executionContext: ExecutionContext): Future[Try[Transfer]] = {
    val finalUrl = endpoint.url + s"/v1/transfers/$id"

    createRequestGET[Transfer](finalUrl, logger)
  }

  case class TransferListInput(created: Option[ListFilterInput],
                               date: Option[ListFilterInput],
                               destination: Option[String],
                               endingBefore: Option[String],
                               limit: Option[String],
                               recipient: Option[String],
                               startingAfter: Option[String],
                               status: Option[Status])

  object TransferListInput {
    def default: TransferListInput = TransferListInput(
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

  case class TransferList(override val url: String,
                          override val hasMore: Boolean,
                          override val data: List[Transfers.Transfer],
                          override val totalCount: Option[Long])
      extends Collections.List[Transfer](
        url,
        hasMore,
        data,
        totalCount
      )

  object TransferList extends Collections.ListJsonMappers[Transfer] {
    implicit val transferListDecoder: Decoder[TransferList] =
      listDecoder(implicitly)(TransferList.apply)

    implicit val transferListEncoder: Encoder[TransferList] =
      listEncoder[TransferList]
  }

  def list(transferListInput: TransferListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[TransferList]] = {

    val finalUrl = {
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/transfers$totalCountUrl"

      val created: Uri =
        (transferListInput.created, transferListInput.date) match {
          case (Some(createdInput), Some(dateInput)) =>
            listFilterInputToUri(dateInput, listFilterInputToUri(createdInput, baseUrl, "created"), "date")
          case (Some(createdInput), None) =>
            listFilterInputToUri(createdInput, baseUrl, "created")
          case (None, Some(dateInput)) =>
            listFilterInputToUri(dateInput, baseUrl, "date")
          case (None, None) => baseUrl
        }

      val queries = PostParams.flatten(
        List(
          "destination"    -> transferListInput.destination,
          "ending_before"  -> transferListInput.endingBefore,
          "limit"          -> transferListInput.limit.map(_.toString),
          "recipient"      -> transferListInput.recipient,
          "starting_after" -> transferListInput.startingAfter,
          "status"         -> transferListInput.status.map(_.id)
        ))

      val query = queries.foldLeft(created.query())((a, b) => b +: a)
      created.withQuery(query)
    }

    createRequestGET[TransferList](finalUrl, logger)
  }
}
