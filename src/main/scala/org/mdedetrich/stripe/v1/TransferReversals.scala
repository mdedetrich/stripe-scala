package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey}

import scala.concurrent.Future
import scala.util.Try

/**
  * @see https://stripe.com/docs/api#transfer_reversals
  */
object TransferReversals extends LazyLogging {

  /**
    * @see https://stripe.com/docs/api#transfer_reversal_object
    * @param id
    * @param amount             Amount, in cents.
    * @param balanceTransaction Balance transaction that describes
    *                           the impact on your account balance.
    * @param created
    * @param currency           Three-letter ISO code representing the currency.
    * @param metadata           A set of key/value pairs that you can attach to
    *                           the object. It can be useful for storing
    *                           additional information in a structured format.
    * @param transfer           ID of the transfer that was reversed.
    */
  case class TransferReversal(id: String,
                              amount: BigDecimal,
                              balanceTransaction: String,
                              created: OffsetDateTime,
                              currency: Currency,
                              metadata: Option[Map[String, String]],
                              transfer: String)
      extends StripeObject

  implicit val transferReversalReads: Reads[TransferReversal] = (
      (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "balance_transaction").read[String] ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "transfer").read[String]
  ).tupled.map((TransferReversal.apply _).tupled)

  implicit val transferReversalWrites: Writes[TransferReversal] = Writes(
      (transferReversal: TransferReversal) =>
        Json.obj(
            "id" -> transferReversal.id,
            "object" -> "transfer_reversal",
            "amount" -> transferReversal.amount,
            "balance_transaction" -> transferReversal.balanceTransaction,
            "created" -> Json.toJson(transferReversal.created)(
                stripeDateTimeWrites),
            "currency" -> transferReversal.currency,
            "metadata" -> transferReversal.metadata,
            "transfer" -> transferReversal.transfer
      ))

  /**
    * @see https://stripe.com/docs/api#create_transfer_reversal
    * @param id                   The identifier of the transfer to be reversed.
    * @param amount               A positive integer in cents representing
    *                             how much of this transfer to reverse.
    *                             Can only reverse up to the unreversed
    *                             amount remaining of the transfer. Partial
    *                             transfer reversals are only allowed for
    *                             transfers to Stripe Accounts.
    * @param description          An arbitrary string which you
    *                             can attach to a reversal object. It is
    *                             displayed alongside the reversal in
    *                             the dashboard. This will be unset
    *                             if you POST an empty value.
    * @param metadata             A set of key/value pairs that you can
    *                             attach to a reversal object. It can
    *                             be useful for storing additional
    *                             information about the reversal in
    *                             a structured format. You can unset
    *                             individual keys if you POST an
    *                             empty value for that key. You can
    *                             clear all keys if you POST an
    *                             empty value for metadata.
    * @param refundApplicationFee Boolean indicating whether the application
    *                             fee should be refunded when reversing this
    *                             transfer. If a full transfer reversal is
    *                             given, the full application fee will be
    *                             refunded. Otherwise, the application fee
    *                             will be refunded with an amount proportional
    *                             to the amount of the transfer reversed.
    */
  case class TransferReversalInput(id: String,
                                   amount: Option[BigDecimal],
                                   description: Option[String],
                                   metadata: Option[Map[String, String]],
                                   refundApplicationFee: Option[Boolean])

  object TransferReversalInput {
    def default(id: String): TransferReversalInput = TransferReversalInput(
        id,
        None,
        None,
        None,
        None
    )
  }

  implicit val transferReversalInputReads: Reads[TransferReversalInput] = (
      (__ \ "id").read[String] ~
      (__ \ "amount").readNullable[BigDecimal] ~
      (__ \ "description").readNullable[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "refund_application_fee").readNullable[Boolean]
  ).tupled.map((TransferReversalInput.apply _).tupled)

  implicit val transferReversalInputWrites: Writes[TransferReversalInput] =
    Writes(
        (transferReversalInput: TransferReversalInput) =>
          Json.obj(
              "id" -> transferReversalInput.id,
              "amount" -> transferReversalInput.amount,
              "description" -> transferReversalInput.description,
              "metadata" -> transferReversalInput.metadata,
              "refund_application_fee" -> transferReversalInput.refundApplicationFee
        ))

  def create(transferReversalInput: TransferReversalInput)(
      idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[TransferReversal]] = {
    val postFormParameters: Map[String, String] = {
      Map(
          "amount" -> transferReversalInput.amount.map(_.toString()),
          "description" -> transferReversalInput.description,
          "refund_application_fee" -> transferReversalInput.refundApplicationFee
            .map(_.toString)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(transferReversalInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl =
      endpoint.url + s"/v1/transfers/${transferReversalInput.id}/reversals"

    createRequestPOST[TransferReversal](
        finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(id: String, transferId: String)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[TransferReversal]] = {
    val finalUrl = endpoint.url + s"/v1/transfers/$id/reversals/$transferId"

    createRequestGET[TransferReversal](finalUrl, logger)
  }

  /**
    * @see https://stripe.com/docs/api#list_transfer_reversals
    * @param id            The ID of the transfer whose
    *                      reversals will be retrieved.
    * @param endingBefore  A cursor for use in pagination. [[endingBefore]]
    *                      is an object ID that defines your place in the
    *                      list. For instance, if you make a list request
    *                      and receive 100 objects, starting with obj_bar,
    *                      your subsequent call can include
    *                      [[endingBefore]]=obj_bar in order to fetch the
    *                      previous page of the list.
    * @param limit         A limit on the number of objects to be returned.
    *                      Limit can range between 1 and 100 items.
    * @param startingAfter A cursor for use in pagination. [[startingAfter]]
    *                      is an object ID that defines your place in the
    *                      list. For instance, if you make a list request
    *                      and receive 100 objects, ending with obj_foo,
    *                      your subsequent call can include [[startingAfter]]=obj_foo
    *                      in order to fetch the next page of the list.
    */
  case class TransferReversalListInput(id: String,
                                       endingBefore: Option[String],
                                       limit: Option[Long],
                                       startingAfter: Option[String])

  object TransferReversalListInput {
    def default(id: String): TransferReversalListInput =
      TransferReversalListInput(
          id,
          None,
          None,
          None
      )
  }

  case class TransferReversalList(override val url: String,
                                  override val hasMore: Boolean,
                                  override val data: List[TransferReversal],
                                  override val totalCount: Option[Long])
      extends Collections.List[TransferReversal](
          url, hasMore, data, totalCount)

  object TransferReversalList
      extends Collections.ListJsonMappers[TransferReversal] {
    implicit val transferReversalReads: Reads[TransferReversalList] =
      listReads.tupled.map((TransferReversalList.apply _).tupled)

    implicit val transferReversalWrites: Writes[TransferReversalList] =
      listWrites
  }

  def list(transferReversalListInput: TransferReversalListInput,
           includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[TransferReversalList]] = {
    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl =
        endpoint.url +
        s"/v1/transfers/${transferReversalListInput.id}/reversals$totalCountUrl"

      (baseUrl ?
          ("ending_before" -> transferReversalListInput.endingBefore) ?
          ("limit" -> transferReversalListInput.limit.map(_.toString)) ?
          ("starting_after" -> transferReversalListInput.startingAfter))
        .toString()
    }

    createRequestGET[TransferReversalList](finalUrl, logger)
  }
}
