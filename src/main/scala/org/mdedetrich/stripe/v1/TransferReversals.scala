package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey}

import scala.concurrent.Future
import scala.util.Try

object TransferReversals extends LazyLogging {

  case class TransferReversal(id: String,
                              amount: BigDecimal,
                              balanceTransaction: String,
                              created: DateTime,
                              currency: Currency,
                              metadata: Option[Map[String, String]],
                              transfer: String) extends StripeObject

  implicit val transferReversalReads: Reads[TransferReversal] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "balance_transaction").read[String] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "transfer").read[String]
    ).tupled.map((TransferReversal.apply _).tupled)

  implicit val transferReversalWrites: Writes[TransferReversal] =
    Writes((transferReversal: TransferReversal) =>
      Json.obj(
        "id" -> transferReversal.id,
        "object" -> "transfer_reversal",
        "amount" -> transferReversal.amount,
        "balance_transaction" -> transferReversal.balanceTransaction,
        "created" -> Json.toJson(transferReversal.created)(stripeDateTimeWrites),
        "currency" -> transferReversal.currency,
        "metadata" -> transferReversal.metadata,
        "transfer" -> transferReversal.transfer
      )
    )
  
  case class TransferReversalInput(id: String,
                                   amount: Option[BigDecimal],
                                   description: Option[String],
                                   metadata: Option[Map[String,String]],
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
    Writes ((transferReversalInput: TransferReversalInput) =>
      Json.obj(
        "id" -> transferReversalInput.id,
        "amount" -> transferReversalInput.amount,
        "description" -> transferReversalInput.description,
        "metadata" -> transferReversalInput.metadata,
        "refund_application_fee" -> transferReversalInput.refundApplicationFee
      )
    )
  
  def create(transferReversalInput: TransferReversalInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[TransferReversal]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "amount" -> transferReversalInput.amount.map(_.toString()),
        "description" -> transferReversalInput.description,
        "refund_application_fee" -> transferReversalInput.refundApplicationFee.map(_.toString)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(transferReversalInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + s"/v1/transfers/${transferReversalInput.id}/reversals"

    createRequestPOST[TransferReversal](finalUrl, postFormParameters, idempotencyKey, logger)

  }
  
  def get(id: String, transferId: String)
         (implicit apiKey: ApiKey,
          endpoint: Endpoint): Future[Try[TransferReversal]] = {
    val finalUrl = endpoint.url + s"/v1/transfers/$id/reversals/$transferId"

    createRequestGET[TransferReversal](finalUrl, logger)

  }

  case class TransferReversalListInput(id: String,
                                       endingBefore: Option[String],
                                       limit: Option[Long],
                                       startingAfter: Option[String])

  object TransferReversalListInput {
    def default(id: String): TransferReversalListInput = TransferReversalListInput(
      id,
      None,
      None,
      None
    )
  }

  case class TransferReversalList(override val url: String,
                                  override val hasMore: Boolean,
                                  override val data: List[TransferReversal],
                                  override val totalCount: Option[Long]
                                 )
    extends Collections.List[TransferReversal](url, hasMore, data, totalCount)

  object TransferReversalList extends Collections.ListJsonMappers[TransferReversal] {
    implicit val transferReversalReads: Reads[TransferReversalList] =
      listReads.tupled.map((TransferReversalList.apply _).tupled)

    implicit val transferReversalWrites: Writes[TransferReversalList] =
      listWrites
  }

  def list(transferReversalListInput: TransferReversalListInput,
           includeTotalCount: Boolean)
          (implicit apiKey: ApiKey,
           endpoint: Endpoint): Future[Try[TransferReversalList]] = {
    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl = if (includeTotalCount)
        "/include[]=total_count"
      else
        ""

      val baseUrl = endpoint.url + s"/v1/transfers/${transferReversalListInput.id}/reversals$totalCountUrl"

      (baseUrl ?
        ("ending_before" -> transferReversalListInput.endingBefore) ?
        ("limit" -> transferReversalListInput.limit.map(_.toString)) ?
        ("starting_after" -> transferReversalListInput.startingAfter)
        ).toString()
    }

    createRequestGET[TransferReversalList](finalUrl, logger)

  }

}
