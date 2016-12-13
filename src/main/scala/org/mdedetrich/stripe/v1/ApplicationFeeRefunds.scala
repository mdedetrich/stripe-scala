package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import com.typesafe.scalalogging.LazyLogging
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.PostParams.{flatten, toPostParams}
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/**
  * @see https://stripe.com/docs/api#fee_refunds
  */
object ApplicationFeeRefunds extends LazyLogging {

  case class ApplicationFeeRefund(id: String,
                                  amount: BigDecimal,
                                  metadata: Option[Map[String, String]],
                                  created: OffsetDateTime,
                                  currency: Currency,
                                  fee: String)

  implicit val refundReads: Reads[ApplicationFeeRefund] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "fee").read[String]
  ).tupled.map((ApplicationFeeRefund.apply _).tupled)

  implicit val refundWrites: Writes[ApplicationFeeRefund] = Json.writes[ApplicationFeeRefund]

  case class ApplicationFeeRefundInput(id: String, amount: Option[BigDecimal], metadata: Map[String, String])

  object ApplicationFeeRefundInput {
    def default(id: String) = ApplicationFeeRefundInput(
      id,
      None,
      Map.empty
    )
  }

  implicit val refundInputPostParams = PostParams.params[ApplicationFeeRefundInput] { refundInput =>
    val optional = Map(
      "amount" -> refundInput.amount.map(_.toString)
    )
    flatten(optional) ++ PostParams.toPostParams("metadata", refundInput.metadata)
  }

  def create(refundInput: ApplicationFeeRefundInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[ApplicationFeeRefund]] = {

    val postFormParameters = toPostParams(refundInput)
    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = s"${endpoint.url}/v1/application_fees/${refundInput.id}/refunds"

    createRequestPOST[ApplicationFeeRefund](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(id: String)(implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[ApplicationFeeRefund]] = {
    val finalUrl = s"${endpoint.url}/v1/application_fees/$id/refunds"

    createRequestGET[ApplicationFeeRefund](finalUrl, logger)
  }
}
