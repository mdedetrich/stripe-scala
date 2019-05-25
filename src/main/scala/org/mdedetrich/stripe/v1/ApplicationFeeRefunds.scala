package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.stream.Materializer
import com.typesafe.scalalogging.LazyLogging
import defaults._
import io.circe.{Decoder, Encoder}
import org.mdedetrich.stripe.PostParams.{flatten, toPostParams}
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * @see https://stripe.com/docs/api#fee_refunds
  */
object ApplicationFeeRefunds extends LazyLogging {

  case class ApplicationFeeRefund(
      id: String,
      amount: BigDecimal,
      metadata: Option[Map[String, String]],
      created: OffsetDateTime,
      currency: Currency,
      fee: String,
      balanceTransaction: Option[String]
  )

  implicit val applicationFeeRefundDecoder: Decoder[ApplicationFeeRefund] = Decoder.forProduct7(
    "id",
    "amount",
    "metadata",
    "created",
    "currency",
    "fee",
    "balance_transaction"
  )(ApplicationFeeRefund.apply)

  implicit val applicationFeeRefundEncoder: Encoder[ApplicationFeeRefund] = Encoder.forProduct8(
    "id",
    "object",
    "amount",
    "metadata",
    "created",
    "currency",
    "fee",
    "balance_transaction"
  )(x => (x.id, "fee_refund", x.amount, x.metadata, x.created, x.currency, x.fee, x.balanceTransaction))

  case class ApplicationFeeRefundInput(
      id: String,
      amount: Option[BigDecimal] = None,
      metadata: Map[String, String] = Map.empty
  )

  implicit val refundInputPostParams = PostParams.params[ApplicationFeeRefundInput] { refundInput =>
    val optional = Map(
      "amount" -> refundInput.amount.map(_.toString)
    )
    flatten(optional) ++ PostParams.toPostParams("metadata", refundInput.metadata)
  }

  def create(refundInput: ApplicationFeeRefundInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[ApplicationFeeRefund]] = {

    val postFormParameters = toPostParams(refundInput)
    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = s"${endpoint.url}/v1/application_fees/${refundInput.id}/refunds"

    createRequestPOST[ApplicationFeeRefund](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(id: String)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[ApplicationFeeRefund]] = {
    val finalUrl = s"${endpoint.url}/v1/application_fees/$id/refunds"

    createRequestGET[ApplicationFeeRefund](finalUrl, logger)
  }
}
