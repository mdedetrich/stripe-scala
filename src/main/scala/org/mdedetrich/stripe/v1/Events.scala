package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.stream.Materializer
import com.typesafe.scalalogging.LazyLogging
import defaults._
import enumeratum._
import io.circe.{Decoder, Encoder, JsonObject}
import org.mdedetrich.stripe.{ApiKey, Endpoint}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Events extends LazyLogging {

  sealed abstract class Type(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Type extends Enum[Type] {
    val values = findValues

    case object AccountUpdated                    extends Type("account.updated")
    case object AccountApplicationDeauthorized    extends Type("account.application.deauthorized")
    case object AccountExternalAccountCreated     extends Type("account.external_account.created")
    case object AccountExternalAccountDeleted     extends Type("account.external_account.deleted")
    case object AccountExternalAccountUpdated     extends Type("account.external_account.updated")
    case object ApplicationFeeCreated             extends Type("application_fee.created")
    case object ApplicationFeeRefunded            extends Type("application_fee.refunded")
    case object ApplicationFeeRefundUpdated       extends Type("application_fee.refund.updated")
    case object BalanceAvailable                  extends Type("balance.available")
    case object BitcoinReceiverCreated            extends Type("bitcoin.receiver.created")
    case object BitcoinReceiverFilled             extends Type("bitcoin.receiver.filled")
    case object BitcoinReceiverUpdated            extends Type("bitcoin.receiver.updated")
    case object BitcoinReceiverTransactionCreated extends Type("bitcoin.receiver.transaction.created")
    case object ChargeCaptured                    extends Type("charge.captured")
    case object ChargeFailed                      extends Type("charge.failed")
    case object ChargeRefunded                    extends Type("charge.refunded")
    case object ChargeSucceeded                   extends Type("charge.succeeded")
    case object ChargeUpdated                     extends Type("charge.updated")
    case object ChargeDisputeClosed               extends Type("charge.dispute.closed")
    case object ChargeDisputeCreated              extends Type("charge.dispute.created")
    case object ChargeDisputeFundsReinstated      extends Type("charge.dispute.funds_reinstated")
    case object ChargeDisputeFundsWithdrawn       extends Type("charge.dispute.funds_withdrawn")
    case object ChargeDisputeUpdated              extends Type("charge.dispute.updated")
    case object CouponCreated                     extends Type("coupon.created")
    case object CouponDeleted                     extends Type("coupon.deleted")
    case object CouponUpdated                     extends Type("coupon.updated")
    case object CustomerCreated                   extends Type("customer.created")
    case object CustomerDeleted                   extends Type("customer.deleted")
    case object CustomerUpdated                   extends Type("customer.updated")
    case object CustomerDiscountCreated           extends Type("customer.discount.created")
    case object CustomerDiscountDeleted           extends Type("customer.discount.deleted")
    case object CustomerDiscountUpdated           extends Type("ustomer.discount.updated")
    case object CustomerSourceCreated             extends Type("customer.source.created")
    case object CustomerSourceDeleted             extends Type("customer.source.deleted")
    case object CustomerSourceUpdated             extends Type("customer.source.updated")
    case object CustomerSubscriptionCreated       extends Type("customer.subscription.created")
    case object CustomerSubscriptionDeleted       extends Type("customer.subscription.deleted")
    case object CustomerSubscriptionTrialWillEnd  extends Type("customer.subscription.trial_will_end")
    case object CustomerSubscriptionUpdated       extends Type("customer.subscription.updated")
    case object PaymentCreated                    extends Type("payment.created")
    case object InvoiceCreated                    extends Type("invoice.created")
    case object InvoicePaymentFailed              extends Type("invoice.payment_failed")
    case object InvoicePaymentSucceeded           extends Type("invoice.payment_succeeded")
    case object InvoiceUpdated                    extends Type("invoice.updated")
    case object InvoiceitemCreated                extends Type("invoiceitem.created")
    case object InvoiceitemDeleted                extends Type("invoiceitem.deleted")
    case object InvoiceitemUpdated                extends Type("invoiceitem.updated")
    case object OrderCreated                      extends Type("order.created")
    case object OrderPaymentFailed                extends Type("order.payment_failed")
    case object OrderPaymentSucceeded             extends Type("order.payment_succeeded")
    case object OrderUpdated                      extends Type("order.updated")
    case object PlanCreated                       extends Type("plan.created")
    case object PlanDeleted                       extends Type("plan.deleted")
    case object PlanUpdated                       extends Type("plan.updated")
    case object ProductCreated                    extends Type("product.created")
    case object ProductDeleted                    extends Type("product.deleted")
    case object ProductUpdated                    extends Type("product.updated")
    case object RecipientCreated                  extends Type("recipient.created")
    case object RecipientDeleted                  extends Type("recipient.deleted")
    case object RecipientUpdated                  extends Type("recipient.updated")
    case object SKUCreated                        extends Type("sku.created")
    case object SKUDeleted                        extends Type("sku.deleted")
    case object SKUUpdated                        extends Type("sku.updated")
    case object TransferCreated                   extends Type("transfer.created")
    case object TransferFailed                    extends Type("transfer.failed")
    case object TransferPaid                      extends Type("transfer.paid")
    case object TransferReversed                  extends Type("transfer.reversed")
    case object TransferUpdated                   extends Type("transfer.updated")
    case object Ping                              extends Type("ping")

    implicit val eventTypeDecoder: Decoder[Type] = enumeratum.Circe.decoder(Type)
    implicit val eventTypeEncoder: Encoder[Type] = enumeratum.Circe.encoder(Type)
  }

  final case class Data(`object`: StripeObject, previousAttributes: Option[JsonObject])

  implicit val eventDataDecoder: Decoder[Data] = Decoder.forProduct2("object", "previous_attributes")(Data.apply)
  implicit val eventDataEncoder: Encoder[Data] =
    Encoder.forProduct2("object", "previous_attributes")(x => Data.unapply(x).get)

  final case class Event(
      id: String,
      apiVersion: String,
      created: OffsetDateTime,
      data: Data,
      livemode: Boolean,
      pendingWebhooks: Long,
      request: Option[String],
      `type`: Type,
      userId: Option[String],
      account: Option[String]
  )

  implicit val eventDecoder: Decoder[Event] = Decoder.forProduct10(
    "id",
    "api_version",
    "created",
    "data",
    "livemode",
    "pending_webhooks",
    "request",
    "type",
    "user_id",
    "account"
  )(Event.apply)

  implicit val eventEncoder: Encoder[Event] = Encoder.forProduct10(
    "id",
    "api_version",
    "created",
    "data",
    "livemode",
    "pending_webhooks",
    "request",
    "type",
    "user_id",
    "account"
  )(x => Event.unapply(x).get)

  final case class EventList(
      override val url: String,
      override val hasMore: Boolean,
      override val data: List[Event],
      override val totalCount: Option[Long]
  ) extends Collections.List[Event](url, hasMore, data, totalCount)

  object EventList extends Collections.ListJsonMappers[Event] {
    implicit val eventListDecoder: Decoder[EventList] =
      listDecoder(implicitly)(EventList.apply)
    implicit val eventListEncoder: Encoder[EventList] =
      listEncoder[EventList]
  }

  def get(id: String, stripeAccount: Option[String] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Event]] = {
    val finalUrl = endpoint.url + s"/v1/events/$id"
    createRequestGET[Event](finalUrl, logger, stripeAccount)
  }
}
