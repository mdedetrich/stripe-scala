package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.Discounts.Discount
import org.mdedetrich.stripe.v1.Plans.Plan
import org.mdedetrich.stripe.v1.Sources.BaseCardSource
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey}
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

object Subscriptions extends LazyLogging {

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {

    val values = findValues

    case object Trialing extends Status("trialing")

    case object Active extends Status("active")

    case object PastDue extends Status("past_due")

    case object Canceled extends Status("canceled")

    case object Unpaid extends Status("unpaid")

  }

  implicit val statusFormats = EnumFormats.formats(Status, insensitive = true)

  case class Subscription(id: String,
                          applicationFeePercent: Option[BigDecimal],
                          cancelAtPeriodEnd: Boolean,
                          canceledAt: Option[DateTime],
                          currentPeriodEnd: DateTime,
                          currentPeriodStart: DateTime,
                          customer: String,
                          discount: Option[Discount],
                          endedAt: Option[DateTime],
                          metadata: Option[Map[String, String]],
                          plan: Plan,
                          quantity: Long,
                          start: DateTime,
                          status: Status,
                          taxPercent: Option[BigDecimal],
                          trialEnd: Option[DateTime],
                          trialStart: Option[DateTime]
                         )

  object Subscription {
    def default(id: String,
                cancelAtPeriod: Boolean,
                customer: String,
                currentPeriodEnd: DateTime,
                currentPeriodStart: DateTime,
                plan: Plan,
                quantity: Long,
                start: DateTime,
                status: Status) = Subscription(
      id,
      None,
      cancelAtPeriod,
      None,
      currentPeriodEnd,
      currentPeriodStart,
      customer,
      None,
      None,
      None,
      plan,
      quantity,
      start,
      status,
      None,
      None,
      None
    )
  }

  implicit val subscriptionReads: Reads[Subscription] = (
    (__ \ "id").read[String] ~
      (__ \ "application_fee_percent").readNullable[BigDecimal] ~
      (__ \ "cancel_at_period_end").read[Boolean] ~
      (__ \ "canceled_at").readNullable[DateTime](stripeDateTimeReads) ~
      (__ \ "current_period_end").read[DateTime](stripeDateTimeReads) ~
      (__ \ "current_period_start").read[DateTime](stripeDateTimeReads) ~
      (__ \ "customer").read[String] ~
      (__ \ "discount").readNullable[Discount] ~
      (__ \ "ended_at").readNullable[DateTime](stripeDateTimeReads) ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "plan").read[Plan] ~
      (__ \ "quantity").read[Long] ~
      (__ \ "start").read[DateTime](stripeDateTimeReads) ~
      (__ \ "status").read[Status] ~
      (__ \ "tax_percent").readNullable[BigDecimal] ~
      (__ \ "trial_end").readNullable[DateTime](stripeDateTimeReads) ~
      (__ \ "trial_start").readNullable[DateTime](stripeDateTimeReads)
    ).tupled.map((Subscription.apply _).tupled)

  implicit val subscriptionWrites: Writes[Subscription] =
    Writes((subscription: Subscription) =>
      Json.obj(
        "id" -> subscription.id,
        "object" -> "subscription",
        "application_fee_percent" -> subscription.applicationFeePercent,
        "cancel_at_period_end" -> subscription.cancelAtPeriodEnd,
        "canceled_at" -> subscription.canceledAt.map(x => Json.toJson(x)(stripeDateTimeWrites)),
        "current_period_end" -> Json.toJson(subscription.currentPeriodEnd)(stripeDateTimeWrites),
        "current_period_start" -> Json.toJson(subscription.currentPeriodStart)(stripeDateTimeWrites),
        "customer" -> subscription.customer,
        "discount" -> subscription.discount,
        "ended_at" -> subscription.endedAt.map(x => Json.toJson(x)(stripeDateTimeWrites)),
        "metadata" -> subscription.metadata,
        "plan" -> subscription.plan,
        "quantity" -> subscription.quantity,
        "start" -> subscription.start,
        "status" -> subscription.status,
        "tax_percent" -> subscription.taxPercent,
        "trial_end" -> subscription.trialEnd.map(x => Json.toJson(x)(stripeDateTimeWrites)),
        "trial_start" -> subscription.trialStart.map(x => Json.toJson(x)(stripeDateTimeWrites))
      )
    )

  sealed abstract class Source

  object Source {

    case class Token(id: String) extends Source

    case class Card(expMonth: Int,
                    expYear: Int,
                    number: String,
                    addressCountry: Option[String],
                    addressLine1: Option[String],
                    addressLine2: Option[String],
                    addressState: Option[String],
                    addressZip: Option[String],
                    cvc: Option[String],
                    name: Option[String]
                   ) extends Source with BaseCardSource

  }

  implicit val sourceReads: Reads[Source] = {
    __.read[JsValue].flatMap {
      case jsObject: JsObject => (
        (__ \ "exp_month").read[Int] ~
          (__ \ "exp_year").read[Int] ~
          (__ \ "number").read[String] ~
          (__ \ "address_country").readNullable[String] ~
          (__ \ "address_line1").readNullable[String] ~
          (__ \ "address_line2").readNullable[String] ~
          (__ \ "address_state").readNullable[String] ~
          (__ \ "address_zip").readNullable[String] ~
          (__ \ "cvc").readNullable[String] ~
          (__ \ "name").readNullable[String]
        ).tupled.map((Source.Card.apply _).tupled)
      case jsString: JsString =>
        __.read[String].map { tokenId => Source.Token(tokenId) }
      case _ =>
        Reads[Source](_ => JsError(ValidationError("InvalidSource")))
    }
  }

  implicit val sourceWrites: Writes[Source] =
    Writes((source: Source) =>
      source match {
        case Source.Token(id) =>
          JsString(id)
        case Source.Card(
        expMonth,
        expYear,
        number,
        addressCountry,
        addressLine1,
        addressLine2,
        addressState,
        addressZip,
        cvc,
        name
        ) =>
          Json.obj(
            "object" -> "card",
            "exp_month" -> expMonth,
            "exp_year" -> expYear,
            "number" -> number,
            "address_country" -> addressCountry,
            "address_line1" -> addressLine1,
            "address_line2" -> addressLine2,
            "address_state" -> addressState,
            "address_zip" -> addressZip,
            "cvc" -> cvc,
            "name" -> name
          )
      }
    )

  case class SubscriptionInput(applicationFeePercent: Option[BigDecimal],
                               coupon: Option[String],
                               plan: String,
                               source: Option[Source],
                               quantity: Option[Long],
                               metadata: Option[Map[String, String]],
                               taxPercent: Option[BigDecimal],
                               trialEnd: Option[DateTime]
                              )

  object SubscriptionInput {
    def default(plan: String): SubscriptionInput = SubscriptionInput(
      None,
      None,
      plan,
      None,
      None,
      None,
      None,
      None
    )
  }

  def create(customerId: String,
             subscriptionInput: SubscriptionInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Subscription]] = {

    val postFormParameters: Map[String, String] = {
      Map(
        "application_fee_percent" -> subscriptionInput.applicationFeePercent.map(_.toString()),
        "coupon" -> subscriptionInput.coupon,
        "plan" -> Option(subscriptionInput.plan),
        "quantity" -> subscriptionInput.quantity.map(_.toString),
        "tax_percent" -> subscriptionInput.taxPercent.map(_.toString()),
        "trial_end" -> subscriptionInput.trialEnd.map(stripeDateTimeParamWrites)
      ).collect {
        case (k, Some(v)) => (k, v)
      }

    } ++ mapToPostParams(subscriptionInput.metadata, "metadata") ++ {
      subscriptionInput.source match {
        case Some(Source.Card(
        expMonth,
        expYear,
        number,
        addressCountry,
        addressLine1,
        addressLine2,
        addressState,
        addressZip,
        cvc,
        name
        )) =>
          val map = Map(
            "exp_month" -> Option(expMonth.toString),
            "exp_year" -> Option(expYear.toString),
            "number" -> Option(number),
            "address_country" -> addressCountry,
            "address_line1" -> addressLine1,
            "address_line2" -> addressLine2,
            "address_state" -> addressState,
            "address_zip" -> addressZip,
            "cvc" -> cvc,
            "name" -> name
          ).collect {
            case (k, Some(v)) => (k, v)
          }

          mapToPostParams(Option(map), "card")
        case Some(Source.Token(id)) =>
          Map("source" -> id)
        case None =>
          Map.empty
      }
    }

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + s"/v1/customers/$customerId/subscriptions"

    createRequestPOST[Subscription](finalUrl, postFormParameters, idempotencyKey, logger)

  }

}
