package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import dispatch.Defaults._
import dispatch._
import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.stripe.{IdempotencyKey, InvalidJsonModelException, Endpoint, ApiKey}
import org.mdedetrich.stripe.v1.Discounts.Discount
import org.mdedetrich.stripe.v1.Plans.Plan
import org.mdedetrich.stripe.v1.Sources.BaseCardSource
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

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

  implicit val subscriptionReads: Reads[Subscription] = (
    (__ \ "id").read[String] ~
      (__ \ "application_fee_percent").readNullable[BigDecimal] ~
      (__ \ "cancel_at_period_end").read[Boolean] ~
      (__ \ "canceled_at").readNullable[Long].map {
        _.map { timestamp => new DateTime(timestamp * 1000) }
      } ~
      (__ \ "current_period_end").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "current_period_start").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "customer").read[String] ~
      (__ \ "discount").readNullable[Discount] ~
      (__ \ "ended_at").readNullable[Long].map {
        _.map { timestamp => new DateTime(timestamp * 1000) }
      } ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "plan").read[Plan] ~
      (__ \ "quantity").read[Long] ~
      (__ \ "start").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "status").read[Status] ~
      (__ \ "tax_percent").readNullable[BigDecimal] ~
      (__ \ "trial_end").readNullable[Long].map {
        _.map { timestamp => new DateTime(timestamp * 1000) }
      } ~
      (__ \ "trial_start").readNullable[Long].map {
        _.map { timestamp => new DateTime(timestamp * 1000) }
      }
    ).tupled.map(Subscription.tupled)

  implicit val subscriptionWrites: Writes[Subscription] =
    Writes((subscription: Subscription) =>
      Json.obj(
        "id" -> subscription.id,
        "object" -> "subscription",
        "application_fee_percent" -> subscription.applicationFeePercent,
        "cancel_at_period_end" -> subscription.cancelAtPeriodEnd,
        "canceled_at" -> subscription.canceledAt.map(_.getMillis / 1000),
        "current_period_end" -> subscription.currentPeriodEnd.getMillis / 1000,
        "current_period_start" -> subscription.currentPeriodStart.getMillis / 1000,
        "customer" -> subscription.customer,
        "discount" -> subscription.discount,
        "ended_at" -> subscription.endedAt.map(_.getMillis / 1000),
        "metadata" -> subscription.metadata,
        "plan" -> subscription.plan,
        "quantity" -> subscription.quantity,
        "start" -> subscription.start,
        "status" -> subscription.status,
        "tax_percent" -> subscription.taxPercent,
        "trial_end" -> subscription.trialEnd.map(_.getMillis / 1000),
        "trial_start" -> subscription.trialStart.map(_.getMillis / 1000)
      )
    )

  sealed abstract class Source

  object Source {

    case class Token(val id: String) extends Source

    case class Card(val expMonth: Long,
                    val expYear: Long,
                    val number: String,
                    val addressCountry: Option[String],
                    val addressLine1: Option[String],
                    val addressLine2: Option[String],
                    val addressState: Option[String],
                    val addressZip: Option[String],
                    val cvc: Option[String],
                    val name: Option[String]
                   ) extends Source with BaseCardSource

  }

  implicit val sourceReads: Reads[Source] = {
    __.read[JsValue].flatMap {
      case jsObject: JsObject => (
        (__ \ "exp_month").read[Long] ~
          (__ \ "exp_year").read[Long] ~
          (__ \ "number").read[String] ~
          (__ \ "address_country").readNullable[String] ~
          (__ \ "address_line1").readNullable[String] ~
          (__ \ "address_line2").readNullable[String] ~
          (__ \ "address_state").readNullable[String] ~
          (__ \ "address_zip").readNullable[String] ~
          (__ \ "cvc").readNullable[String] ~
          (__ \ "name").readNullable[String]
        ).tupled.map(Source.Card.tupled)
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

  def create(subscriptionInput: SubscriptionInput)
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
        "trial_end" -> subscriptionInput.trialEnd.map(dateTime => (dateTime.getMillis / 1000).toString)
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

    val finalUrl = endpoint.url + "/v1/customers"

    val req = {
      val r = (
        url(finalUrl)
          .addHeader("Content-Type", "application/x-www-form-urlencoded")
          << postFormParameters
        ).POST.as(apiKey.apiKey, "")

      idempotencyKey match {
        case Some(key) =>
          r.addHeader(idempotencyKeyHeader, key.key)
        case None =>
          r
      }
    }

    Http(req).map { response =>

      parseStripeServerError(response, finalUrl, Option(postFormParameters), None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[Subscription](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, Option(postFormParameters), None, jsValue, errors)
              }, subscription => subscription
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }

}
