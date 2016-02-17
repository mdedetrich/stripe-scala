package org.mdedetrich.stripe.v1

import org.joda.time.DateTime
import org.mdedetrich.stripe.v1.Discounts.Discount
import org.mdedetrich.stripe.v1.Plans.Plan
import org.mdedetrich.utforsca.SealedContents
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

object Subscriptions {

  sealed abstract class Status(val id: String)

  case class UnknownStatus(val id: String) extends Exception {
    override def getMessage = s"Unknown Subscription Status Type, received $id"
  }

  object Status {

    case object Trialing extends Status("trialing")

    case object Active extends Status("active")

    case object PastDue extends Status("past_due")

    case object Canceled extends Status("canceled")

    case object Unpaid extends Status("unpaid")

    lazy val all: Set[Status] = SealedContents.values[Status]
  }

  implicit val statusReads: Reads[Status] = Reads.of[String].map { statusId =>
    Status.all.find(_.id == statusId).getOrElse {
      throw UnknownStatus(statusId)
    }
  }

  implicit val statusWrites: Writes[Status] =
    Writes((status: Status) => JsString(status.id))

  case class Subscription(id: String,
                          applicationFeePercent: Option[BigDecimal],
                          cancelAtPeriodEnd: Boolean,
                          canceledAt: Option[DateTime],
                          currentPeriodEnd: DateTime,
                          currentPeriodStart: DateTime,
                          customer: String,
                          discount: Option[Discount],
                          endedAt: Option[DateTime],
                          metadata: Option[Map[String,String]],
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
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String,String]] ~
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

}
