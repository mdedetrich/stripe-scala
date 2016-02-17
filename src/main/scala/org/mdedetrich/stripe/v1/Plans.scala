package org.mdedetrich.stripe.v1

import org.joda.time.DateTime
import org.mdedetrich.utforsca.SealedContents
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

object Plans {

  sealed abstract class Interval(val id: String)

  case class UnknownInterval(val id: String) extends Exception {
    override def getMessage = s"Unknown Plan Interval, received $id"
  }

  object Interval {

    case object Day extends Interval("day")

    case object Week extends Interval("week")

    case object Month extends Interval("month")

    case object Year extends Interval("year")

    lazy val all: Set[Interval] = SealedContents.values[Interval]
  }

  implicit val intervalReads: Reads[Interval] = Reads.of[String].map { intervalId =>
    Interval.all.find(_.id == intervalId).getOrElse {
      throw UnknownInterval(intervalId)
    }
  }

  implicit val intervalWrites: Writes[Interval] =
    Writes((interval: Interval) => JsString(interval.id))

  sealed abstract class Status(val id: String)

  case class UnknownStatus(val id: String) extends Exception {
    override def getMessage = s"Unknown Plan Status, received $id"
  }

  object Status {

    case object Trialing extends Status("trialing")

    case object active extends Status("active")

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

  case class Plan(id: String,
                  amount: BigDecimal,
                  created: DateTime,
                  currency: Currency,
                  interval: Interval,
                  intervalCount: Long,
                  livemode: Boolean,
                  metadata: Option[Map[String, String]],
                  name: String,
                  quantity: Long,
                  start: DateTime,
                  status: Status,
                  taxPercent: BigDecimal,
                  trialEnd: DateTime,
                  trialStart: DateTime
                 )

  implicit val planReads: Reads[Plan] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").read[Currency] ~
      (__ \ "interval").read[Interval] ~
      (__ \ "interval_count").read[Long] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "name").read[String] ~
      (__ \ "quantity").read[Long] ~
      (__ \ "start").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "status").read[Status] ~
      (__ \ "tax_percent").read[BigDecimal] ~
      (__ \ "trial_end").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "trial_start").read[Long].map { timestamp => new DateTime(timestamp * 1000) }
    ).tupled.map(Plan.tupled)

  implicit val planWrites: Writes[Plan] =
    Writes((plan: Plan) => Json.obj(
      "id" -> plan.id,
      "object" -> "plan",
      "amount" -> plan.amount,
      "created" -> plan.created.getMillis / 1000,
      "currency" -> plan.currency,
      "interval" -> plan.interval,
      "interval_count" -> plan.intervalCount,
      "livemode" -> plan.livemode,
      "metadata" -> plan.metadata,
      "name" -> plan.name,
      "quantity" -> plan.quantity,
      "start" -> plan.start,
      "status" -> plan.status,
      "tax_percent" -> plan.taxPercent,
      "trial_end" -> plan.trialEnd,
      "trial_start" -> plan.trialStart
    ))
}
