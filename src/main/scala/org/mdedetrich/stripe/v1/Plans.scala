package org.mdedetrich.stripe.v1

import enumeratum._
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

object Plans {

  sealed abstract class Interval(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Interval extends Enum[Interval] {

    val values = findValues

    case object Day extends Interval("day")

    case object Week extends Interval("week")

    case object Month extends Interval("month")

    case object Year extends Interval("year")

  }

  implicit val intervalFormats = EnumFormats.formats(Interval, insensitive = true)

  sealed abstract class Status(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Status extends Enum[Status] {

    val values = findValues

    case object Trialing extends Status("trialing")

    case object active extends Status("active")

    case object PastDue extends Status("past_due")

    case object Canceled extends Status("canceled")

    case object Unpaid extends Status("unpaid")

  }

  implicit val statusFormats = EnumFormats.formats(Status, insensitive = true)

  case class Plan(id: String,
                  amount: BigDecimal,
                  created: DateTime,
                  currency: Currency,
                  interval: Interval,
                  intervalCount: Long,
                  livemode: Boolean,
                  metadata: Option[Map[String, String]],
                  name: String,
                  quantity: Option[Long],
                  start: Option[DateTime],
                  status: Option[Status],
                  taxPercent: Option[BigDecimal],
                  trialEnd: Option[DateTime],
                  trialStart: Option[DateTime]
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
      (__ \ "quantity").readNullable[Long] ~
      (__ \ "start").readNullable[Long].map(_.map { timestamp => new DateTime(timestamp * 1000) }) ~
      (__ \ "status").readNullable[Status] ~
      (__ \ "tax_percent").readNullable[BigDecimal] ~
      (__ \ "trial_end").readNullable[Long].map(_.map { timestamp => new DateTime(timestamp * 1000) }) ~
      (__ \ "trial_start").readNullable[Long].map(_.map { timestamp => new DateTime(timestamp * 1000) })
    ).tupled.map((Plan.apply _).tupled)

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
      "start" -> plan.start.map(datetime => datetime.getMillis / 1000),
      "status" -> plan.status,
      "tax_percent" -> plan.taxPercent,
      "trial_end" -> plan.trialEnd.map(datetime => datetime.getMillis / 1000),
      "trial_start" -> plan.trialStart.map(datetime => datetime.getMillis / 1000)
    ))
}
