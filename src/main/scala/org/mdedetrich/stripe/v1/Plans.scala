package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.stripe.{InvalidJsonModelException, Endpoint, ApiKey, IdempotencyKey}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._
import dispatch.Defaults._
import dispatch._

import scala.concurrent.Future
import scala.util.Try

object Plans extends LazyLogging {

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

  object Plan {
    def default(id: String,
                amount: BigDecimal,
                created: DateTime,
                currency: Currency,
                interval: Interval,
                intervalCount: Long,
                livemode: Boolean,
                name: String): Plan = Plan(
      id,
      amount,
      created,
      currency,
      interval,
      intervalCount,
      livemode,
      None,
      name,
      None,
      None,
      None,
      None,
      None,
      None
    )
  }

  implicit val planReads: Reads[Plan] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "interval").read[Interval] ~
      (__ \ "interval_count").read[Long] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "name").read[String] ~
      (__ \ "quantity").readNullable[Long] ~
      (__ \ "start").readNullable[DateTime](stripeDateTimeReads) ~
      (__ \ "status").readNullable[Status] ~
      (__ \ "tax_percent").readNullable[BigDecimal] ~
      (__ \ "trial_end").readNullable[DateTime](stripeDateTimeReads) ~
      (__ \ "trial_start").readNullable[DateTime](stripeDateTimeReads)
    ).tupled.map((Plan.apply _).tupled)

  implicit val planWrites: Writes[Plan] =
    Writes((plan: Plan) => Json.obj(
      "id" -> plan.id,
      "object" -> "plan",
      "amount" -> plan.amount,
      "created" -> Json.toJson(plan.created)(stripeDateTimeWrites),
      "currency" -> plan.currency,
      "interval" -> plan.interval,
      "interval_count" -> plan.intervalCount,
      "livemode" -> plan.livemode,
      "metadata" -> plan.metadata,
      "name" -> plan.name,
      "quantity" -> plan.quantity,
      "start" -> plan.start.map(x => Json.toJson(x)(stripeDateTimeWrites)),
      "status" -> plan.status,
      "tax_percent" -> plan.taxPercent,
      "trial_end" -> plan.trialEnd.map(x => Json.toJson(x)(stripeDateTimeWrites)),
      "trial_start" -> plan.trialStart.map(x => Json.toJson(x)(stripeDateTimeWrites))
    ))

  case class PlanInput(id: String,
                       amount: BigDecimal,
                       currency: Currency,
                       interval: Interval,
                       name: String,
                       intervalCount: Option[Long],
                       metadata: Option[Map[String, String]],
                       statementDescriptor: Option[String],
                       trialPeriodDays: Option[Long]
                      )

  object PlanInput {
    def default(id: String,
                amount: BigDecimal,
                currency: Currency,
                interval: Interval,
                name: String): PlanInput = PlanInput(
      id,
      amount,
      currency,
      interval,
      name,
      None,
      None,
      None,
      None
    )
  }

  implicit val planInputReads: Reads[PlanInput] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "currency").read[Currency] ~
      (__ \ "interval").read[Interval] ~
      (__ \ "name").read[String] ~
      (__ \ "interval_count").readNullable[Long] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "statement_descriptor").readNullable[String] ~
      (__ \ "trial_period_days").readNullable[Long]
    ).tupled.map((PlanInput.apply _).tupled)

  implicit val planInputWrites: Writes[PlanInput] =
    Writes((planInput: PlanInput) =>
      Json.obj(
        "id" -> planInput.id,
        "amount" -> planInput.amount,
        "currency" -> planInput.currency,
        "interval" -> planInput.interval,
        "name" -> planInput.name,
        "interval_count" -> planInput.intervalCount,
        "metadata" -> planInput.metadata,
        "statement_descriptor" -> planInput.statementDescriptor,
        "trial_period_days" -> planInput.trialPeriodDays
      )
    )

  def create(planInput: PlanInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Plan]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "id" -> Option(planInput.id.toString),
        "amount" -> Option(planInput.amount.toString()),
        "currency" -> Option(planInput.currency.iso.toLowerCase),
        "interval" -> Option(planInput.interval.id.toString),
        "name" -> Option(planInput.name),
        "interval_count" -> planInput.intervalCount.map(_.toString),
        "statement_descriptor" -> planInput.statementDescriptor,
        "trial_period_days" -> planInput.trialPeriodDays.map(_.toString)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(planInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/plans"

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
            val jsResult = Json.fromJson[Plan](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, Option(postFormParameters), None, jsValue, errors)
              }, plan => plan
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }

}
