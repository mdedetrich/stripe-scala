package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.joda.time.DateTime
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

object Coupons extends LazyLogging {

  sealed abstract class Duration(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Duration extends Enum[Duration] {

    val values = findValues

    case object Forever extends Duration("forever")

    case object Once extends Duration("once")

    case object Repeating extends Duration("repeating")

  }

  implicit val durationFormats = EnumFormats.formats(Duration, insensitive = true)

  case class Coupon(id: String,
                    amountOff: Option[Long],
                    created: DateTime,
                    currency: Option[Currency],
                    duration: Duration,
                    durationInMonths: Option[Long],
                    livemode: Boolean,
                    maxRedemptions: Option[Long],
                    metadata: Option[Map[String, String]],
                    percentOff: Option[BigDecimal],
                    redeemBy: Option[DateTime],
                    timesRedeemed: Long,
                    valid: Boolean
                   ) extends StripeObject

  object Coupon {
    def default(id: String,
                created: DateTime,
                duration: Duration,
                livemode: Boolean,
                timesRedeemed: Long,
                valid: Boolean) = Coupon(
      id,
      None,
      created,
      None,
      duration,
      None,
      livemode,
      None,
      None,
      None,
      None,
      timesRedeemed,
      valid
    )
  }

  implicit val couponReads: Reads[Coupon] = (
    (__ \ "id").read[String] ~
      (__ \ "amount_off").readNullable[Long] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").readNullable[Currency] ~
      (__ \ "duration").read[Duration] ~
      (__ \ "duration_in_months").readNullable[Long] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "max_redemptions").readNullable[Long] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "percent_off").readNullable[BigDecimal] ~
      (__ \ "redeem_by").readNullable[DateTime](stripeDateTimeReads) ~
      (__ \ "times_redeemed").read[Long] ~
      (__ \ "valid").read[Boolean]
    ).tupled.map((Coupon.apply _).tupled)

  implicit val couponWrites: Writes[Coupon] =
    Writes((coupon: Coupon) =>
      Json.obj(
        "id" -> coupon.id,
        "object" -> "coupon",
        "amount_off" -> coupon.amountOff,
        "created" -> Json.toJson(coupon.created)(stripeDateTimeWrites),
        "currency" -> coupon.amountOff,
        "duration" -> coupon.duration,
        "duration_in_months" -> coupon.durationInMonths,
        "livemode" -> coupon.livemode,
        "max_redemptions" -> coupon.maxRedemptions,
        "metadata" -> coupon.metadata,
        "percent_off" -> coupon.percentOff,
        "redeem_by" -> coupon.redeemBy.map(x => Json.toJson(x)(stripeDateTimeWrites)),
        "times_redeemed" -> coupon.timesRedeemed,
        "valid" -> coupon.valid
      )
    )

  case class CouponInput(id: Option[String],
                         duration: Duration,
                         amountOff: Option[Long],
                         currency: Option[Currency],
                         durationInMonths: Option[Long],
                         maxRedemptions: Option[Long],
                         metadata: Option[Map[String, String]],
                         percentOff: Option[BigDecimal],
                         redeemBy: Option[DateTime]
                        )

  object CouponInput {
    def default(duration: Duration): CouponInput = CouponInput(
      None,
      duration,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )
  }

  implicit val couponInputReads: Reads[CouponInput] = (
    (__ \ "id").readNullable[String] ~
      (__ \ "duration").read[Duration] ~
      (__ \ "amount_off").readNullable[Long] ~
      (__ \ "currency").readNullable[Currency] ~
      (__ \ "duration_in_months").readNullable[Long] ~
      (__ \ "max_redemptions").readNullable[Long] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "percent_off").readNullable[BigDecimal] ~
      (__ \ "redeemBy").readNullable[DateTime](stripeDateTimeReads)
    ).tupled.map((CouponInput.apply _).tupled)

  implicit val couponInputWrites: Writes[CouponInput] =
    Writes((couponInput: CouponInput) =>
      Json.obj(
        "id" -> couponInput.id,
        "duration" -> couponInput.duration,
        "amount_off" -> couponInput.amountOff,
        "currency" -> couponInput.currency,
        "duration_in_months" -> couponInput.durationInMonths,
        "max_redemptions" -> couponInput.maxRedemptions,
        "metadata" -> couponInput.metadata,
        "percent_off" -> couponInput.percentOff,
        "redeemBy" -> couponInput.redeemBy.map(x => Json.toJson(x)(stripeDateTimeWrites))
      )
    )

  def create(couponInput: CouponInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Coupon]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "id" -> couponInput.id,
        "duration" -> Option(couponInput.duration.entryName),
        "amount_off" -> couponInput.amountOff.map(_.toString),
        "currency" -> couponInput.currency.map(_.iso.toLowerCase),
        "duration_in_months" -> couponInput.durationInMonths.map(_.toString),
        "max_redemptions" -> couponInput.maxRedemptions.map(_.toString),
        "percent_off" -> couponInput.percentOff.map(_.toString()),
        "redeemBy" -> couponInput.redeemBy.map(stripeDateTimeParamWrites)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(couponInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/charges"

    createRequestPOST[Coupon](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(id: String)
         (implicit apiKey: ApiKey,
          endpoint: Endpoint): Future[Try[Coupon]] = {
    val finalUrl = endpoint.url + s"/v1/coupons/$id"

    createRequestGET[Coupon](finalUrl, logger)

  }

  def delete(id: String)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[DeleteResponse]] = {
    val finalUrl = endpoint.url + s"/v1/coupons/$id"

    createRequestDELETE(finalUrl, idempotencyKey, logger)

  }

  case class CouponListInput(created: Option[ListFilterInput],
                             endingBefore: Option[String],
                             limit: Option[Long],
                             startingAfter: Option[String]
                            )

  object CouponListInput {
    def default: CouponListInput = CouponListInput(
      None,
      None,
      None,
      None
    )
  }

  case class CouponList(override val url: String,
                        override val hasMore: Boolean,
                        override val data: List[Coupon],
                        override val totalCount: Option[Long]
                       )
    extends Collections.List[Coupon](url, hasMore, data, totalCount)

  object CouponList extends Collections.ListJsonMappers[Coupon] {
    implicit val couponListReads: Reads[CouponList] =
      listReads.tupled.map((CouponList.apply _).tupled)

    implicit val couponListWrites: Writes[CouponList] =
      listWrites
  }

  def list(couponListInput: CouponListInput,
           includeTotalCount: Boolean)
          (implicit apiKey: ApiKey,
           endpoint: Endpoint): Future[Try[CouponList]] = {

    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl = if (includeTotalCount)
        "/include[]=total_count"
      else
        ""

      val baseUrl = endpoint.url + s"/v1/customers$totalCountUrl"

      val created: com.netaporter.uri.Uri = couponListInput.created match {
        case Some(createdInput) =>
          listFilterInputToUri(createdInput, baseUrl)
        case None => baseUrl
      }

      (created ?
        ("ending_before" -> couponListInput.endingBefore) ?
        ("limit" -> couponListInput.limit.map(_.toString)) ?
        ("starting_after" -> couponListInput.startingAfter)
        ).toString()

    }

    createRequestGET[CouponList](finalUrl, logger)

  }

}
