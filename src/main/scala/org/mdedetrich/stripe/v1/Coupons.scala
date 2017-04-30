package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.stream.Materializer
import com.typesafe.scalalogging.LazyLogging
import enumeratum._
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
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

  implicit val durationFormats =
    EnumFormats.formats(Duration, insensitive = true)

  /**
    * @see https://stripe.com/docs/api#coupons
    * @param id
    * @param amountOff        Amount (in the currency specified) that will be taken
    *                         off the subtotal of any invoices for this customer.
    * @param created
    * @param currency         If [[amountOff]] has been set, the currency of the amount to take off.
    * @param duration         One of [[Duration.Forever]], [[Duration.Once]], and [[Duration.Repeating]].
    *                         Describes how long a customer who applies this coupon will get the discount.
    * @param durationInMonths If duration is repeating, the number of months the coupon applies.
    *                         [[None]] if coupon duration is forever or once.
    * @param livemode
    * @param maxRedemptions   Maximum number of times this coupon can be redeemed,
    *                         in total, before it is no longer valid.
    * @param metadata         A set of key/value pairs that you can attach to a coupon object.
    *                         It can be useful for storing additional information
    *                         about the coupon in a structured format.
    * @param percentOff       Percent that will be taken off the subtotal of any invoices for this
    *                         customer for the duration of the coupon. For example, a coupon with
    *                         [[percentOff]] of 50 will make a $100 invoice $50 instead.
    * @param redeemBy         Date after which the coupon can no longer be redeemed
    * @param timesRedeemed    Number of times this coupon has been applied to a customer.
    * @param valid            Taking account of the above properties,
    *                         whether this coupon can still be applied to a customer
    */
  case class Coupon(id: String,
                    amountOff: Option[Long],
                    created: OffsetDateTime,
                    currency: Option[Currency],
                    duration: Duration,
                    durationInMonths: Option[Long],
                    livemode: Boolean,
                    maxRedemptions: Option[Long],
                    metadata: Option[Map[String, String]],
                    percentOff: Option[BigDecimal],
                    redeemBy: Option[OffsetDateTime],
                    timesRedeemed: Long,
                    valid: Boolean)
      extends StripeObject

  object Coupon {
    def default(id: String,
                created: OffsetDateTime,
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
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "currency").readNullable[Currency] ~
      (__ \ "duration").read[Duration] ~
      (__ \ "duration_in_months").readNullable[Long] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "max_redemptions").readNullable[Long] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "percent_off").readNullable[BigDecimal] ~
      (__ \ "redeem_by").readNullable[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "times_redeemed").read[Long] ~
      (__ \ "valid").read[Boolean]
  ).tupled.map((Coupon.apply _).tupled)

  implicit val couponWrites: Writes[Coupon] = Writes(
    (coupon: Coupon) =>
      Json.obj(
        "id"                 -> coupon.id,
        "object"             -> "coupon",
        "amount_off"         -> coupon.amountOff,
        "created"            -> Json.toJson(coupon.created)(stripeDateTimeWrites),
        "currency"           -> coupon.amountOff,
        "duration"           -> coupon.duration,
        "duration_in_months" -> coupon.durationInMonths,
        "livemode"           -> coupon.livemode,
        "max_redemptions"    -> coupon.maxRedemptions,
        "metadata"           -> coupon.metadata,
        "percent_off"        -> coupon.percentOff,
        "redeem_by"          -> coupon.redeemBy.map(x => Json.toJson(x)(stripeDateTimeWrites)),
        "times_redeemed"     -> coupon.timesRedeemed,
        "valid"              -> coupon.valid
    ))

  /**
    * @see https://stripe.com/docs/api#create_coupon
    * @param id               Unique string of your choice that will be used to identify this
    *                         coupon when applying it to a customer. This is often a specific
    *                         code you’ll give to your customer to use when signing up
    *                         (e.g. FALL25OFF). If you don’t want to specify a particular code,
    *                         you can leave the ID blank and we’ll generate a random code for you.
    * @param duration         Specifies how long the discount will be in effect.
    *                         Can be [[Duration.Forever]], [[Duration.Once]],
    *                         or [[Duration.Repeating]].
    * @param amountOff        A positive integer representing the amount to subtract
    *                         from an invoice total (required if [[percentOff]] is not passed)
    * @param currency         Currency of the [[amountOff]] parameter
    *                         (required if [[amountOff]] is passed)
    * @param durationInMonths Required only if [[duration]] is [[Duration.Repeating]],
    *                         in which case it must be a positive integer that
    *                         specifies the number of months the discount will be in effect.
    * @param maxRedemptions   A positive integer specifying the number of times
    *                         the coupon can be redeemed before it’s no longer valid.
    *                         For example, you might have a 50% off coupon that the
    *                         first 20 readers of your blog can use.
    * @param metadata         A set of key/value pairs that you can attach to a coupon object.
    *                         It can be useful for storing additional information about the
    *                         coupon in a structured format. This will be unset if you POST
    *                         an empty value.
    * @param percentOff       A positive integer between 1 and 100 that represents the discount
    *                         the coupon will apply (required if [[amountOff]] is not passed)
    * @param redeemBy         Unix timestamp specifying the last time at which
    *                         the coupon can be redeemed. After the [[redeemBy]] date, the
    *                         coupon can no longer be applied to new customers.
    */
  case class CouponInput(id: Option[String],
                         duration: Duration,
                         amountOff: Option[Long],
                         currency: Option[Currency],
                         durationInMonths: Option[Long],
                         maxRedemptions: Option[Long],
                         metadata: Option[Map[String, String]],
                         percentOff: Option[BigDecimal],
                         redeemBy: Option[OffsetDateTime])

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
      (__ \ "redeemBy").readNullable[OffsetDateTime](stripeDateTimeReads)
  ).tupled.map((CouponInput.apply _).tupled)

  implicit val couponInputWrites: Writes[CouponInput] = Writes(
    (couponInput: CouponInput) =>
      Json.obj(
        "id"                 -> couponInput.id,
        "duration"           -> couponInput.duration,
        "amount_off"         -> couponInput.amountOff,
        "currency"           -> couponInput.currency,
        "duration_in_months" -> couponInput.durationInMonths,
        "max_redemptions"    -> couponInput.maxRedemptions,
        "metadata"           -> couponInput.metadata,
        "percent_off"        -> couponInput.percentOff,
        "redeemBy"           -> couponInput.redeemBy.map(x => Json.toJson(x)(stripeDateTimeWrites))
    ))

  def create(couponInput: CouponInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[Coupon]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "id"                 -> couponInput.id,
        "duration"           -> Option(couponInput.duration.entryName),
        "amount_off"         -> couponInput.amountOff.map(_.toString),
        "currency"           -> couponInput.currency.map(_.iso.toLowerCase),
        "duration_in_months" -> couponInput.durationInMonths.map(_.toString),
        "max_redemptions"    -> couponInput.maxRedemptions.map(_.toString),
        "percent_off"        -> couponInput.percentOff.map(_.toString()),
        "redeemBy"           -> couponInput.redeemBy.map(stripeDateTimeParamWrites)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(couponInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/charges"

    createRequestPOST[Coupon](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(id: String)(implicit apiKey: ApiKey,
                      endpoint: Endpoint,
                      client: HttpExt,
                      materializer: Materializer,
                      executionContext: ExecutionContext): Future[Try[Coupon]] = {
    val finalUrl = endpoint.url + s"/v1/coupons/$id"

    createRequestGET[Coupon](finalUrl, logger)
  }

  def delete(id: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[DeleteResponse]] = {
    val finalUrl = endpoint.url + s"/v1/coupons/$id"

    createRequestDELETE(finalUrl, idempotencyKey, logger)
  }

  /**
    * @see https://stripe.com/docs/api#list_coupons
    * @param created       A filter on the list based on the object created field.
    *                      The value can be a string with an integer Unix timestamp,
    *                      or it can be a dictionary with the following options:
    * @param endingBefore  A cursor for use in pagination. [[endingBefore]]
    *                      is an object ID that defines your place in the list.
    *                      For instance, if you make a list request and
    *                      receive 100 objects, starting with obj_bar,
    *                      your subsequent call can include [[endingBefore]]=obj_bar
    *                      in order to fetch the previous page of the list.
    * @param limit         A limit on the number of objects to be returned.
    *                      Limit can range between 1 and 100 items.
    * @param startingAfter A cursor for use in pagination. [[startingAfter]]
    *                      is an object ID that defines your place in the list.
    *                      For instance, if you make a list request and
    *                      receive 100 objects, ending with obj_foo, your
    *                      subsequent call can include [[startingAfter]]=obj_foo
    *                      in order to fetch the next page of the list.
    */
  case class CouponListInput(created: Option[ListFilterInput],
                             endingBefore: Option[String],
                             limit: Option[Long],
                             startingAfter: Option[String])

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
                        override val totalCount: Option[Long])
      extends Collections.List[Coupon](url, hasMore, data, totalCount)

  object CouponList extends Collections.ListJsonMappers[Coupon] {
    implicit val couponListReads: Reads[CouponList] =
      listReads.tupled.map((CouponList.apply _).tupled)

    implicit val couponListWrites: Writes[CouponList] = listWrites
  }

  def list(couponListInput: CouponListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[CouponList]] = {

    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/coupons$totalCountUrl"

      val created: com.netaporter.uri.Uri = couponListInput.created match {
        case Some(createdInput) =>
          listFilterInputToUri(createdInput, baseUrl, "created")
        case None => baseUrl
      }

      (created ?
        ("ending_before"  -> couponListInput.endingBefore) ?
        ("limit"          -> couponListInput.limit.map(_.toString)) ?
        ("starting_after" -> couponListInput.startingAfter)).toString()
    }

    createRequestGET[CouponList](finalUrl, logger)
  }
}
