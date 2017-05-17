package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import com.typesafe.scalalogging.LazyLogging
import defaults._
import enumeratum._
import io.circe.{Decoder, Encoder}
import org.mdedetrich.stripe.v1.defaults._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Coupons extends LazyLogging {

  sealed abstract class Duration(val id: String) extends EnumEntry {
    override val entryName = id
  }

  object Duration extends Enum[Duration] {
    val values = findValues

    case object Forever   extends Duration("forever")
    case object Once      extends Duration("once")
    case object Repeating extends Duration("repeating")

    implicit val couponDurationDecoder: Decoder[Duration] = enumeratum.Circe.decoder(Duration)
    implicit val couponDurationEncoder: Encoder[Duration] = enumeratum.Circe.encoder(Duration)
  }

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

  implicit val couponDecoder: Decoder[Coupon] = Decoder.forProduct13(
    "id",
    "amount_off",
    "created",
    "currency",
    "duration",
    "duration_in_months",
    "livemode",
    "max_redemptions",
    "metadata",
    "percent_off",
    "redeem_by",
    "times_redeemed",
    "valid"
  )(Coupon.apply)

  implicit val couponEncoder: Encoder[Coupon] = Encoder.forProduct14(
    "id",
    "object",
    "amount_off",
    "created",
    "currency",
    "duration",
    "duration_in_months",
    "livemode",
    "max_redemptions",
    "metadata",
    "percent_off",
    "redeem_by",
    "times_redeemed",
    "valid"
  )(
    x =>
      (x.id,
       "coupon",
       x.amountOff,
       x.created,
       x.currency,
       x.duration,
       x.durationInMonths,
       x.livemode,
       x.maxRedemptions,
       x.metadata,
       x.percentOff,
       x.redeemBy,
       x.timesRedeemed,
       x.valid))

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

  implicit val couponInputDecoder: Decoder[CouponInput] = Decoder.forProduct9(
    "id",
    "duration",
    "amount_off",
    "currency",
    "duration_in_months",
    "max_redemptions",
    "metadata",
    "percent_off",
    "redeem_by"
  )(CouponInput.apply)

  implicit val couponInputEncoder: Encoder[CouponInput] = Encoder.forProduct9(
    "id",
    "duration",
    "amount_off",
    "currency",
    "duration_in_months",
    "max_redemptions",
    "metadata",
    "percent_off",
    "redeem_by"
  )(x => CouponInput.unapply(x).get)

  def create(couponInput: CouponInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[Coupon]] = {
    val postFormParameters = PostParams.flatten(
      Map(
        "id"                 -> couponInput.id,
        "duration"           -> Option(couponInput.duration.entryName),
        "amount_off"         -> couponInput.amountOff.map(_.toString),
        "currency"           -> couponInput.currency.map(_.iso.toLowerCase),
        "duration_in_months" -> couponInput.durationInMonths.map(_.toString),
        "max_redemptions"    -> couponInput.maxRedemptions.map(_.toString),
        "percent_off"        -> couponInput.percentOff.map(_.toString()),
        "redeemBy"           -> couponInput.redeemBy.map(stripeDateTimeParamWrites)
      )) ++ mapToPostParams(couponInput.metadata, "metadata")

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
    implicit val couponListDecoder: Decoder[CouponList] =
      listDecoder(implicitly)(CouponList.apply)

    implicit val couponListEncoder: Encoder[CouponList] =
      listEncoder[CouponList]
  }

  def list(couponListInput: CouponListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[CouponList]] = {

    val finalUrl = {
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/coupons$totalCountUrl"

      val created: Uri = couponListInput.created match {
        case Some(createdInput) =>
          listFilterInputToUri(createdInput, baseUrl, "created")
        case None => baseUrl
      }

      val queries = PostParams.flatten(
        List(
          "ending_before"  -> couponListInput.endingBefore,
          "limit"          -> couponListInput.limit.map(_.toString),
          "starting_after" -> couponListInput.startingAfter
        ))

      val query = queries.foldLeft(created.query())((a, b) => b +: a)
      created.withQuery(query)
    }

    createRequestGET[CouponList](finalUrl, logger)
  }
}
