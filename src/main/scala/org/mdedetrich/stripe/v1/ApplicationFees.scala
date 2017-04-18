package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import com.typesafe.scalalogging.LazyLogging
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.PostParams.{flatten, toPostParams}
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/**
  * @see https://stripe.com/docs/api#application_fees
  */
object ApplicationFees extends LazyLogging {

  case class ApplicationFee(id: String,
                            amount: BigDecimal,
                            application: String,
                            created: OffsetDateTime,
                            currency: Currency,
                            originatingTransaction: String)
      extends StripeObject

  implicit val applicationFeeReads: Reads[ApplicationFee] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "application").read[String] ~
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "originating_transaction").read[String]
  ).tupled.map((ApplicationFee.apply _).tupled)

  implicit val applicationFeeWrites: Writes[ApplicationFee] = Json.writes[ApplicationFee]

}
