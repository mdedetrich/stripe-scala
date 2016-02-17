package org.mdedetrich.stripe.v1

import org.joda.time.DateTime
import org.mdedetrich.stripe.v1.Discounts.Discount
import org.mdedetrich.stripe.v1.Shippings.Shipping
import org.mdedetrich.stripe.v1.Subscriptions.Subscription
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

object Customers {

  case class Sources(data: List[PaymentSource],
                     hasMore: Boolean,
                     totalCount: Long,
                     url: String)

  implicit val sourcesReads: Reads[Sources] = (
    (__ \ "data").read[List[PaymentSource]] ~
      (__ \ "has_more").read[Boolean] ~
      (__ \ "total_count").read[Long] ~
      (__ \ "url").read[String]
    ).tupled.map(Sources.tupled)

  implicit val sourcesWrites: Writes[Sources] = {
    Writes((sources: Sources) => Json.obj(
      "data" -> sources.data,
      "has_more" -> sources.hasMore,
      "total_count" -> sources.totalCount,
      "url" -> sources.url
    ))
  }

  case class Customer(id: String,
                      accountBalance: BigDecimal,
                      created: DateTime,
                      currency: Currency,
                      defaultSource: String,
                      delinquent: Boolean,
                      description: String,
                      discount: Option[Discount],
                      email: String,
                      livemode: Boolean,
                      metadata: Option[Map[String, String]],
                      shipping: Shipping,
                      sources: Sources,
                      subscriptions: List[Subscription]) extends StripeObject

  implicit val customerReads: Reads[Customer] = (
    (__ \ "id").read[String] ~
      (__ \ "account_balance").read[BigDecimal] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").read[Currency] ~
      (__ \ "default_source").read[String] ~
      (__ \ "delinquent").read[Boolean] ~
      (__ \ "description").read[String] ~
      (__ \ "discount").readNullable[Discount] ~
      (__ \ "email").read[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "shipping").read[Shipping] ~
      (__ \ "sources").read[Sources] ~
      (__ \ "subscriptions").read[List[Subscription]]
    ).tupled.map(Customer.tupled)

  implicit val customerWrites: Writes[Customer] =
    Writes((customer: Customer) => Json.obj(
      "id" -> customer.id,
      "object" -> "customer",
      "account_balance" -> customer.accountBalance,
      "created" -> customer.created.getMillis / 1000,
      "currency" -> customer.currency,
      "default_source" -> customer.defaultSource,
      "delinquent" -> customer.delinquent,
      "description" -> customer.description,
      "discount" -> customer.discount,
      "email" -> customer.email,
      "livemode" -> customer.livemode,
      "metadata" -> customer.metadata,
      "shipping" -> customer.shipping,
      "sources" -> customer.sources,
      "subscriptions" -> customer.subscriptions
    ))

}
