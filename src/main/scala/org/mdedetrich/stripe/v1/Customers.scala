package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import org.joda.time.DateTime
import dispatch.Defaults._
import dispatch._
import org.mdedetrich.stripe.{IdempotencyKey, InvalidJsonModelException, Endpoint, ApiKey}
import org.mdedetrich.stripe.v1.Discounts.Discount
import org.mdedetrich.stripe.v1.Shippings.Shipping
import org.mdedetrich.stripe.v1.Sources.BaseCardSource
import org.mdedetrich.stripe.v1.Subscriptions.Subscription
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

import scala.concurrent.Future
import scala.util.Try

object Customers extends LazyLogging {

  case class Sources(data: List[PaymentSource],
                     hasMore: Boolean,
                     totalCount: Long,
                     url: String)

  implicit val sourcesReads: Reads[Sources] = (
    (__ \ "data").read[List[PaymentSource]] ~
      (__ \ "has_more").read[Boolean] ~
      (__ \ "total_count").read[Long] ~
      (__ \ "url").read[String]
    ).tupled.map((Sources.apply _).tupled)

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
                      description: Option[String],
                      discount: Option[Discount],
                      email: Option[String],
                      livemode: Boolean,
                      metadata: Option[Map[String, String]],
                      shipping: Shipping,
                      sources: Sources,
                      subscriptions: List[Subscription]) extends StripeObject

  object Customer {
    def default(id: String,
                accountBalance: BigDecimal,
                created: DateTime,
                currency: Currency,
                defaultSource: String,
                delinquent: Boolean,
                livemode: Boolean,
                shipping: Shipping,
                sources: Sources,
                subscriptions: List[Subscription]): Customer = Customer(
      id,
      accountBalance,
      created,
      currency,
      defaultSource,
      delinquent,
      None,
      None,
      None,
      livemode,
      None,
      shipping,
      sources,
      subscriptions
    )
  }

  implicit val customerReads: Reads[Customer] = (
    (__ \ "id").read[String] ~
      (__ \ "account_balance").read[BigDecimal] ~
      (__ \ "created").read[DateTime](stripeDateTimeReads) ~
      (__ \ "currency").read[Currency] ~
      (__ \ "default_source").read[String] ~
      (__ \ "delinquent").read[Boolean] ~
      (__ \ "description").readNullable[String] ~
      (__ \ "discount").readNullable[Discount] ~
      (__ \ "email").readNullable[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "shipping").read[Shipping] ~
      (__ \ "sources").read[Sources] ~
      (__ \ "subscriptions" \ "data").read[List[Subscription]]
    ).tupled.map((Customer.apply _).tupled)

  implicit val customerWrites: Writes[Customer] =
    Writes((customer: Customer) => Json.obj(
      "id" -> customer.id,
      "object" -> "customer",
      "account_balance" -> customer.accountBalance,
      "created" -> Json.toJson(customer.created)(stripeDateTimeWrites),
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

  sealed abstract class Source

  object Source {

    case class Token(val id: String) extends Source

    case class Card(expMonth: Int,
                    expYear: Int,
                    number: String,
                    addressCity: Option[String],
                    addressCountry: Option[String],
                    addressLine1: Option[String],
                    addressLine2: Option[String],
                    addressState: Option[String],
                    addressZip: Option[String],
                    currency: Option[Currency],
                    cvc: Option[String],
                    defaultForCurrency: Option[Boolean],
                    metadata: Option[Map[String, String]],
                    name: Option[String]
                   ) extends Source with BaseCardSource

    object Card {
      def default(expMonth: Int,
                  expYear: Int,
                  number: String
                 ): Card = Card(
        expMonth,
        expYear,
        number,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )
    }

  }

  implicit val sourceReads: Reads[Source] = {
    __.read[JsValue].flatMap {
      case jsObject: JsObject =>
        (
          (__ \ "exp_month").read[Int] ~
            (__ \ "exp_year").read[Int] ~
            (__ \ "number").read[String] ~
            (__ \ "address_city").readNullable[String] ~
            (__ \ "address_country").readNullable[String] ~
            (__ \ "address_line1").readNullable[String] ~
            (__ \ "address_line2").readNullable[String] ~
            (__ \ "address_state").readNullable[String] ~
            (__ \ "address_zip").readNullable[String] ~
            (__ \ "currency").readNullable[Currency] ~
            (__ \ "cvc").readNullable[String] ~
            (__ \ "default_for_currency").readNullable[Boolean] ~
            (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
            (__ \ "name").readNullable[String]
          ).tupled.map((Source.Card.apply _).tupled)
      case jsString: JsString =>
        __.read[String].map { tokenId => Source.Token(tokenId) }
      case _ =>
        Reads[Source](_ => JsError(ValidationError("InvalidSource")))
    }
  }

  implicit val sourceWrites: Writes[Source] = {
    Writes((source: Source) => {
      source match {
        case Source.Token(id) =>
          JsString(id)
        case Source.Card(
        expMonth,
        expYear,
        number,
        addressCity,
        addressCountry,
        addressLine1,
        addressLine2,
        addressState,
        addressZip,
        currency,
        cvc,
        defaultForCurrency,
        metadata,
        name
        ) =>
          Json.obj(
            "object" -> "card",
            "exp_month" -> expMonth,
            "exp_year" -> expYear,
            "number" -> number,
            "address_city" -> addressCity,
            "address_country" -> addressCountry,
            "address_line1" -> addressLine1,
            "address_line2" -> addressLine2,
            "address_state" -> addressState,
            "address_zip" -> addressZip,
            "currency" -> currency,
            "cvc" -> cvc,
            "default_for_currency" -> defaultForCurrency,
            "metadata" -> metadata,
            "name" -> name
          )
      }
    })
  }

  case class CustomerInput(accountBalance: Option[BigDecimal],
                           coupon: Option[String],
                           description: Option[String],
                           email: Option[String],
                           metadata: Option[Map[String, String]],
                           plan: Option[String],
                           quantity: Option[Long],
                           shipping: Option[Shipping],
                           source: Option[Source],
                           taxPercent: Option[BigDecimal],
                           trialEnd: Option[DateTime]
                          )

  object CustomerInput {
    def default: CustomerInput = CustomerInput(
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )
  }

  implicit val customerInputReads: Reads[CustomerInput] = (
    (__ \ "account_balance").readNullable[BigDecimal] ~
      (__ \ "coupon").readNullable[String] ~
      (__ \ "description").readNullable[String] ~
      (__ \ "email").readNullable[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "plan").readNullable[String] ~
      (__ \ "quantity").readNullable[Long] ~
      (__ \ "shipping").readNullable[Shipping] ~
      (__ \ "source").readNullable[Source] ~
      (__ \ "tax_percent").readNullable[BigDecimal] ~
      (__ \ "trial_end").readNullable[DateTime](stripeDateTimeReads)
    ).tupled.map((CustomerInput.apply _).tupled)

  implicit val customerInputWrites: Writes[CustomerInput] =
    Writes((customerInput: CustomerInput) =>
      Json.obj(
        "account_balance" -> customerInput.accountBalance,
        "coupon" -> customerInput.coupon,
        "description" -> customerInput.description,
        "email" -> customerInput.email,
        "metadata" -> customerInput.metadata,
        "plan" -> customerInput.plan,
        "quantity" -> customerInput.quantity,
        "shipping" -> customerInput.shipping,
        "source" -> customerInput.source,
        "tax_percent" -> customerInput.taxPercent,
        "trial_end" -> customerInput.trialEnd.map(x => Json.toJson(x)(stripeDateTimeWrites))
      )
    )

  def create(customerInput: CustomerInput)
            (idempotencyKey: Option[IdempotencyKey] = None)
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[Customer]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "account_balance" -> customerInput.accountBalance.map(_.toString()),
        "coupon" -> customerInput.coupon,
        "description" -> customerInput.description,
        "email" -> customerInput.email,
        "plan" -> customerInput.plan,
        "quantity" -> customerInput.quantity.map(_.toString),
        "tax_percent" -> customerInput.taxPercent.map(_.toString()),
        "trial_end" -> customerInput.trialEnd.map(stripeDateTimeParamWrites)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(customerInput.metadata, "metadata") ++ {
      customerInput.source match {
        case Some(Source.Card(
        expMonth,
        expYear,
        number,
        addressCity,
        addressCountry,
        addressLine1,
        addressLine2,
        addressState,
        addressZip,
        currency,
        cvc,
        defaultForCurrency,
        metadata,
        name
        )) =>
          /*
            TODO: metadata is missing from serialization here,
            however I don't know how to double nest objects for
            form parameters
           */

          val map = Map(
            "exp_month" -> Option(expMonth.toString),
            "exp_year" -> Option(expYear.toString),
            "number" -> Option(number),
            "address_city" -> addressCity,
            "address_country" -> addressCountry,
            "address_line1" -> addressLine1,
            "address_line2" -> addressLine2,
            "address_state" -> addressState,
            "address_zip" -> addressZip,
            "currency" -> currency.map(_.iso.toLowerCase),
            "cvc" -> cvc,
            "default_for_currency" -> defaultForCurrency.map(_.toString),
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
            val jsResult = Json.fromJson[Customer](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, Option(postFormParameters), None, jsValue, errors)
              }, customer => customer
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }
}
