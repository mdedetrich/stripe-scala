package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import com.typesafe.scalalogging.LazyLogging
import org.mdedetrich.playjson.Utils._
import org.mdedetrich.stripe.v1.Customers.Source.Token
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.v1.Discounts.Discount
import org.mdedetrich.stripe.v1.PaymentSourceList._
import org.mdedetrich.stripe.v1.Shippings.Shipping
import org.mdedetrich.stripe.v1.Sources.NumberCardSource
import org.mdedetrich.stripe.v1.Subscriptions.Subscription
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

object Customers extends LazyLogging {

  case class Customer(id: String,
                      accountBalance: BigDecimal,
                      created: OffsetDateTime,
                      currency: Option[Currency],
                      defaultSource: Option[String],
                      delinquent: Boolean,
                      description: Option[String],
                      discount: Option[Discount],
                      email: Option[String],
                      livemode: Boolean,
                      metadata: Option[Map[String, String]],
                      shipping: Shipping,
                      sources: PaymentSourceList,
                      subscriptions: List[Subscription])
      extends StripeObject

  object Customer {
    def default(id: String,
                accountBalance: BigDecimal,
                created: OffsetDateTime,
                currency: Currency,
                delinquent: Boolean,
                livemode: Boolean,
                shipping: Shipping,
                sources: PaymentSourceList,
                subscriptions: List[Subscription]): Customer = Customer(
      id,
      accountBalance,
      created,
      None,
      None,
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
      (__ \ "created").read[OffsetDateTime](stripeDateTimeReads) ~
      (__ \ "currency").readNullable[Currency] ~
      (__ \ "default_source").readNullable[String] ~
      (__ \ "delinquent").read[Boolean] ~
      (__ \ "description").readNullable[String] ~
      (__ \ "discount").readNullable[Discount] ~
      (__ \ "email").readNullable[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "shipping").read[Shipping] ~
      (__ \ "sources").read[PaymentSourceList] ~
      (__ \ "subscriptions" \ "data").read[List[Subscription]]
  ).tupled.map((Customer.apply _).tupled)

  implicit val customerWrites: Writes[Customer] = Writes(
    (customer: Customer) =>
      Json.obj(
        "id"              -> customer.id,
        "object"          -> "customer",
        "account_balance" -> customer.accountBalance,
        "created"         -> Json.toJson(customer.created)(stripeDateTimeWrites),
        "currency"        -> customer.currency,
        "default_source"  -> customer.defaultSource,
        "delinquent"      -> customer.delinquent,
        "description"     -> customer.description,
        "discount"        -> customer.discount,
        "email"           -> customer.email,
        "livemode"        -> customer.livemode,
        "metadata"        -> customer.metadata,
        "shipping"        -> customer.shipping,
        "sources"         -> customer.sources,
        "subscriptions"   -> customer.subscriptions
    ))

  sealed abstract class Source

  object Source {

    case class Token(id: String) extends Source

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
                    name: Option[String])
        extends Source
        with NumberCardSource

    object Card {
      def default(expMonth: Int, expYear: Int, number: String): Card = Card(
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
        __.read[String].map { tokenId =>
          Source.Token(tokenId)
        }
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
            "object"               -> "card",
            "exp_month"            -> expMonth,
            "exp_year"             -> expYear,
            "number"               -> number,
            "address_city"         -> addressCity,
            "address_country"      -> addressCountry,
            "address_line1"        -> addressLine1,
            "address_line2"        -> addressLine2,
            "address_state"        -> addressState,
            "address_zip"          -> addressZip,
            "currency"             -> currency,
            "cvc"                  -> cvc,
            "default_for_currency" -> defaultForCurrency,
            "metadata"             -> metadata,
            "name"                 -> name
          )
      }
    })
  }

  case class CustomerInput(accountBalance: Option[BigDecimal],
                           coupon: Option[String],
                           description: Option[String],
                           email: Option[String],
                           metadata: Map[String, String],
                           plan: Option[String],
                           quantity: Option[Long],
                           shipping: Option[Shipping],
                           source: Option[Source],
                           taxPercent: Option[BigDecimal],
                           trialEnd: Option[OffsetDateTime])

  object CustomerInput {
    def default: CustomerInput = CustomerInput(
      None,
      None,
      None,
      None,
      Map.empty,
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
      (__ \ "metadata").read[Map[String, String]] ~
      (__ \ "plan").readNullable[String] ~
      (__ \ "quantity").readNullable[Long] ~
      (__ \ "shipping").readNullable[Shipping] ~
      (__ \ "source").readNullable[Source] ~
      (__ \ "tax_percent").readNullable[BigDecimal] ~
      (__ \ "trial_end").readNullable[OffsetDateTime](stripeDateTimeReads)
  ).tupled.map((CustomerInput.apply _).tupled)

  implicit val customerInputWrites: Writes[CustomerInput] = Writes(
    (customerInput: CustomerInput) =>
      Json.obj(
        "account_balance" -> customerInput.accountBalance,
        "coupon"          -> customerInput.coupon,
        "description"     -> customerInput.description,
        "email"           -> customerInput.email,
        "metadata"        -> customerInput.metadata,
        "plan"            -> customerInput.plan,
        "quantity"        -> customerInput.quantity,
        "shipping"        -> customerInput.shipping,
        "source"          -> customerInput.source,
        "tax_percent"     -> customerInput.taxPercent,
        "trial_end"       -> customerInput.trialEnd.map(x => Json.toJson(x)(stripeDateTimeWrites))
    ))

  case class CustomerUpdate(
      paymentSource: Option[Token]
  )

  implicit val customerUpdatePostParams = new PostParams[CustomerUpdate] {
    override def toMap(t: CustomerUpdate): Map[String, String] =
      Map("source" -> t.paymentSource.map(_.id)).collect({ case (key, Some(value)) => (key, value) })
  }

  // CRUD methods

  def create(customerInput: CustomerInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[Customer]] = {
    val postFormParameters: Map[String, String] = {
      Map(
        "account_balance" -> customerInput.accountBalance.map(_.toString()),
        "coupon"          -> customerInput.coupon,
        "description"     -> customerInput.description,
        "email"           -> customerInput.email,
        "plan"            -> customerInput.plan,
        "quantity"        -> customerInput.quantity.map(_.toString),
        "tax_percent"     -> customerInput.taxPercent.map(_.toString()),
        "trial_end"       -> customerInput.trialEnd.map(stripeDateTimeParamWrites)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ PostParams.toPostParams("metadata", customerInput.metadata) ++ {
      customerInput.source match {
        case Some(
            Source.Card(
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
            "exp_month"            -> Option(expMonth.toString),
            "exp_year"             -> Option(expYear.toString),
            "number"               -> Option(number),
            "address_city"         -> addressCity,
            "address_country"      -> addressCountry,
            "address_line1"        -> addressLine1,
            "address_line2"        -> addressLine2,
            "address_state"        -> addressState,
            "address_zip"          -> addressZip,
            "currency"             -> currency.map(_.iso.toLowerCase),
            "cvc"                  -> cvc,
            "default_for_currency" -> defaultForCurrency.map(_.toString),
            "name"                 -> name
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

    createRequestPOST[Customer](finalUrl, postFormParameters, idempotencyKey, logger)
  }

  def get(id: String)(implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[Customer]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$id"

    createRequestGET[Customer](finalUrl, logger)
  }

  /**
    * API call which does no parsing but returns the Stripe customer API response as is.
    *
    * This is useful for when you want to use the Stripe Mobile SDK and parse the API response on the device in order
    * to use the UI for selecting a payment method.
    *
    * @see https://stripe.github.io/stripe-ios/docs/Protocols/STPBackendAPIAdapter.html
    */
  def getCustomerJson(id: String)(implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[JsValue]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$id"

    createRequestGET[JsValue](finalUrl, logger)
  }

  def update(id: String, customerUpdate: CustomerUpdate)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[Customer]] = {

    val finalUrl   = endpoint.url + s"/v1/customers/$id"
    val postParams = PostParams.toPostParams(customerUpdate)
    createRequestPOST[Customer](finalUrl, postParams, idempotencyKey, logger)
  }

  def delete(id: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint): Future[Try[DeleteResponse]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$id"

    createRequestDELETE(finalUrl, idempotencyKey, logger)
  }

  case class CustomerListInput(created: Option[ListFilterInput],
                               endingBefore: Option[String],
                               limit: Option[Long],
                               startingAfter: Option[String])

  object CustomerListInput {
    def default: CustomerListInput = CustomerListInput(
      None,
      None,
      None,
      None
    )
  }

  case class CustomerList(override val url: String,
                          override val hasMore: Boolean,
                          override val data: List[Customer],
                          override val totalCount: Option[Long])
      extends Collections.List[Customer](url, hasMore, data, totalCount)

  object CustomerList extends Collections.ListJsonMappers[Customer] {
    implicit val customerListReads: Reads[CustomerList] =
      listReads.tupled.map((CustomerList.apply _).tupled)

    implicit val customerWrites: Writes[CustomerList] = listWrites
  }

  def list(customerListInput: CustomerListInput,
           includeTotalCount: Boolean)(implicit apiKey: ApiKey, endpoint: Endpoint): Future[Try[CustomerList]] = {
    val finalUrl = {
      import com.netaporter.uri.dsl._
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/customers$totalCountUrl"

      val created: com.netaporter.uri.Uri = customerListInput.created match {
        case Some(createdInput) =>
          listFilterInputToUri(createdInput, baseUrl, "created")
        case None => baseUrl
      }

      (created ?
        ("ending_before" -> customerListInput.endingBefore) ?
        ("limit" -> customerListInput.limit.map(_.toString)) ?
        ("starting_after" -> customerListInput.startingAfter)).toString()
    }

    createRequestGET[CustomerList](finalUrl, logger)
  }
}
