package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.stream.Materializer
import cats.syntax.either._
import defaults._
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.syntax._
import org.mdedetrich.stripe.v1.Customers.Source.Token
import org.mdedetrich.stripe.v1.Discounts.Discount
import org.mdedetrich.stripe.v1.Shippings.Shipping
import org.mdedetrich.stripe.v1.Sources.NumberCardSource
import org.mdedetrich.stripe.v1.Subscriptions.SubscriptionList
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
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
                      shipping: Option[Shipping],
                      sources: PaymentSourceList,
                      subscriptions: SubscriptionList)
      extends StripeObject

  object Customer {
    def default(id: String,
                accountBalance: BigDecimal,
                created: OffsetDateTime,
                currency: Currency,
                delinquent: Boolean,
                livemode: Boolean,
                sources: PaymentSourceList,
                subscriptions: SubscriptionList): Customer = Customer(
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
      None,
      sources,
      subscriptions
    )
  }

  implicit val customerDecoder: Decoder[Customer] = Decoder.forProduct14(
    "id",
    "account_balance",
    "created",
    "currency",
    "default_source",
    "delinquent",
    "description",
    "discount",
    "email",
    "livemode",
    "metadata",
    "shipping",
    "sources",
    "subscriptions"
  )(Customer.apply)

  implicit val customerEncoder: Encoder[Customer] = Encoder.forProduct15(
    "id",
    "object",
    "account_balance",
    "created",
    "currency",
    "default_source",
    "delinquent",
    "description",
    "discount",
    "email",
    "livemode",
    "metadata",
    "shipping",
    "sources",
    "subscriptions"
  )(
    x =>
      (
        x.id,
        "customer",
        x.accountBalance,
        x.created,
        x.currency,
        x.defaultSource,
        x.delinquent,
        x.description,
        x.discount,
        x.email,
        x.livemode,
        x.metadata,
        x.shipping,
        x.sources,
        x.subscriptions
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

  implicit val sourceDecoder: Decoder[Source] = Decoder.instance[Source] { c =>
    c.as[JsonObject] match {
      case Left(_) =>
        c.as[String].map(Source.Token.apply)
      case Right(_) =>
        val decoder: Decoder[Source.Card] = Decoder.forProduct14(
          "exp_month",
          "exp_year",
          "number",
          "address_city",
          "address_country",
          "address_line1",
          "address_line2",
          "address_state",
          "address_zip",
          "currency",
          "cvc",
          "default_for_currency",
          "metadata",
          "name"
        )(Source.Card.apply)
        decoder.apply(c)
    }
  }

  implicit val sourceEncoder: Encoder[Source] = Encoder.instance[Source] {
    case Source.Token(id) => id.asJson
    case card: Source.Card =>
      val encoder: Encoder[Source.Card] = Encoder.forProduct14(
        "exp_month",
        "exp_year",
        "number",
        "address_city",
        "address_country",
        "address_line1",
        "address_line2",
        "address_state",
        "address_zip",
        "currency",
        "cvc",
        "default_for_currency",
        "metadata",
        "name"
      )(x => Source.Card.unapply(x).get)
      encoder.apply(card)
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

  implicit val customerInputDecoder: Decoder[CustomerInput] = Decoder.forProduct11(
    "account_balance",
    "coupon",
    "description",
    "email",
    "metadata",
    "plan",
    "quantity",
    "shipping",
    "source",
    "tax_percent",
    "trial_end"
  )(CustomerInput.apply)

  implicit val customerInputEncoder: Encoder[CustomerInput] = Encoder.forProduct11(
    "account_balance",
    "coupon",
    "description",
    "email",
    "metadata",
    "plan",
    "quantity",
    "shipping",
    "source",
    "tax_percent",
    "trial_end"
  )(x => CustomerInput.unapply(x).get)

  case class CustomerUpdate(
      paymentSource: Option[Token],
      defaultSource: Option[String]
  )

  object CustomerUpdate {
    def default: CustomerUpdate = CustomerUpdate(None, None)
  }

  implicit val customerUpdatePostParams: PostParams[CustomerUpdate] = PostParams.params[CustomerUpdate] { t =>
    val params = Map(
      "source"         -> t.paymentSource.map(_.id),
      "default_source" -> t.defaultSource
    )
    PostParams.flatten(params)
  }

  // CRUD methods

  def create(customerInput: CustomerInput)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[Customer]] = {
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

  def get(id: String)(implicit apiKey: ApiKey,
                      endpoint: Endpoint,
                      client: HttpExt,
                      materializer: Materializer,
                      executionContext: ExecutionContext): Future[Try[Customer]] = {
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
  def getCustomerJson(id: String)(implicit apiKey: ApiKey,
                                  endpoint: Endpoint,
                                  client: HttpExt,
                                  materializer: Materializer,
                                  executionContext: ExecutionContext): Future[Try[Json]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$id"

    createRequestGET[Json](finalUrl, logger)
  }

  def update(id: String, customerUpdate: CustomerUpdate)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[Customer]] = {

    val finalUrl   = endpoint.url + s"/v1/customers/$id"
    val postParams = PostParams.toPostParams(customerUpdate)
    createRequestPOST[Customer](finalUrl, postParams, idempotencyKey, logger)
  }

  def delete(id: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[DeleteResponse]] = {
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
    implicit val customerListDecoder: Decoder[CustomerList] =
      listDecoder(implicitly)(CustomerList.apply)

    implicit val customerListEncoder: Encoder[CustomerList] =
      listEncoder[CustomerList]
  }

  def list(customerListInput: CustomerListInput, includeTotalCount: Boolean)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext): Future[Try[CustomerList]] = {
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
        ("ending_before"  -> customerListInput.endingBefore) ?
        ("limit"          -> customerListInput.limit.map(_.toString)) ?
        ("starting_after" -> customerListInput.startingAfter)).toString()
    }

    createRequestGET[CustomerList](finalUrl, logger)
  }
}
