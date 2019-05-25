package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.syntax._
import org.mdedetrich.stripe.v1.Customers.Source.Token
import org.mdedetrich.stripe.v1.Discounts.Discount
import org.mdedetrich.stripe.v1.Shippings.Shipping
import org.mdedetrich.stripe.v1.Sources.NumberCardSource
import org.mdedetrich.stripe.v1.Subscriptions.SubscriptionList
import org.mdedetrich.stripe.v1.defaults._
import org.mdedetrich.stripe.{ApiKey, Endpoint, IdempotencyKey, PostParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Customers extends LazyLogging {

  final case class Customer(
      id: String,
      accountBalance: BigDecimal,
      created: OffsetDateTime,
      livemode: Boolean,
      delinquent: Boolean,
      sources: PaymentSourceList,
      subscriptions: SubscriptionList,
      currency: Option[Currency] = None,
      defaultSource: Option[String] = None,
      description: Option[String] = None,
      discount: Option[Discount] = None,
      email: Option[String] = None,
      metadata: Option[Map[String, String]] = None,
      shipping: Option[Shipping] = None
  ) extends StripeObject

  implicit val customerDecoder: Decoder[Customer] = Decoder.forProduct14(
    "id",
    "account_balance",
    "created",
    "livemode",
    "delinquent",
    "sources",
    "subscriptions",
    "currency",
    "default_source",
    "description",
    "discount",
    "email",
    "metadata",
    "shipping"
  )(Customer.apply)

  implicit val customerEncoder: Encoder[Customer] = Encoder.forProduct15(
    "id",
    "object",
    "account_balance",
    "created",
    "livemode",
    "delinquent",
    "sources",
    "subscriptions",
    "currency",
    "default_source",
    "description",
    "discount",
    "email",
    "metadata",
    "shipping"
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
      )
  )

  sealed abstract class Source

  object Source {

    final case class Token(id: String) extends Source

    final case class Card(
        expMonth: Int,
        expYear: Int,
        number: String,
        addressCity: Option[String] = None,
        addressCountry: Option[String] = None,
        addressLine1: Option[String] = None,
        addressLine2: Option[String] = None,
        addressState: Option[String] = None,
        addressZip: Option[String] = None,
        currency: Option[Currency] = None,
        cvc: Option[String] = None,
        defaultForCurrency: Option[Boolean] = None,
        metadata: Option[Map[String, String]] = None,
        name: Option[String] = None
    ) extends Source
        with NumberCardSource
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

  final case class CustomerInput(
      accountBalance: Option[BigDecimal] = None,
      coupon: Option[String] = None,
      description: Option[String] = None,
      email: Option[String] = None,
      metadata: Map[String, String] = Map.empty,
      plan: Option[String] = None,
      quantity: Option[Long] = None,
      shipping: Option[Shipping] = None,
      source: Option[Source] = None,
      taxPercent: Option[BigDecimal] = None,
      trialEnd: Option[OffsetDateTime] = None
  )

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

  final case class CustomerUpdate(
      paymentSource: Option[Token] = None,
      defaultSource: Option[String] = None
  )

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
      executionContext: ExecutionContext
  ): Future[Try[Customer]] = {
    val postFormParameters = PostParams.flatten(
      Map(
        "account_balance" -> customerInput.accountBalance.map(_.toString()),
        "coupon"          -> customerInput.coupon,
        "description"     -> customerInput.description,
        "email"           -> customerInput.email,
        "plan"            -> customerInput.plan,
        "quantity"        -> customerInput.quantity.map(_.toString),
        "tax_percent"     -> customerInput.taxPercent.map(_.toString()),
        "trial_end"       -> customerInput.trialEnd.map(stripeDateTimeParamWrites)
      )
    ) ++ PostParams.toPostParams("metadata", customerInput.metadata) ++ {
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
            )
            ) =>
          /*
            TODO: metadata is missing from serialization here,
            however I don't know how to double nest objects for
            form parameters
           */

          val map = PostParams.flatten(
            Map(
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
            )
          )
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

  def get(id: String)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Customer]] = {
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
  def getCustomerJson(id: String)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Json]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$id"

    createRequestGET[Json](finalUrl, logger)
  }

  def update(id: String, customerUpdate: CustomerUpdate)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[Customer]] = {

    val finalUrl   = endpoint.url + s"/v1/customers/$id"
    val postParams = PostParams.toPostParams(customerUpdate)
    createRequestPOST[Customer](finalUrl, postParams, idempotencyKey, logger)
  }

  def delete(id: String)(idempotencyKey: Option[IdempotencyKey] = None)(
      implicit apiKey: ApiKey,
      endpoint: Endpoint,
      client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext
  ): Future[Try[DeleteResponse]] = {
    val finalUrl = endpoint.url + s"/v1/customers/$id"

    createRequestDELETE(finalUrl, idempotencyKey, logger)
  }

  final case class CustomerListInput(
      created: Option[ListFilterInput] = None,
      endingBefore: Option[String] = None,
      limit: Option[Long] = None,
      startingAfter: Option[String] = None
  )

  final case class CustomerList(
      override val url: String,
      override val hasMore: Boolean,
      override val data: List[Customer],
      override val totalCount: Option[Long]
  ) extends Collections.List[Customer](url, hasMore, data, totalCount)

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
      executionContext: ExecutionContext
  ): Future[Try[CustomerList]] = {
    val finalUrl = {
      val totalCountUrl =
        if (includeTotalCount)
          "/include[]=total_count"
        else
          ""

      val baseUrl = endpoint.url + s"/v1/customers$totalCountUrl"

      val created: Uri = customerListInput.created match {
        case Some(createdInput) =>
          listFilterInputToUri(createdInput, baseUrl, "created")
        case None => baseUrl
      }

      val queries = PostParams.flatten(
        List(
          "ending_before"  -> customerListInput.endingBefore,
          "limit"          -> customerListInput.limit.map(_.toString),
          "starting_after" -> customerListInput.startingAfter
        )
      )

      val query = queries.foldLeft(created.query())((a, b) => b +: a)
      created.withQuery(query)
    }

    createRequestGET[CustomerList](finalUrl, logger)
  }
}
