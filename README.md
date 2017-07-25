
# stripe-scala, API for Stripe Using Scala

[![Build Status](https://travis-ci.org/mdedetrich/stripe-scala.svg?branch=master)](https://travis-ci.org/mdedetrich/stripe-scala)
[![Maven Central Version](https://img.shields.io/maven-central/v/org.mdedetrich/stripe-scala_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.mdedetrich%22%20AND%20a%3A%22stripe-scala_2.12%22)
[![Join the chat at https://gitter.im/mdedetrich/stripe-scala](https://badges.gitter.im/mdedetrich/stripe-scala.svg)](https://gitter.im/mdedetrich/stripe-scala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


stripe-scala is a wrapper over the [Stripe](https://stripe.com/) [REST api](https://stripe.com/docs/api/curl#intro). Unlike
[stripe-java](https://github.com/stripe/stripe-java), stripe-scala binds JSON response to the stripe object models (using Scala
case classes) and lets you create requests from typed case classes (rather than just using Java `Map<String,Object>`)

## Libraries Used
- [circe](https://circe.github.io/circe/) for JSON (circe provides compile time macros for
reading/writing JSON from/to scala case classes). It also provides a very powerful API for validating/querying JSON
- [akka-http](http://doc.akka.io/docs/akka-http/current/scala.html) for making HTTP requests
- [akka-stream-json](https://github.com/knutwalker/akka-stream-json) for streaming JSON
- [ficus](https://github.com/iheartradio/ficus) for providing config (via [typesafe-config](https://github.com/typesafehub/config))
- [enumeratum](https://github.com/lloydmeta/enumeratum) for providing typesafe enumerations on stripe enum models as well

stripe-scala was intentionally designed to use bare minimum external dependencies so its easier to integrate with scala codebases

## Installation

Currently, stripe-scala is in pre-1.0 stage. It is powering the payment processing of at least one company in production but not all endpoints are completed.

It has been deployed to Maven Central, so to install it add the following to your build definition:

```scala
libraryDependencies ++= Seq(
  "org.mdedetrich" %% "stripe-scala" % "0.2.1"
)
```

To get the latest version please check the [Maven repository search](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.mdedetrich%22%20AND%20a%3A%22stripe-scala_2.11%22).

## TODO for 1.0 release
- [ ] Add all operations for all endpoints
- [x] Add tests
- [ ] Shade jawn/enumeratum if possible. These dependencies don't need to be exposed to users
- [ ] Document Stripe API with ScalaDoc
- [x] Figure out how to deal with list collections
- [x] Figure out how to deal with error handling
- [x] Implement a single instance of all operation types to figure out if there are any potential issues
  - [x] get
  - [x] create
  - [x] update
  - [x] list
  - [x] delete
- [ ] Clean up/refactor code (still a lot of duplication)
- [x] Webhooks/Events

## Examples

There are integration tests that show how the library is intended to be used.

- [Create token for a credit card and charge it](https://github.com/mdedetrich/stripe-scala/blob/token-input/src/it/scala/org/mdedetrich/stripe/v1/ChargeIT.scala#L15)
- [Create a customer, add token and charge it](https://github.com/mdedetrich/stripe-scala/blob/master/src/it/scala/org/mdedetrich/stripe/v1/CustomerIT.scala#L18)
- [Create managed/connected account and payout money to it](https://github.com/mdedetrich/stripe-scala/blob/master/src/it/scala/org/mdedetrich/stripe/v1/AccountIT.scala#L32)
- [Payout money to a connected account](https://github.com/mdedetrich/stripe-scala/blob/master/src/it/scala/org/mdedetrich/stripe/v1/TransferIT.scala#L11)
- [Refund a charge](https://github.com/mdedetrich/stripe-scala/blob/master/src/it/scala/org/mdedetrich/stripe/v1/RefundIT.scala#L10)

## Usage
Stripe Api key and url endpoint are provided implicitly by using the `org.mdedetrich.stripe.ApiKey` and `org.mdedetrich.stripe.Endpoint`
types. The `org.mdedetrich.stripe.Config` object provides these keys through environment variables/system settings (see `application.conf`
for more details), although you can manually provide your own implicit `ApiKey` and `Endpoint` instances.

All base responses made are in the format of `Future[Try[T]]` where `T` is the model for the object being returned (i.e. creating a charge
will return a `Future[Try[Charges.Charge]]`). If there is an error in making the response that involves either invalid JSON or an error
in mapping the JSON to the models  `case class`, this will throw an exception which you need to catch
as a failed `Future` (it is by design that the models defined in stripe-scala are correct and that stripe does actually return valid JSON).

If there however is a checked error (such as an invalid API key) this will not throw an exception,
instead it will be contained within the `Try` monad (i.e. you will get a `scala.util.Failure`)

The second parameter for stripe POST requests (often named as create in stripe-scala) has an optional `idempotencyKey` which defaults
to `None`. You can specify a `IdempotencyKey` to make sure that you don't create duplicate POST requests with the same input.

stripe-scala provides `handle`/`handleIdempotent` functions which provides the typical way of dealing with stripe-errors.
It will attempt to retry the original request (using the `IdempotencyKey` to prevent duplicate side effects with `handleIdempotent`) for
errors which are deemed to be network related errors, else it will return a failed `Future`. If it
fails due to going over the retry limit, `handle`/`handleIdempotent` will also return a failed `Future` with `MaxNumberOfRetries`

```scala
import org.mdedetrich.stripe.v1.{Customers, handleIdempotent}
import scala.concurrent.Future

val customerInput: Customers.CustomerInput = ??? // Some customer input
val response: Future[Customers.Customer] = handleIdempotent(Customers.create(customerInput))
```

For the most part you will want to use `handleIdempotent`/`handle` however if you want
more fine grained control over potential errors then you can use the various `.create`/`.get` methods

### Building case classes
The Stripe object models in stripe-scala have named parameters set to default values which simplifies creating
the Stripe models

```scala
import org.mdedetrich.stripe.v1.Customers._

val expMonth = 01
val expYear = 2020
val cardNumber = "4242424242424242"
val cvc = "536"

// Inefficient way
val source = Source.Card(expMonth,
                        expYear,
                        cardNumber,
                        None,
                        None,
                        None,
                        None,
                        None,
                        None,
                        None,
                        Option(cvc),
                        None,
                        None,
                        None
                      )

// Efficient way
val source2 = Source.Card(
  expMonth = expMonth,
  expYear = expYear,
  number = cardNumber,
  cvc = Option(cvc)
)
```

### metadata

Stripe provides a metadata field which is available as an input field to most of the stripe objects. The metadata in stripe-scala
has a type of `Option[Map[String,String]]`. As you can see, the metadata is wrapped in an `Option`. This is to make working
with metadata easier.

### Timestamps

Stripe represents all of its timestamps as unix timestamp numbers (https://support.stripe.com/questions/what-timezone-does-the-dashboard-and-api-use)
however stripe-scala models store these timestamps as an `OffsetDateTime`. stripe-scala handles converting the unix timestamp
to `OffsetDateTime` and vice versa by using custom circe encoders/decoders for JSON (`defaults.stripeDateTimeDecoder`/`defaults.stripeDateTimeEncoder`) and
`stripeDateTimeParamWrites` for form parameters.

These functions are exposed publicly via the [package object](https://github.com/mdedetrich/stripe-scala/blob/master/src/main/scala/org/mdedetrich/stripe/v1/package.scala).

### Dealing with Card Errors
Since error messages from stripe are properly checked, dealing with errors like invalid CVC when adding a card are very easy to do.
Here is an example (we assume that you are using Play, but this can work with any web framework. Only `OK`,`BadRequest` and `Json.obj`
are Play related methods)

```scala

import org.mdedetrich.stripe.v1.Cards
import org.mdedetrich.stripe.v1.Errors._
import org.mdedetrich.stripe.v1.{handleIdempotent,transformParam}

import play.api.mvc // Play related import

val expMonth = 01
val expYear = 2020
val cardNumber = "4000000000000127"
val cvc = "536"

val stripeCustomerId: String = ??? // Some stripe customer Id

val cardData = Cards.CardData.Source.Object(
  expMonth = expMonth,
  expYear = expYear,
  number = cardNumber,
  cvc = Option(cvc)
)

val cardInput = Cards.CardInput(cardData)

val futureResponse = handleIdempotent(Cards.create(stripeCustomerId, cardInput)).recover {
  case Errors.Error.RequestFailed(CardError, _, Some(message), Some(param)) =>
    // We have a parameter, this usually means one of our fields is incorrect such as an invalid CVC
    BadRequest(Json.obj("message" -> List((transformParam(param), List(message)))))
  case Errors.Error.RequestFailed(CardError, _, Some(message), None) =>
    // No parameter, usually means a more general error, such as a declined card
    BadRequest(Json.obj("message" -> message))
}.map { cardData =>
  Ok(Json.toJson(cardData))
}
```

We attempt to create a card, and if it fails due to a `CardError` we use the `.recover`
method on a `Future` with pattern matching to map it to a `BadRequest`. If the request passes, we simply wrap the
card data around an `Ok`. If we don't catch something of type `CardError` we let it propagate as a failed `Future`.

One thing to note is the `transformParam` function. Since scala-stripe uses camel case instead of stripe's snake case,
returned params for error messages from stripe will use snake case (i.e. "exp_month"). `transformParam` will convert
that to a "expMonth".

If you try and run the above code (remembering to implement `stripeCustomerId`) with that credit card number
in a test environment it should return an incorrect CVC, see [stripe testing](https://stripe.com/docs/testing)
for more info.

### List collection
stripe can return items in the form a of a list which has the following format

```json
{
  "object": "list",
  "url": "/v1/customers/35/sources",
  "has_more": false,
  "data": [
    {...},
    {...}
  ]
}
```

In stripe-scala, there is a base List collection at `org.mdedetrich.stripe.v1.Collections.List` with represents
the model for the list. Other stripe objects extend `org.mdedetrich.stripe.v1.Collections.List` to provide an implementation
of the object as a list collection, i.e. `BankAccountList` for `BankAccount`

### Formatting/Style Guide
The project uses scalafmt to enforce consistent Scala formatting. Please run scalafmt before commiting your
code to github (i.e. do `scalafmt` inside of sbt)

### Testing

The project has unit and integration tests. These can be run with:

```
sbt test
sbt it:test
```
