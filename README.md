# stripe-scala, API for Stripe Using Scala

[![Build Status](https://travis-ci.org/mdedetrich/stripe-scala.svg?branch=master)](https://travis-ci.org/mdedetrich/stripe-scala)

stripe-scala is a wrapper over the [Stripe](https://stripe.com/) [REST api](https://stripe.com/docs/api/curl#intro). Unlike
[stripe-java](https://github.com/stripe/stripe-java), stripe-scala binds JSON response to the stripe object models (using Scala
case classes) and lets you create requests from typed case classes (rather than just using Java `Map<String,Object>`)

## Libraries Used
- [play-json](https://www.playframework.com/documentation/2.4.x/ScalaJson) for JSON (play-json provides compile time macros for 
reading/writing JSON from/to scala case classes). It also provides a very powerful API for validating/querying JSON
- [dispatch](https://github.com/dispatch/reboot) for making HTTP requests
- [jawn](https://github.com/non/jawn) for parsing the response from dispatch to a play-json
- [ficus](https://github.com/iheartradio/ficus) for providing config (via [typesafe-config](https://github.com/typesafehub/config))
- [nscala-time](https://github.com/nscala-time/nscala-time) for date/time handling (via [JodaTime](http://www.joda.org/joda-time/))
- [enumeratum](https://github.com/lloydmeta/enumeratum) for providing typesafe enumerations on stripe enum models as well 
play-json formats for such models

stripe-scala was intentionally designed to use bare minimum external dependencies so its easier to integrate with scala codebases

## Installation

Currently, stripe-scala is in alpha stage. The models are being completed, and very few endpoints have been coded. It is being uploaded
frequently as a SNAPSHOT on sonatype

```scala
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.mdedetrich" %% "stripe-scala" % "1.0.0-SNAPSHOT"
)
```

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

The second parameter for stripe POST requests (often named as create in stripe-scala) has an optional idempotencyKey which defaults
to None. You can specify a IdempotencyKey to make sure that you don't create duplicate POST requests with the same input.

stripe-scala provides a `handleCreate` function which provides the typical way of dealing with stripe-errors.
It will attempt to retry the original request (using the `IdempotencyKey` to prevent duplicate side effects) for
errors which are deemed to be network related errors, else it will return a failed `Future`. If it
fails due to going over the retry limit, `handleCreate` will also return a failed `Future` with `MaxNumberOfRetries`

```scala
import org.mdedetrich.stripe.v1.{Customers, handleCreate}

val customerInput: Customers.CustomerInput = ??? // Some customer input
val response: Future[Customers.Customer] = handleCreate(Customers.create(customerInput))
```

For the most part you will want to use `handleCreate` and other related methods, however if you want 
more fine grained control over potential errors then you can use the various `.create`/`.get` methods
 
### Default methods
The stripe object models in stripe-scala provide a `.default` method on the companion object which simplifies creating
the stripe models

```scala
import org.mdedetrich.stripe.v1.Customers._

val expMonth = 01
val expYear = 2012
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
val source2 = Source.Card
  .default(expMonth,expYear,cardNumber)
  .copy(cvc = Option(cvc))
```
The `.default` methods create an instance of the model with all of the `Optional` fields filled as `None`. Models
that have no `Optional` fields do not have a `.default` method.

### metadata

Stripe provides a metadata field which is available as an input field to most of the stripe objects. The metadata in stripe-scala
has a type of `Option[Map[String,String]]`. As you can see, the metadata is wrapped in an `Option`. This is to make working
with metadata easier. If the map for the metadata happens to empty, the metadata will be `None`.

### Timestamps

Stripe represents all of its timestamps as unix timestamp numbers (https://support.stripe.com/questions/what-timezone-does-the-dashboard-and-api-use)
however stripe-scala models store these timestamps as a JodaTime `DateTime`. stripe-scala handles converting the unix timestamp
to `DateTime` and vice versa by using custom play-json writers/readers for JSON (`stripeDateTimeReads`/`stripeDateTimeWrites`) and
`stripeDateTimeParamWrites` for form parameters.

These functions are exposed publicly via the [package object](https://github.com/mdedetrich/stripe-scala/blob/master/src/main/scala/org/mdedetrich/stripe/v1/package.scala).