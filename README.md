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

stripe-scala was intentionally designed to use bare minimum external dependencies so its easier to integrate with scala codebases

## Installation

Currently, stripe-scala is in alpha stage. The models are being completed, and very few endpoints have been coded. It is being uploaded
frequently as a SNAPSHOT on sonatype

```
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
 
All responses made are in the format of `Future[Try[T]]` where `T` is the model for the object being returned (i.e. creating a charge
will return a `Future[Try[Charges.Charge]]`). If there is an error in making the response that involves either invalid JSON or an error
in mapping the JSON to the models  `case class`, this will throw an exception which you need to catch 
(it is by design that the models defined in stripe-scala are correct and that stripe does actually return valid JSON).

If there however is a checked error (such as an invalid API key) this will not throw an exception, 
instead it will be contained within the `Try` monad (i.e. you will get a `scala.util.Failure`)
