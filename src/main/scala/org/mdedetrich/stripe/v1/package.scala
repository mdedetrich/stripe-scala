package org.mdedetrich.stripe

import java.time.temporal.ChronoField
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.Materializer
import com.typesafe.scalalogging.Logger
import de.knutwalker.akka.http.support.CirceHttpSupport._
import de.knutwalker.akka.stream.support.CirceStreamSupport
import io.circe.syntax._
import io.circe.{Errors => _, _}
import org.mdedetrich.stripe.v1.Errors.{Error, StripeServerError, UnhandledServerError}

import scala.concurrent.{ExecutionContext, Future}
import scala.util._

package object v1 {

  object defaults {

    implicit val stripeDateTimeDecoder: Decoder[OffsetDateTime] = Decoder[Long].map { timestamp =>
      OffsetDateTime.ofInstant(Instant.ofEpochSecond(timestamp), ZoneOffset.UTC)
    }

    implicit val stripeDateTimeEncoder: Encoder[OffsetDateTime] = Encoder.instance[OffsetDateTime] { dateTime =>
      dateTime.get(ChronoField.OFFSET_SECONDS).asJson
    }

  }

  /**
    * A helper function which creates a DELETE request through akka-http.
    * Note that DELETE requests in stripe all have the same response
    *
    * @param finalUrl       The URL for the request
    * @param idempotencyKey The logger to use, should the logger for the model for
    *                       easy debugging
    * @param logger
    * @param apiKey
    * @return
    */
  private[v1] def createRequestDELETE(finalUrl: Uri, idempotencyKey: Option[IdempotencyKey], logger: Logger)(
      implicit client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext,
      apiKey: ApiKey): Future[Try[DeleteResponse]] = {

    val headers = buildHeaders(apiKey, None, idempotencyKey)

    val req = HttpRequest(uri = finalUrl, method = HttpMethods.DELETE, headers = headers)

    for {
      response <- client.singleRequest(req)
      _ = logger.debug(s"Received response $response")
      parsed <- parseStripeServerError[DeleteResponse](response, finalUrl, None, None, logger)
      result = parsed match {
        case Right(triedDeleteResponse) =>
          util.Success(triedDeleteResponse.get)
        case Left(error) =>
          util.Failure(error)
      }
    } yield result
  }

  /**
    * A helper function which creates a GET request through akka-http
    *
    * @param finalUrl The URL for the request
    * @param logger   The logger to use, should the logger for the model for
    *                 easy debugging
    * @param decoder
    * @param apiKey
    * @tparam M The model which this request should return
    * @return
    */
  private[v1] def createRequestGET[M](finalUrl: Uri, logger: Logger, stripeAccount: Option[String] = None)(
      implicit client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext,
      decoder: Decoder[M],
      apiKey: ApiKey): Future[Try[M]] = {

    val headers = buildHeaders(apiKey, stripeAccount, None)
    val req =
      HttpRequest(uri = finalUrl, method = HttpMethods.GET, headers = headers)

    for {
      response <- client.singleRequest(req)
      _ = logger.debug(s"Received response $response")
      parsed <- parseStripeServerError[M](response, finalUrl, None, None, logger)
      result = parsed match {
        case Right(triedValue) =>
          util.Success(triedValue.get)
        case Left(error) =>
          util.Failure(error)
      }
    } yield result
  }

  /**
    * A helper function which creates a POST request through akka-http
    *
    * @param finalUrl           The URL for the request
    * @param postFormParameters The POST form parameters
    * @param idempotencyKey     The IdempotencyKey
    * @param logger             The logger to use, should the logger for the model for
    *                           easy debugging
    * @param decoder
    * @param apiKey
    * @tparam M The model which this request should return
    * @return
    */
  private[v1] def createRequestPOST[M](finalUrl: String,
                                       postFormParameters: Map[String, String],
                                       idempotencyKey: Option[IdempotencyKey],
                                       logger: Logger,
                                       stripeAccount: Option[String] = None)(implicit client: HttpExt,
                                                                             materializer: Materializer,
                                                                             executionContext: ExecutionContext,
                                                                             decoder: Decoder[M],
                                                                             apiKey: ApiKey): Future[Try[M]] = {

    val headers = buildHeaders(apiKey, stripeAccount, idempotencyKey)

    val req = HttpRequest(
      uri = finalUrl,
      entity = FormData(postFormParameters).toEntity,
      method = HttpMethods.POST,
      headers = headers
    )

    for {
      response <- client.singleRequest(req)
      _ = logger.debug(s"Received response $response")
      parsed <- parseStripeServerError[M](response, finalUrl, Option(postFormParameters), None, logger)
      result = parsed match {
        case Right(triedValue) =>
          util.Success(triedValue.get)
        case Left(error) =>
          util.Failure(error)
      }

    } yield result
  }

  private[v1] def listFilterInputToUri(createdInput: ListFilterInput, baseUrl: Uri, key: String): Uri = {
    createdInput match {
      case c: ListFilterInput.Object =>
        val query = PostParams.flatten(
          Map(
            s"$key[gt]"  -> c.gt.map(stripeDateTimeParamWrites),
            s"$key[gte]" -> c.gte.map(stripeDateTimeParamWrites),
            s"$key[lt]"  -> c.lt.map(stripeDateTimeParamWrites),
            s"$key[lte]" -> c.lte.map(stripeDateTimeParamWrites)
          ))

        baseUrl.withQuery(Query(query))
      case c: ListFilterInput.Timestamp =>
        baseUrl.withQuery(Query(s"$key" -> stripeDateTimeParamWrites(c.timestamp)))
    }
  }

  /**
    * Transforms a param from Stripes naming scheme (snake case) to scala-stripe's naming scheme (camel case).
    * Often used when dealing with stripe error messages for invalid fields, such as invalid CVC
    * Code taken from https://gist.github.com/sidharthkuruvila/3154845
    *
    * @param param
    * @return
    */
  def transformParam(param: String): String = {
    "_([a-z\\d])".r.replaceAllIn(param, { m =>
      m.group(1).toUpperCase()
    })
  }

  // Stripe stores timestamps in Unix time https://support.stripe.com/questions/what-timezone-does-the-dashboard-and-api-use

  def stripeDateTimeParamWrites(dateTime: OffsetDateTime): String =
    dateTime.get(ChronoField.OFFSET_SECONDS).toString

  /**
    * A function which does the simplest ideal handling for making a stripe request.
    * It handles specific stripe errors, and will retry the request for errors that
    * indicate some sort of network error. It uses the Stripe idempotency key to make
    * sure that duplicate side effects (such as creating multiple charges) do not happen
    *
    * @param request         The request that you are making with Stripe
    * @param numberOfRetries Number of retries, provided by default in [[org.mdedetrich.stripe.Config]]
    * @tparam T The returning Stripe object for the request
    * @return
    */
  def handleIdempotent[T](
      request: => Option[IdempotencyKey] => Future[Try[T]],
      numberOfRetries: Int = Config.numberOfRetries)(implicit executionContext: ExecutionContext): Future[T] = {

    val idempotencyKey = Option(IdempotencyKey(java.util.UUID.randomUUID.toString))

    handle(request(idempotencyKey), numberOfRetries)
  }

  /**
    * A function which does the simplest ideal handling for making a stripe request.
    * It handles specific stripe errors, and will retry the request for errors that
    * indicate some sort of network error.
    *
    * @param request         The request that you are making with Stripe
    * @param numberOfRetries Number of retries, provided by default in [[org.mdedetrich.stripe.Config]]
    * @tparam T The returning Stripe object for the request
    * @return
    */
  def handle[T](request: Future[Try[T]], numberOfRetries: Int = Config.numberOfRetries)(
      implicit executionContext: ExecutionContext): Future[T] = {
    def responseBlock = request

    def responseBlockWithRetries(currentRetryCount: Int): Future[Try[T]] = {
      if (currentRetryCount > numberOfRetries) {
        Future.failed {
          MaxNumberOfRetries(currentRetryCount)
        }
      } else {
        responseBlock.flatMap {
          case scala.util.Success(customer) =>
            Future.successful(Success(customer))
          case scala.util.Failure(failure) =>
            failure match {
              case Errors.Error.RequestFailed(error, _, _, _) =>
                // According to documentation, these errors imply some sort of network error so we should retry
                error match {
                  case Errors.Type.ApiError =>
                    responseBlockWithRetries(currentRetryCount + 1)
                  case Errors.Type.ApiConnectionError =>
                    responseBlockWithRetries(currentRetryCount + 1)
                  case _ =>
                    Future.failed(failure)
                }
              case Errors.Error.TooManyRequests(_, _, _, _) =>
                responseBlockWithRetries(currentRetryCount + 1)
              case _ =>
                Future.failed(failure)
            }
        }
      }
    }

    responseBlockWithRetries(0).flatMap {
      case Success(success) =>
        Future.successful(success)
      case Failure(throwable) =>
        Future.failed(throwable)
    }
  }

  /**
    * This is a header constant to specify a Idempotency-Key
    */
  private[v1] val idempotencyKeyHeader = "Idempotency-Key"

  /**
    * Header to specify on behalf of which stripe account this API call should be executed.
    */
  private[v1] val stripeAccountHeader = "Stripe-Account"

  /**
    * Which version of the API this call is for.
    */
  private[v1] val stripeVersionHeader = "Stripe-Version"

  private def buildHeaders(apiKey: ApiKey,
                           stripeAccount: Option[String],
                           idempotencyKey: Option[IdempotencyKey]): List[HttpHeader] =
    List(
      stripeAccount.map(a => RawHeader(stripeAccountHeader, a)),
      Some(Authorization(BasicHttpCredentials(apiKey.apiKey, ""))),
      idempotencyKey.map(i => RawHeader(idempotencyKeyHeader, i.key))
    ).flatten

  /**
    * Parses a response from dispatch and attempts to do error process handling for specific stripe errors
    *
    * @param response
    * @param finalUrl
    * @param postFormParameters
    * @param postJsonParameters
    * @return Will return a [[Left]] if we catch one of the handled errors that are described at
    *         https://stripe.com/docs/api/curl#errors. Will return a [[Right]] if no server errors
    *         are made. Will throw an [[UnhandledServerError]] or [[StripeServerError]] for uncaught errors.
    */
  private[v1] def parseStripeServerError[A](response: HttpResponse,
                                            finalUrl: Uri,
                                            postFormParameters: Option[Map[String, String]],
                                            postJsonParameters: Option[Json],
                                            logger: Logger)(implicit executionContext: ExecutionContext,
                                                            materializer: Materializer,
                                                            decoder: Decoder[A]): Future[Either[Error, Try[A]]] = {
    val httpCode = response.status.intValue()

    logger.debug(s"Response status code is $httpCode")

    for {
      result <- {
        if (response.status.isSuccess()) {
          Unmarshal(response.entity.httpEntity.withContentType(ContentTypes.`application/json`))
            .to[A]
            .map(x => Right(util.Success(x)))
            .recover {
              case e: CirceStreamSupport.JsonParsingException => Right(util.Failure(e))
            }
        } else {
          httpCode match {
            case 400 | 401 | 402 | 404 | 429 =>
              for {
                json <- {
                  Unmarshal(response.entity.httpEntity.withContentType(ContentTypes.`application/json`))
                    .to[Json]
                    .map(x => util.Success(x))
                    .recover { case e => util.Failure(e) }
                }
                jsonResponse = {
                  json.map { jsValue =>
                    val jsResult: Decoder.Result[Error] = httpCode match {
                      case 400 =>
                        val decoder: Decoder[Error.BadRequest] = Decoder.instance[Error.BadRequest] { c =>
                          c.downField("error").as[Error.BadRequest]
                        }
                        decoder.apply(jsValue.hcursor)
                      case 401 | 403 =>
                        val decoder: Decoder[Error.Unauthorized] = Decoder.instance[Error.Unauthorized] { c =>
                          c.downField("error").as[Error.Unauthorized]
                        }
                        decoder.apply(jsValue.hcursor)
                      case 402 =>
                        val decoder: Decoder[Error.RequestFailed] = Decoder.instance[Error.RequestFailed] { c =>
                          c.downField("error").as[Error.RequestFailed]
                        }
                        decoder.apply(jsValue.hcursor)
                      case 404 =>
                        val decoder: Decoder[Error.NotFound] = Decoder.instance[Error.NotFound] { c =>
                          c.downField("error").as[Error.NotFound]
                        }
                        decoder.apply(jsValue.hcursor)
                      case 429 =>
                        val decoder: Decoder[Error.TooManyRequests] = Decoder.instance[Error.TooManyRequests] { c =>
                          c.downField("error").as[Error.TooManyRequests]
                        }
                        decoder.apply(jsValue.hcursor)
                    }

                    jsResult.fold(
                      error => {
                        throw InvalidJsonModelException(httpCode,
                                                        finalUrl,
                                                        postFormParameters,
                                                        postJsonParameters,
                                                        jsValue,
                                                        error)
                      },
                      error => error
                    )

                  }
                }
              } yield
                Left {
                  jsonResponse match {
                    case util.Success(error)     => error
                    case util.Failure(throwable) => throw throwable
                  }
                }
            case 500 | 502 | 503 | 504 =>
              response.discardEntityBytes()
              throw StripeServerError(response)
            case _ =>
              response.discardEntityBytes()
              throw UnhandledServerError(response)
          }
        }
      }
    } yield result
  }

  private[v1] def mapToPostParams(optionalMap: Option[Map[String, String]], parentKey: String) = {
    optionalMap match {
      case Some(map) =>
        map.map {
          case (key, value) =>
            s"$parentKey[$key]" -> value
        }
      case None =>
        Map.empty
    }
  }
}
