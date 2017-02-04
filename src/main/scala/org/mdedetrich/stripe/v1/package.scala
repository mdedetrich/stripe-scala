package org.mdedetrich.stripe

import java.time.temporal.ChronoField
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.Materializer
import com.typesafe.scalalogging.Logger
import de.knutwalker.akka.http.JsonSupport._
import jawn.support.play.Parser._
import org.mdedetrich.stripe.v1.DeleteResponses.DeleteResponse
import org.mdedetrich.stripe.v1.Errors.{Error, StripeServerError, UnhandledServerError}
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util._

package object v1 {

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
  private[v1] def createRequestDELETE(finalUrl: String, idempotencyKey: Option[IdempotencyKey], logger: Logger)(
      implicit client: HttpExt,
      materializer: Materializer,
      executionContext: ExecutionContext,
      apiKey: ApiKey): Future[Try[DeleteResponse]] = {
    val authorization = Authorization(BasicHttpCredentials(apiKey.apiKey, ""))

    val req = {
      val h = idempotencyKey match {
        case Some(key) =>
          List(authorization, RawHeader(idempotencyKeyHeader, key.key))
        case None =>
          List(authorization)
      }

      HttpRequest(uri = finalUrl, method = HttpMethods.DELETE, headers = h)
    }

    for {
      response <- client.singleRequest(req)
      parsed   <- parseStripeServerError(response, finalUrl, None, None, logger)
      result = parsed match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[DeleteResponse](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.status.intValue, finalUrl, None, None, jsValue, errors)
              },
              deleteResponse => deleteResponse
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    } yield result
  }

  /**
    * A helper function which creates a GET request through akka-http
    *
    * @param finalUrl The URL for the request
    * @param logger   The logger to use, should the logger for the model for
    *                 easy debugging
    * @param reads
    * @param apiKey
    * @tparam M The model which this request should return
    * @return
    */
  private[v1] def createRequestGET[M](finalUrl: String, logger: Logger)(implicit client: HttpExt,
                                                                        materializer: Materializer,
                                                                        executionContext: ExecutionContext,
                                                                        reads: Reads[M],
                                                                        apiKey: ApiKey): Future[Try[M]] = {
    val req =
      HttpRequest(uri = finalUrl,
                  method = HttpMethods.GET,
                  headers = List(Authorization(BasicHttpCredentials(apiKey.apiKey, ""))))

    for {
      response <- client.singleRequest(req)
      parsed   <- parseStripeServerError(response, finalUrl, None, None, logger)
      result = parsed match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[M](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.status.intValue(), finalUrl, None, None, jsValue, errors)
              },
              model => model
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
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
    * @param reads
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
                                                                             reads: Reads[M],
                                                                             apiKey: ApiKey): Future[Try[M]] = {

    val formData = FormData(postFormParameters.mapValues(HttpEntity.apply)).toEntity()

    val req = {

      val authorization = Authorization(BasicHttpCredentials(apiKey.apiKey, ""))

      val headers = {
        val id = idempotencyKey match {
          case Some(key) =>
            List(authorization, RawHeader(idempotencyKeyHeader, key.key))
          case None =>
            List(authorization)
        }
        stripeAccount match {
          case Some(account) => id ++ List(RawHeader(stripeAccountHeader, account))
          case None          => id
        }

      }

      HttpRequest(uri = finalUrl, entity = formData, method = HttpMethods.POST, headers = headers)
    }

    for {
      response <- client.singleRequest(req)
      parsed   <- parseStripeServerError(response, finalUrl, Option(postFormParameters), None, logger)
      result = parsed match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[M](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.status.intValue(),
                                                finalUrl,
                                                Option(postFormParameters),
                                                None,
                                                jsValue,
                                                errors)
              },
              model => model
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }

    } yield result
  }

  private[v1] def listFilterInputToUri(createdInput: ListFilterInput,
                                       baseUrl: String,
                                       key: String): com.netaporter.uri.Uri = {
    import com.netaporter.uri.dsl._
    createdInput match {
      case c: ListFilterInput.Object =>
        baseUrl ?
          (s"$key[gt]"  -> c.gt.map(stripeDateTimeParamWrites)) ?
          (s"$key[gte]" -> c.gte.map(stripeDateTimeParamWrites)) ?
          (s"$key[lt]"  -> c.lt.map(stripeDateTimeParamWrites)) ?
          (s"$key[lte]" -> c.lte.map(stripeDateTimeParamWrites))
      case c: ListFilterInput.Timestamp =>
        baseUrl ? (s"$key" -> Option(stripeDateTimeParamWrites(c.timestamp)))
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

  val stripeDateTimeReads: Reads[OffsetDateTime] = Reads.of[Long].map { timestamp =>
    OffsetDateTime.ofInstant(Instant.ofEpochSecond(timestamp), ZoneOffset.UTC)
  }

  val stripeDateTimeWrites: Writes[OffsetDateTime] = Writes { (dateTime: OffsetDateTime) =>
    JsNumber(dateTime.get(ChronoField.OFFSET_SECONDS))
  }

  val stripeDateTimeFormats: Format[OffsetDateTime] = Format(stripeDateTimeReads, stripeDateTimeWrites)

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
            Future {
              Success {
                customer
              }
            }
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
                    Future.failed {
                      failure
                    }
                }
              case Errors.Error.TooManyRequests(_, _, _, _) =>
                responseBlockWithRetries(currentRetryCount + 1)
              case _ =>
                Future.failed {
                  failure
                }
            }
        }
      }
    }

    responseBlockWithRetries(0).flatMap {
      case Success(success) =>
        Future {
          success
        }
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
  private[v1] def parseStripeServerError(response: HttpResponse,
                                         finalUrl: String,
                                         postFormParameters: Option[Map[String, String]],
                                         postJsonParameters: Option[JsValue],
                                         logger: Logger)(
      implicit executionContext: ExecutionContext,
      materializer: Materializer): Future[Either[Errors.Error, Try[JsValue]]] = {
    val httpCode = response.status.intValue()

    logger.debug(s"Response status code is $httpCode")

    if (logger.underlying.isDebugEnabled) {
      for {
        body <- Unmarshal(response.entity).to[String]
      } yield s"Response retrieved from $finalUrl is \n$body"
    }

    for {
      result <- {
        if (response.status.isSuccess()) {
          Unmarshal(response.entity.httpEntity.withContentType(ContentTypes.`application/json`))
            .to[JsValue]
            .map(x => scala.util.Success(x))
            .recover { case e => scala.util.Failure(e) }
            .map(Right.apply)

        } else {
          httpCode match {
            case 400 | 401 | 402 | 404 | 429 =>
              for {
                json <- {
                  Unmarshal(response.entity.httpEntity.withContentType(ContentTypes.`application/json`))
                    .to[JsValue]
                    .map(x => scala.util.Success(x))
                    .recover { case e => scala.util.Failure(e) }
                }
                jsonResponse = {
                  json.map { jsValue =>
                    val path = __ \ "error"
                    val jsResult: JsResult[Error] = httpCode match {
                      case 400 =>
                        path.read[Error.BadRequest].reads(jsValue)
                      case 401 =>
                        path.read[Error.Unauthorized].reads(jsValue)
                      case 402 =>
                        path.read[Error.RequestFailed].reads(jsValue)
                      case 404 =>
                        path.read[Error.NotFound].reads(jsValue)
                      case 429 =>
                        path.read[Error.TooManyRequests].reads(jsValue)
                    }

                    jsResult.fold(
                      errors => {
                        val error =
                          InvalidJsonModelException(httpCode,
                                                    finalUrl,
                                                    postFormParameters,
                                                    postJsonParameters,
                                                    jsValue,
                                                    errors)
                        throw error
                      },
                      error => error
                    )

                  }
                }
              } yield
                Left {
                  jsonResponse match {
                    case scala.util.Success(error)     => error
                    case scala.util.Failure(throwable) => throw throwable
                  }
                }
            case 500 | 502 | 503 | 504 =>
              throw StripeServerError(response)
            case _ =>
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
