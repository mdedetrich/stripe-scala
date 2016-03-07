package org.mdedetrich.stripe

import com.ning.http.client.Response
import com.typesafe.scalalogging.Logger
import jawn.support.play.Parser
import org.joda.time.DateTime
import org.mdedetrich.stripe.v1.Errors.{Error, StripeServerError, UnhandledServerError}
import play.api.libs.json._
import scala.concurrent.{ExecutionContext, Future}
import scala.util._

package object v1 {
  /**
    * Transforms a param from Stripes naming scheme (snake case) to scala-stripe's naming scheme (camel case).
    * Often used when dealing with stripe error messages for invalid fields, such as invalid CVC
    * Code taken from https://gist.github.com/sidharthkuruvila/3154845
    * @param param
    * @return
    */
  
  def transformParam(param: String): String = {
    "_([a-z\\d])".r.replaceAllIn(param, {m =>
      m.group(1).toUpperCase()
    })
  }

  // Stripe stores timestamps in Unix time https://support.stripe.com/questions/what-timezone-does-the-dashboard-and-api-use

  def stripeDateTimeParamWrites(dateTime: DateTime): String = (dateTime.getMillis / 1000).toString

  val stripeDateTimeReads: Reads[DateTime] =
    Reads.of[Long].map { timestamp => new DateTime(timestamp * 1000) }

  val stripeDateTimeWrites: Writes[DateTime] =
    Writes { (dateTime: DateTime) =>
      JsNumber(dateTime.getMillis / 1000)
    }

  val stripeDateTimeFormats: Format[DateTime] = Format(stripeDateTimeReads, stripeDateTimeWrites)

  /**
    * A function which does the simplest ideal handling for making a stripe request.
    * It handles specific stripe errors, and will retry the request for errors that
    * indicate some sought of network error. It uses the Stripe idempotency key to make
    * sure that duplicate side effects (such as creating multiple charges) do not happen
    *
    * @param request         The request that you are making with Stripe
    * @param numberOfRetries Number of retries, provided by default in [[org.mdedetrich.stripe.Config]]
    * @tparam T The returning Stripe object for the request
    * @return
    */

  def handleIdempotent[T](request: => Option[IdempotencyKey] => Future[Try[T]],
                          numberOfRetries: Int = Config.numberOfRetries
                         )
                         (implicit executionContext: ExecutionContext): Future[T] = {

    val idempotencyKey = Option(IdempotencyKey(java.util.UUID.randomUUID.toString))

    handle(request(idempotencyKey), numberOfRetries)
  }


  /**
    * A function which does the simplest ideal handling for making a stripe request.
    * It handles specific stripe errors, and will retry the request for errors that
    * indicate some sought of network error.
    *
    * @param request         The request that you are making with Stripe
    * @param numberOfRetries Number of retries, provided by default in [[org.mdedetrich.stripe.Config]]
    * @tparam T The returning Stripe object for the request
    * @return
    */


  def handle[T](request: Future[Try[T]],
                numberOfRetries: Int = Config.numberOfRetries
               )(implicit executionContext: ExecutionContext): Future[T] = {
    def responseBlock = request

    def responseBlockWithRetries(currentRetryCount: Int): Future[Try[T]] = {
      if (currentRetryCount > numberOfRetries) {
        Future.failed {
          MaxNumberOfRetries(currentRetryCount)
        }
      } else {
        responseBlock.flatMap {
          case scala.util.Success(customer) => Future {
            Success {
              customer
            }
          }
          case scala.util.Failure(failure) =>
            failure match {
              case Errors.Error.RequestFailed(error, _, _, _) =>

                /**
                  * According to documentation, these errors imply some sought of network error
                  * so we should retry
                  */
                error match {
                  case Errors.Type.ApiError => responseBlockWithRetries(currentRetryCount + 1)
                  case Errors.Type.ApiConnectionError => responseBlockWithRetries(currentRetryCount + 1)
                  case _ => Future.failed {
                    failure
                  }
                }
              case Errors.Error.TooManyRequests(_, _, _, _) =>
                responseBlockWithRetries(currentRetryCount + 1)
              case _ => Future.failed {
                failure
              }
            }
        }
      }
    }

    responseBlockWithRetries(0).flatMap {
      case Success(success) => Future {
        success
      }
      case Failure(throwable) => Future.failed(throwable)
    }
  }

  /**
    * This is a header constat to specify a Idempotency-Key
    */

  val idempotencyKeyHeader = "Idempotency-Key"


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

  def parseStripeServerError(response: Response,
                             finalUrl: String,
                             postFormParameters: Option[Map[String, String]],
                             postJsonParameters: Option[JsValue])
                            (implicit logger: Logger): Either[Errors.Error, Try[JsValue]] = {
    val httpCode = response.getStatusCode

    logger.debug(s"Response status code is $httpCode")

    logger.debug(s"Response retrieved from $finalUrl is \n${
      response.getResponseBody
    }")

    httpCode match {
      case code if code / 100 == 2 =>
        Right(Parser.parseFromByteBuffer(response.getResponseBodyAsByteBuffer))
      case 400 | 401 | 402 | 404 | 429 =>
        val jsonResponse = Parser.parseFromByteBuffer(response.getResponseBodyAsByteBuffer).map { jsValue =>
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

          val error = jsResult.fold(
            errors => {
              val error = InvalidJsonModelException(httpCode, finalUrl, postFormParameters, postJsonParameters, jsValue, errors)
              throw error
            }, error => error
          )

          error
        }

        Left {
          jsonResponse match {
            case scala.util.Success(error) => error
            case scala.util.Failure(throwable) => throw throwable
          }
        }

      case 500 | 502 | 503 | 504 =>
        throw StripeServerError(response)
      case _ =>
        throw UnhandledServerError(response)
    }

  }

  def mapToPostParams(optionalMap: Option[Map[String, String]], parentKey: String) = {
    optionalMap match {
      case Some(map) =>
        map.map { case (key, value) =>
          s"$parentKey[$key]" -> value
        }
      case None =>
        Map.empty
    }
  }

}
