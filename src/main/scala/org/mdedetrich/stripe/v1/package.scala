package org.mdedetrich.stripe

import com.ning.http.client.Response
import com.typesafe.scalalogging.Logger
import jawn.support.play.Parser
import org.mdedetrich.stripe.v1.Errors.{Error, StripeServerError, UnhandledServerError}
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.util.Try

package object v1 {

  /**
    * This is a header constat to specify a Idempotency-Key
    */
  
  val idempotencyKeyHeader = "Idempotency-Key"
  

  /**
    * Parses a response from dispatch and attempts to do error process handling specific for stripe
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
          val jsResult: JsResult[Error] = httpCode match {
            case 400 =>
              (__ \ "error").read[Error.BadRequest].reads(jsValue)
            case 401 =>
              (__ \ "error").read[Error.Unauthorized].reads(jsValue)
            case 402 =>
              (__ \ "error").read[Error.RequestFailed].reads(jsValue)
            case 404 =>
              (__ \ "error").read[Error.NotFound].reads(jsValue)
            case 429 =>
              (__ \ "error").read[Error.TooManyRequests].reads(jsValue)
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
