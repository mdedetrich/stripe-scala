package org.mdedetrich.stripe

import akka.http.scaladsl.model.Uri
import io.circe.{DecodingFailure, Json}

/**
  * This exception is thrown when there is an error in converting the JSON to a case class
  *
  * @param url            The URL for the call
  * @param postParameters The POST body as form parameters
  * @param postJson       The POST body as JSON
  * @param jsonResponse   The original json response
  * @param error          The error as reported from circe
  */
case class InvalidJsonModelException(httpStatusCode: Long,
                                     url: Uri,
                                     postParameters: Option[Map[String, String]],
                                     postJson: Option[Json],
                                     jsonResponse: Json,
                                     error: DecodingFailure)
    extends Exception {
  override def getMessage = s"Invalid JSON model, errors are $error"
}
