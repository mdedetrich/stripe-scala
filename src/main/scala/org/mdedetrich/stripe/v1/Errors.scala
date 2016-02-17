package org.mdedetrich.stripe.v1

import org.mdedetrich.utforsca.SealedContents
import play.api.libs.json.{JsString, Writes, Reads}

object Errors {

  sealed abstract class Error(val id: String)

  case class UnknownError(val id: String) extends Exception {
    override def getMessage = "Unknown Error type, received $id"
  }

  object Error {

    case object ApiConnectionError extends Error("api_connection_error")

    case object ApiError extends Error("api_error")

    case object AuthenticationError extends Error("authentication_error")

    case object CardError extends Error("card_error")

    case object InvalidRequestError extends Error("invalid_request_error")

    case object RateLimitError extends Error("rate_limit_error")

    lazy val all: Set[Error] = SealedContents.values[Error]

  }

  implicit val errorReads: Reads[Error] = Reads.of[String].map { errorId =>
    Error.all.find(_.id == errorId).getOrElse {
      throw UnknownError(errorId)
    }
  }

  implicit val errorWrites: Writes[Error] =
    Writes((error: Error) => JsString(error.id))

}


