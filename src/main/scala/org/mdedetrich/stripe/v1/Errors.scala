package org.mdedetrich.stripe.v1

import org.mdedetrich.utforsca.SealedContents
import play.api.libs.json.{JsString, Writes, Reads}

object Errors {

  /**
    * Errors taken from https://stripe.com/docs/api#errors
    * @param id
    */
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

  /**
    * Codes taken from https://stripe.com/docs/api#errors
    * @param id
    */
  sealed abstract class Code(val id: String)

  case class UnknownCode(val id: String) extends Exception {
    override def getMessage = "Unknown Error code, received $id"
  }
  
  object Code {
    case object InvalidNumber extends Code("invalid_number")
    
    case object InvalidExpiryMonth extends Code("invalid_expiry_month")
    
    case object InvalidExpiryYear extends Code("invalid_expiry_year")
    
    case object InvalidCVC extends Code("invalid_cvc")
    
    case object IncorrectNumber extends Code("incorrect_number")
    
    case object ExpiredCard extends Code("expired_card")
    
    case object IncorrectCVC extends Code("incorrect_cvc")
    
    case object IncorrectZip extends Code("incorrect_zip")
    
    case object CardDeclined extends Code("card_declined")
    
    case object Missing extends Code("missing")
    
    case object ProcessingError extends Code("processing_error")
    
    lazy val all: Set[Code] = SealedContents.values[Code]
  }

  implicit val codeReads: Reads[Code] = Reads.of[String].map { codeId =>
    Code.all.find(_.id == codeId).getOrElse {
      throw UnknownCode(codeId)
    }
  }
  
  implicit val codeWrites: Writes[Code] =
    Writes((code: Code) => JsString(code.id))
  
}


