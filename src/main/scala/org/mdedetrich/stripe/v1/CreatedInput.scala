package org.mdedetrich.stripe.v1

import org.joda.time.DateTime
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
  * Common data model for list requests that accept a created input
  */

sealed abstract class CreatedInput

object CreatedInput {

  case class Timestamp(timestamp: DateTime) extends CreatedInput

  case class Object(gt: Option[DateTime],
                    gte: Option[DateTime],
                    lt: Option[DateTime],
                    lte: Option[DateTime]
                   ) extends CreatedInput

  object Object {
    def default: Object = Object(
      None,
      None,
      None,
      None
    )
  }

  implicit val timestampReads: Reads[Timestamp] = stripeDateTimeReads.map(Timestamp)
  implicit val timestampWrites: Writes[Timestamp] =
    Writes((timestamp: Timestamp) =>
      stripeDateTimeWrites.writes(timestamp.timestamp)
    )

  implicit val objectReads: Reads[Object] = (
    (__ \ "gt").readNullable(stripeDateTimeReads) ~
      (__ \ "gte").readNullable(stripeDateTimeReads) ~
      (__ \ "lt").readNullable(stripeDateTimeReads) ~
      (__ \ "lte").readNullable(stripeDateTimeReads)
    ).tupled.map((Object.apply _).tupled)

  implicit val objectWrites: Writes[Object] =
    Writes((o: Object) =>
      Json.obj(
        "gt" -> o.gt,
        "gte" -> o.gte,
        "lt" -> o.lt,
        "gte" -> o.lte
      )
    )

  implicit val createdInputReads: Reads[CreatedInput] =
    __.read[JsValue].flatMap {
      case jsObject: JsObject =>
        __.read[CreatedInput.Object].map(x => x: CreatedInput)
      case jsString: JsString =>
        __.read[CreatedInput.Timestamp].map(x => x: CreatedInput)
      case _ => Reads[CreatedInput](_ => JsError(ValidationError("UnknownCreatedInput")))
    }

  implicit val createdInputWrites: Writes[CreatedInput] =
    Writes((createdInput: CreatedInput) => {
      createdInput match {
        case o: CreatedInput.Object =>
          Json.toJson(o)
        case timestamp: CreatedInput.Timestamp =>
          Json.toJson(timestamp)
      }
    })
}
