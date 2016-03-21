package org.mdedetrich.stripe.v1

import org.joda.time.DateTime
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
  * Common data model for list requests that accept a created input
  */

sealed abstract class ListFilterInput

object ListFilterInput {

  case class Timestamp(timestamp: DateTime) extends ListFilterInput

  case class Object(gt: Option[DateTime],
                    gte: Option[DateTime],
                    lt: Option[DateTime],
                    lte: Option[DateTime]
                   ) extends ListFilterInput

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

  implicit val listFilterInputReads: Reads[ListFilterInput] =
    __.read[JsValue].flatMap {
      case jsObject: JsObject =>
        __.read[ListFilterInput.Object].map(x => x: ListFilterInput)
      case jsString: JsString =>
        __.read[ListFilterInput.Timestamp].map(x => x: ListFilterInput)
      case _ => Reads[ListFilterInput](_ => JsError(ValidationError("UnknownListFilterInput")))
    }

  implicit val listFilterInputWrites: Writes[ListFilterInput] =
    Writes((filterInput: ListFilterInput) => {
      filterInput match {
        case o: ListFilterInput.Object =>
          Json.toJson(o)
        case timestamp: ListFilterInput.Timestamp =>
          Json.toJson(timestamp)
      }
    })
}
