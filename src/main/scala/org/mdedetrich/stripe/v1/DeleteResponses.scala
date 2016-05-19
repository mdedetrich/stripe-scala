package org.mdedetrich.stripe.v1

import play.api.libs.json._
import play.api.libs.functional.syntax._

object DeleteResponses {

  case class DeleteResponse(id: String, deleted: Boolean)

  implicit val deleteResponseReads: Reads[DeleteResponse] = (
      (__ \ "id").read[String] ~
      (__ \ "deleted").read[Boolean]
  ).tupled.map((DeleteResponse.apply _).tupled)

  implicit val deleteResponseWrites: Writes[DeleteResponse] = Writes(
      (deleteResponse: DeleteResponse) =>
        Json.obj(
            "id" -> deleteResponse.id,
            "deleted" -> deleteResponse.deleted
      ))
}
