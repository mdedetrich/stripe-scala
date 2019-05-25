package org.mdedetrich.stripe.v1

import io.circe.{Decoder, Encoder}

final case class DeleteResponse(id: String, deleted: Boolean)

object DeleteResponse {

  implicit val deleteResponseDecoder: Decoder[DeleteResponse] = Decoder.forProduct2(
    "id",
    "deleted"
  )(DeleteResponse.apply)

  implicit val deleteResponseEncoder: Encoder[DeleteResponse] = Encoder.forProduct2(
    "id",
    "deleted"
  )(x => DeleteResponse.unapply(x).get)

}
