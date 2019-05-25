package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import cats.syntax.either._
import defaults._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/**
  * Common data model for list requests that accept a created input
  */
sealed abstract class ListFilterInput

object ListFilterInput {

  case class Timestamp(timestamp: OffsetDateTime) extends ListFilterInput

  case class Object(
      gt: Option[OffsetDateTime] = None,
      gte: Option[OffsetDateTime] = None,
      lt: Option[OffsetDateTime] = None,
      lte: Option[OffsetDateTime] = None
  ) extends ListFilterInput

  implicit val timestampDecoder: Decoder[Timestamp] = stripeDateTimeDecoder.map(Timestamp)

  implicit val timestampEncoder: Encoder[Timestamp] = Encoder.instance[Timestamp] { timestamp =>
    timestampEncoder.apply(timestamp)
  }

  implicit val objectDecoder: Decoder[Object] = Decoder.forProduct4(
    "gt",
    "gte",
    "lt",
    "lte"
  )(Object.apply)

  implicit val objectEncoder: Encoder[Object] = Encoder.forProduct4(
    "gt",
    "gte",
    "lt",
    "lte"
  )(x => Object.unapply(x).get)

  implicit val listFilterInputDecoder: Decoder[ListFilterInput] = Decoder.instance[ListFilterInput] { c =>
    for {
      json <- c.as[Json]
      result <- {
        if (json.isObject) {
          c.as[ListFilterInput.Object].map(x => x: ListFilterInput)
        } else if (json.isString) {
          c.as[ListFilterInput.Timestamp].map(x => x: ListFilterInput)
        } else {
          Left(DecodingFailure("UnknownListFilterInput", c.history))
        }
      }
    } yield result
  }

  implicit val listFilterInputEncoder: Encoder[ListFilterInput] = Encoder.instance[ListFilterInput] {
    case o: ListFilterInput.Object =>
      implicitly[Encoder[ListFilterInput.Object]].apply(o)
    case timestamp: ListFilterInput.Timestamp =>
      implicitly[Encoder[ListFilterInput.Timestamp]].apply(timestamp)
  }

}
