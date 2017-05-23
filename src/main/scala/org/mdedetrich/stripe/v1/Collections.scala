package org.mdedetrich.stripe.v1

import io.circe.{Decoder, Encoder, ObjectEncoder}

object Collections {

  abstract class StripeTypeDispatcher[A] {
    def apply(a: A): String
  }

  /**
    * Stripe returns its list collection in this format
    *
    * @tparam A
    */
  abstract class List[A](val url: String, val hasMore: Boolean, val data: scala.List[A], val totalCount: Option[Long])

  trait ListJsonMappers[A] {
    protected def listDecoder[B <: List[A]](
        implicit decoder: Decoder[A]): ((String, Boolean, scala.List[A], Option[Long]) => B) => Decoder[B] =
      Decoder.forProduct4[String, Boolean, scala.List[A], Option[Long], B]("url", "has_more", "data", "total_count")

    protected def listEncoder[B <: List[A]](implicit encoder: Encoder[A]): ObjectEncoder[B] =
      Encoder.forProduct5[String, String, Boolean, scala.List[A], Option[Long], B]("object",
                                                                                   "url",
                                                                                   "has_more",
                                                                                   "data",
                                                                                   "total_count")(x =>
        ("list", x.url, x.hasMore, x.data, x.totalCount))
  }
}
