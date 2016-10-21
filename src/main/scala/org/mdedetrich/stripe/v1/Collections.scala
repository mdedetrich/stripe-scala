package org.mdedetrich.stripe.v1

import play.api.libs.json._
import play.api.libs.functional.syntax._

object Collections {

  abstract class StripeTypeDispatcher[A] {
    def apply(a: A): String
  }

  /**
    * Stripe returns its list collection in this format
    *
    * @tparam A
    */
  abstract class List[A](val url: String, val hasMore: Boolean, val data: scala.List[A], val totalCount: Option[Long]) {}

  trait ListJsonMappers[A] {
    protected def listReads(implicit readsEvidence: Reads[A]) =
      (__ \ "url").read[String] ~
        (__ \ "has_more").read[Boolean] ~
        (__ \ "data").read[scala.List[A]] ~
        (__ \ "total_count").readNullable[Long]

    protected def listWrites(implicit writesEvidence: Writes[A]) =
      Writes(
        (list: List[A]) =>
          Json.obj(
            "object"      -> "list",
            "url"         -> list.url,
            "has_more"    -> list.hasMore,
            "data"        -> Json.toJson(list.data),
            "total_count" -> list.totalCount
        ))
  }
}
