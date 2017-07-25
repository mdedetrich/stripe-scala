package org.mdedetrich.stripe

trait PostParams[T] {
  def toMap(t: T): Map[String, String]

  def flatten[K, V](input: Map[K, Option[V]]): Map[K, V] = PostParams.flatten(input)
}

object PostParams {
  def toPostParams[T](optionOfT: Option[T])(implicit postParams: PostParams[T]): Map[String, String] =
    optionOfT.map(postParams.toMap).getOrElse(Map.empty)

  def toPostParams[T](t: T)(implicit postParams: PostParams[T]): Map[String, String] = postParams.toMap(t)

  def toPostParams[T](prefix: String, input: Option[T])(implicit postParams: PostParams[T]): Map[String, String] =
    input.map(toPostParams(prefix, _)).getOrElse(Map.empty)

  def toPostParams[T](prefix: String, input: T)(implicit postParams: PostParams[T]): Map[String, String] = {
    val params = toPostParams(input)
    toPostParams(prefix, params)
  }

  /**
    * Prepends a map of POST parameters with a prefix.
    *
    * This takes into account keys which already have a prefix.
    *
    * toPostParams("baz", Map("foo[bar]" -> "qux")
    *
    * is transformed to
    *
    * Map("baz[foo][bar]" -> "qux")
    */
  def toPostParams(prefix: String, input: Map[String, String]): Map[String, String] =
    input.map({
      case (key, value) if key.contains("[") =>
        // this is definitely hacky and should be replace with a proper AST, modelled after play-json
        val parts = key.split("\\[")
        val first = parts.head
        val rest  = parts.drop(1).mkString("[")
        (s"$prefix[$first][$rest", value)
      case (key, value) => (s"$prefix[$key]", value)
    })

  def params[T](transformer: T => Map[String, String]): PostParams[T] = new PostParams[T] {
    override def toMap(t: T) = transformer(t)
  }

  def flatten[K, V](input: Map[K, Option[V]]): Map[K, V]       = input.collect({ case (k, Some(v)) => (k, v) })
  def flatten[K, V](input: List[(K, Option[V])]): List[(K, V)] = input.collect({ case (k, Some(v)) => (k, v) })
}
