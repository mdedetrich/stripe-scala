package org.mdedetrich.stripe

trait PostParams[T] {
  def toMap(t: T): Map[String, String]
}

object PostParams {
  def toPostParams[T](t: T)(implicit postParams: PostParams[T]): Map[String, String] = postParams.toMap(t)

  def toPostParams(prefix: String, input: Map[String, String]): Map[String, String] = input.map({
    case (key, value) => (s"$prefix[$key]", value)
  })
}
