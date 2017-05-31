package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpecLike}

trait BaseSpec extends Matchers with WordSpecLike {

  def getJsonResource(path: String): Json = {
    val in = this.getClass.getResourceAsStream(path)
    in should not be null
    val string = scala.io.Source.fromInputStream(in).mkString
    parse(string).toOption.get
  }

  def getJsonResourceAs[T](path: String)(implicit d: io.circe.Decoder[T]): T =
    getJsonResource(path).as[T].toOption.get
}
