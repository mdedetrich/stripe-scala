package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.parser.parse
import org.mdedetrich.stripe.v1.Refunds.Refund
import org.scalatest.{Matchers, WordSpec}

class RefundsSpec extends WordSpec with Matchers {

  "Refunds" should {
    "parse JSON correctly" in {
      val in = getClass.getResourceAsStream("/refund.json")

      val string = scala.io.Source.fromInputStream(in).mkString
      val json   = parse(string).toOption

      val refund = json.flatMap(_.as[Refund].toOption).get

      refund.id should be("re_20OUr9J6y4jvjvHAKUY47to")
    }
  }

}
