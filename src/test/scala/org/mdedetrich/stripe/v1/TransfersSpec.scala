package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.parser.parse
import org.mdedetrich.stripe.v1.Transfers.Transfer
import org.scalatest.{Matchers, WordSpec}

class TransfersSpec extends WordSpec with Matchers {

  "Transfers" should {
    "parse JSON correctly" in {
      val in       = getClass.getResourceAsStream("/transfer.json")
      val string   = scala.io.Source.fromInputStream(in).mkString
      val json     = parse(string).toOption
      val transfer = json.flatMap(_.as[Transfer].toOption).get

      transfer.id should be("tr_19N5vuJ6y4jvjvHh8S1r8WfH")
    }
  }

}
