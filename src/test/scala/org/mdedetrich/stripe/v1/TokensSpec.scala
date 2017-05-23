package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.parser.parse
import org.mdedetrich.stripe.v1.Tokens.Token
import org.scalatest.{Matchers, WordSpec}

class TokensSpec extends WordSpec with Matchers {
  "Tokens" should {
    "parse bank account token JSON correctly" in {
      val in = getClass.getResourceAsStream("/bank-account-token.json")

      val string = scala.io.Source.fromInputStream(in).mkString
      val json   = parse(string).toOption

      val token = json.flatMap(_.as[Token].toOption).get
      token.id should be("btok_9OMES2fmhlLbQO")
    }

    "parse credit card token JSON correctly" in {
      val in = getClass.getResourceAsStream("/credit-card-token.json")

      val string = scala.io.Source.fromInputStream(in).mkString
      val json   = parse(string).toOption
      val token  = json.flatMap(_.as[Token].toOption).get

      token.id should be("tok_195vy8J6y4jvjvHho3Fin0x3")
    }
  }

}
