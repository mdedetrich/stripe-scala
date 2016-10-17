package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.v1.Tokens.Token
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class TokensSpec extends WordSpec with Matchers {
  "Tokens" should {
    "parse JSON correctly" in {
      val in = getClass.getResourceAsStream("/bank-account-token.json")
      val json = Json.parse(in)

      val JsSuccess(account, _) = json.validate[Token]
      account.id should be("btok_9OMES2fmhlLbQO")
    }
  }

}
