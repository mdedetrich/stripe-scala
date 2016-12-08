package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.v1.Refunds.Refund
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class RefundsSpec extends WordSpec with Matchers {

  "Refunds" should {
    "parse JSON correctly" in {
      val in   = getClass.getResourceAsStream("/refund.json")
      val json = Json.parse(in)

      val JsSuccess(account, _) = json.validate[Refund]
      account.id should be("re_20OUr9J6y4jvjvHAKUY47to")
    }
  }

}
