package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.PostParams
import org.mdedetrich.stripe.v1.Charges.Charge
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class ChargesSpec extends WordSpec with Matchers {

  "Charges" should {
    "parse JSON correctly" in {
      val in = getClass.getResourceAsStream("/charge.json")
      val json = Json.parse(in)

      val JsSuccess(account, _) = json.validate[Charge]
      account.id should be("ch_194UQpJ4y4jIjvHhJji6ElAp")
      account.source.expYear should be(2018)
    }
  }

  "Charge create POST params" should {

    "put customer charge params" in {
      val customerId = "cus_9MwGfABXdXQKLD"

      val customerSource = Charges.Source.Customer(customerId)

      // must be here for the next line to not throw an NPE
      Currency.lowerCaseNamesToValuesMap
      val input = Charges.ChargeInput.default(100, Currency.`Euro`, capture = true, customerSource)
      val params = PostParams.toPostParams(input)

      params("customer") should be(customerId)
      params should not contain key ("source")
    }

  }

}
