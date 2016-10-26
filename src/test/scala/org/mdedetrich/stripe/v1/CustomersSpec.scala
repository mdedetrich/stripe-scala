package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.PostParams
import org.mdedetrich.stripe.v1.Customers.Source.Token
import org.mdedetrich.stripe.v1.Customers.{Customer, CustomerUpdate}
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class CustomersSpec extends WordSpec with Matchers {

  "Customers" should {
    "parse JSON correctly" in {
      val in = getClass.getResourceAsStream("/customer.json")
      val json = Json.parse(in)

      val JsSuccess(customer, _) = json.validate[Customer]
      customer.id should be("cus_9OhFdQkoPCkZnw")
      customer.sources.data should have length 1
    }

    "convert to JSON" in {
      val in = getClass.getResourceAsStream("/customer.json")
      val inputJson = Json.parse(in)

      val JsSuccess(customer, _) = inputJson.validate[Customer]

      val outputJson = Json.toJson(customer)

      outputJson \ "id" should be
    }
  }

  "Customer update POST params" should {

    "convert payment source" in {
      val token = "radiohead"
      val update = CustomerUpdate.default.copy(paymentSource = Some(Token(token)))
      val postParams = PostParams.toPostParams(update)
      postParams should be(Map("source" -> token))
    }

    "convert default source" in {
      val id = "georgio-moroder"
      val update = CustomerUpdate.default.copy(defaultSource = Some(id))
      val postParams = PostParams.toPostParams(update)
      postParams should be(Map("default_source" -> id))
    }
  }
}
