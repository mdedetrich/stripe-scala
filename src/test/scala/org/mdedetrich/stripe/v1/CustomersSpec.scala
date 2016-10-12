package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.PostParams
import org.mdedetrich.stripe.v1.Customers.CustomerUpdate
import org.mdedetrich.stripe.v1.Customers.Source.Token
import org.scalatest.{Matchers, WordSpec}

class CustomersSpec extends WordSpec with Matchers {
  "Customer update POST params" should {

    "convert payment source" in {
      val token = "radiohead"
      val update = CustomerUpdate(Some(Token(token)))
      val postParams = PostParams.toPostParams(update)
      postParams should be(Map("source" -> token))
    }

  }
}
