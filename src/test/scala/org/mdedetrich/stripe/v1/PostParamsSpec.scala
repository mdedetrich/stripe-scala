package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.PostParams
import org.scalatest.{Matchers, WordSpec}

class PostParamsSpec extends WordSpec with Matchers {
  "Post params" should {
    "convert map to Stripe post params" in {

      val email = "hans@horst.de"
      val meta = Map("email" -> email)
      val params = PostParams.toPostParams("user", meta)
      params should be(Map("user[email]" -> email))
    }
  }
}
