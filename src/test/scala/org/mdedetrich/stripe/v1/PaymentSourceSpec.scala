package org.mdedetrich.stripe.v1

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsValue, Json}

class PaymentSourceSpec extends WordSpec with Matchers {
  "Payment sources" should {
    "convert to JSON" in {
      val ba = BankAccountsPaymentSource.BankAccount("id123")
      Json.toJson(ba) shouldBe a[JsValue]
    }
  }
}
