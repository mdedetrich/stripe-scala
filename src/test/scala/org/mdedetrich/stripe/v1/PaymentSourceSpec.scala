package org.mdedetrich.stripe.v1

import io.circe.Json
import org.scalatest.{Matchers, WordSpec}
import io.circe.syntax._

class PaymentSourceSpec extends WordSpec with Matchers {
  "Payment sources" should {
    "convert to JSON" in {
      val ba = BankAccountsPaymentSource.BankAccount("id123", None, "444", "1243", "DE", true)
      ba.asJson shouldBe a[Json]
    }
  }
}
