package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.v1.ApplicationFeeRefunds.ApplicationFeeRefund
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class ApplicationFeeRefundsSpec extends WordSpec with Matchers {

  "ApplicationFeeRefunds" should {
    "parse JSON correctly" in {
      val in   = getClass.getResourceAsStream("/application-fee-refund.json")
      val json = Json.parse(in)

      val JsSuccess(account, _) = json.validate[ApplicationFeeRefund]
      account.id should be("fr_7iEwuXNLaHBGdZ")
      account.balanceTransaction should be(Some("123"))
    }
  }

}
