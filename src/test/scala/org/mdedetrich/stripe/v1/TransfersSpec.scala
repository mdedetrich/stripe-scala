package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.v1.Transfers.Transfer
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class TransfersSpec extends WordSpec with Matchers {

  "Transfers" should {
    "parse JSON correctly" in {
      val in = getClass.getResourceAsStream("/transfer.json")
      val json = Json.parse(in)

      val JsSuccess(account, _) = json.validate[Transfer]
      account.id should be("tr_19N5vuJ6y4jvjvHh8S1r8WfH")
    }
  }

}
