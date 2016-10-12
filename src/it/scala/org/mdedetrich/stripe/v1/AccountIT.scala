package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.Accounts.Account

class AccountIT extends IntegrationTest {

  val testCard = "4242424242424242"

  "Account" should {
    "create" in {

      val meta = Map("foo" -> "bar")
      val accountInput = Accounts.AccountInput.default.copy(managed = true, metadata = meta)
      val accountF     = handleIdempotent(Accounts.create(accountInput))

      whenReady(accountF) { account =>
        account shouldBe a[Account]
        account.metadata should be(meta)
      }
    }
  }

}
