package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.BankAccountsPaymentSource.BankAccount
import org.mdedetrich.stripe.v1.Charges.Source.Customer
import org.mdedetrich.stripe.v1.Transfers.TransferInput
import org.scalatest.ParallelTestExecution

class TransferIT extends IntegrationTest with ParallelTestExecution {

  "Transfer" should {
    "transfer money from credit card to bank account" in {

      val customerF = CustomerIT.createCustomerWithCC(CustomerIT.cardBypassingPendingCheck)
      val accountF = AccountIT.createManagedAccountWithBankAccount

      val f = for {
        managedAccount <- accountF
        customer <- customerF
        _ <- handleIdempotent(Charges.create(ChargeIT.chargeInput(managedAccount.id, Customer(customer.id))))
        transfer <- handleIdempotent(Transfers.create(TransferInput.default(1400, Currency.`Euro`, "default_for_currency", stripeAccount = Some(managedAccount.id))))
      } yield (transfer, managedAccount)

      f.map { case (transfer, managedAccount) =>
        transfer.id should startWith("tr")
        transfer.amount should be(1400)
        val bankAccount = managedAccount.externalAccounts.data.collect({ case b:BankAccount => b }).head
        transfer.destination should be(bankAccount.id)
      }

    }

  }
}
