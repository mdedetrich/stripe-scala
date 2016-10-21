package org.mdedetrich.stripe.v1

import java.time.LocalDate

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.Charges.ChargeInput
import org.mdedetrich.stripe.v1.Charges.Source.Customer

class ChargeIT extends IntegrationTest {

  "Charge" should {
    "transfer money from a customer credit card into the bank account of a managed account and add a fee for the platform" in {

      def chargeInput(destination: String, customer: Customer): ChargeInput =
        Charges.ChargeInput.default(1500, Currency.`Euro`, capture = true, customer)
          .copy(applicationFee = Some(100), destination = Some(destination))

      val customerF = CustomerIT.createCustomerWithCC
      val accountF = AccountIT.createManagedAccountWithBankAccount

      val chargeF = for {
        customer <- customerF
        managedAccount <- accountF
        charge <- handleIdempotent(Charges.create(chargeInput(managedAccount.id, Customer(customer.id))))
      } yield charge

      whenReady(chargeF) { charge =>
        charge shouldBe a[Charges.Charge]
        charge.source.expYear should be(LocalDate.now.plusYears(2).getYear)
        charge.source.last4 should be(CustomerIT.testCard.takeRight(4))
      }

    }
  }

}
