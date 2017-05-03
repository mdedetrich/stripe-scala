package org.mdedetrich.stripe.v1

import java.time.LocalDate

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.Charges.ChargeInput
import org.mdedetrich.stripe.v1.Charges.Source.MaskedCard
import org.mdedetrich.stripe.v1.Charges.SourceInput.Customer

class ChargeIT extends IntegrationTest {

  "Charge" should {
    "transfer money from a customer credit card into the bank account of a managed account and add a fee for the platform" in {

      val customerF = CustomerIT.createCustomerWithCC()
      val accountF  = AccountIT.createManagedAccountWithBankAccount

      val chargeF = for {
        customer       <- customerF
        managedAccount <- accountF
        charge         <- handleIdempotent(Charges.create(ChargeIT.chargeInput(managedAccount.id, Customer(customer.id))))
      } yield charge

      chargeF.map { charge =>
        charge shouldBe a[Charges.Charge]
        val card = charge.source.asInstanceOf[MaskedCard]
        card.expYear should be(LocalDate.now.plusYears(2).getYear)
        card.last4 should be(CustomerIT.defaultTestCard.takeRight(4))
      }

    }
  }

}

object ChargeIT {
  def chargeInput(customer: Customer): ChargeInput                      = chargeInput(None, customer).copy(applicationFee = None)
  def chargeInput(destination: String, customer: Customer): ChargeInput = chargeInput(Some(destination), customer)
  def chargeInput(destination: Option[String], customer: Customer): ChargeInput =
    Charges.ChargeInput
      .default(1500, Currency.`Euro`, capture = true, customer)
      .copy(applicationFee = Some(100), destination = destination)
}
