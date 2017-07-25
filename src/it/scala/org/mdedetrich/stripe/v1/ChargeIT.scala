package org.mdedetrich.stripe.v1

import java.time.{LocalDate, OffsetDateTime}

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.Charges.{ChargeInput, SourceInput}
import org.mdedetrich.stripe.v1.Charges.Source.MaskedCard
import org.mdedetrich.stripe.v1.Charges.SourceInput.Customer
import org.mdedetrich.stripe.v1.Tokens.{TokenData, TokenInput}

class ChargeIT extends IntegrationTest {

  "Charge" should {

    "create token and charge it" in {

      val cardNumber = "4242424242424242"
      val in2years   = OffsetDateTime.now.plusYears(2)
      val cardData   = TokenData.Card(in2years.getMonthValue, in2years.getYear, cardNumber).copy(cvc = Option("123"))
      val tokenInput = TokenInput(cardData)

      val chargeF = for {
        token <- handle(Tokens.create(tokenInput)())
        charge <- handle(
          Charges.create(
            Charges.ChargeInput(amount = BigDecimal(15000),
                                Currency.`Euro`,
                                capture = true,
                                source = Some(SourceInput.Token(token.id))))())
      } yield charge

      chargeF.map { charge =>
        charge shouldBe a[Charges.Charge]
        val card = charge.source.asInstanceOf[MaskedCard]
        card.expYear should be(LocalDate.now.plusYears(2).getYear)
        card.last4 should be(cardNumber.takeRight(4))
      }

    }

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
  def chargeInput(destination: String, customer: Customer): ChargeInput = chargeInput(Option(destination), customer)
  def chargeInput(destination: Option[String], customer: Customer): ChargeInput =
    Charges.ChargeInput(amount = BigDecimal(1500),
                        Currency.`Euro`,
                        capture = true,
                        customer = Option(customer),
                        applicationFee = Option(BigDecimal(100)),
                        destination = destination)
}
