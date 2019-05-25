package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import akka.http.scaladsl.HttpExt
import akka.stream.Materializer
import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.Cards.Card
import org.mdedetrich.stripe.v1.Customers.Source.Token
import org.mdedetrich.stripe.v1.Customers.{Customer, CustomerInput, CustomerUpdate}
import org.mdedetrich.stripe.v1.Tokens.{TokenData, TokenInput}

import scala.concurrent.{ExecutionContext, Future}

class CustomerIT extends IntegrationTest {

  "Customer" should {
    "save credit card, create customer and add token to customer" in {

      val customer = CustomerIT.createCustomerWithCC()

      customer.map { updatedCustomer =>
        updatedCustomer shouldBe a[Customer]

        updatedCustomer.sources.data should have length 1
        val cards = updatedCustomer.sources.data.collect({ case c: Card => c })
        cards should have length 1

        cards.head.last4 should be(CustomerIT.defaultTestCard.takeRight(4))
      }
    }
  }

}

object CustomerIT {

  val defaultTestCard           = "4242424242424242"
  val cardBypassingPendingCheck = "4000000000000077"

  def createCustomerWithCC(
      cardNumber: String = defaultTestCard
  )(implicit client: HttpExt, materializer: Materializer, executionContext: ExecutionContext): Future[Customer] = {
    val in2years   = OffsetDateTime.now.plusYears(2)
    val cardData   = TokenData.Card(in2years.getMonthValue, in2years.getYear, cardNumber).copy(cvc = Option("123"))
    val tokenInput = TokenInput(cardData)

    for {
      token       <- handle(Tokens.create(tokenInput)())
      newCustomer <- handle(Customers.create(CustomerInput())())
      updated     <- handle(Customers.update(newCustomer.id, CustomerUpdate(paymentSource = Option(Token(token.id))))())
    } yield updated

  }
}
