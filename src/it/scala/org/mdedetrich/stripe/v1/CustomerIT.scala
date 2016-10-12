package org.mdedetrich.stripe.v1

import java.time.OffsetDateTime

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.Cards.Card
import org.mdedetrich.stripe.v1.Customers.Source.Token
import org.mdedetrich.stripe.v1.Customers.{Customer, CustomerInput, CustomerUpdate}
import org.mdedetrich.stripe.v1.Tokens.{TokenData, TokenInput}

class CustomerIT extends IntegrationTest {

  val testCard = "4242424242424242"

  "Customer" should {
    "should save credit card, create customer and add token to customer" in {

      val in2years = OffsetDateTime.now.plusYears(2)
      val cardData = TokenData.Card.default(in2years.getMonthValue, in2years.getYear, testCard).copy(cvc = Some("123"))
      val tokenInput = TokenInput.default(cardData)

      val updateF = for {
        token <- handle(Tokens.create(tokenInput)())
        newCustomer <- handle(Customers.create(CustomerInput.default)())
        updated <- handle(Customers.update(newCustomer.id, CustomerUpdate(Some(Token(token.id))))())
      } yield updated

      whenReady(updateF) { updatedCustomer =>
        updatedCustomer shouldBe a[Customer]

        updatedCustomer.sources.data should have length 1
        val cards = updatedCustomer.sources.data.collect({ case c:Card => c })
        cards should have length 1

        cards.head.last4 should be(testCard.takeRight(4))
      }
    }
  }

}
