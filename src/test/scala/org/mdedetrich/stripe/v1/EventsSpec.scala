package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.v1.Balances.{Balance, BalanceFund, SourceTypes}
import org.mdedetrich.stripe.v1.Currency.`Euro`
import org.mdedetrich.stripe.v1.Customers.Customer
import org.mdedetrich.stripe.v1.Events.{Event, _}

class EventsSpec extends BaseSpec {

  "Events" should {

    "parse customer.create JSON correctly" in {
      val event = getJsonResourceAs[Event]("/events/customer.created.json")

      event.id should be("evt_1A9re7J6y4jvjvHhEh0DfAAv")
      event.`type` should be(Events.Type.CustomerCreated)

      event.data.`object` shouldBe a[Customer]
      val customer = event.data.`object`.asInstanceOf[Customer]
      customer.id should be("cus_AUrMDo0MNqoKI3")
    }

    (0 to 1).foreach { index =>
      val filename = s"account.updated-$index.json"
      s"parse accounts.update from $filename" in {
        val event = getJsonResourceAs[Event](s"/events/$filename")
        event.`type` should be(Events.Type.AccountUpdated)
        event.data.previousAttributes should not be None
      }
    }

    s"parse previous attributes correctly" in {
      val event = getJsonResourceAs[Event]("/events/account.updated-0.json")
      event.`type` should be(Events.Type.AccountUpdated)
      event.data.previousAttributes.get.toMap("transfers_enabled").asBoolean should contain(false)
    }

    "parse payment.created JSON correctly" in {
      val event = getJsonResourceAs[Event]("/events/payment.created.json")
      event.`type` should be(Events.Type.PaymentCreated)
    }

    "parse event list" in {
      val eventList = getJsonResourceAs[EventList]("/events/event-list.json")
      eventList.data.size should be(100)
    }

    "parse balance.available" in {
      val evt = getJsonResourceAs[Event]("/events/balance.available.json")

      evt.data.`object` should be(
        Balance(
          List(BalanceFund(Euro, 10200, SourceTypes(Some(10200), None, None))),
          true,
          List(BalanceFund(Euro, 0, SourceTypes(Some(0), None, None)))
        )
      )

      evt.account should contain("acct_3itc1T5KSo2GauOn")
      evt.userId should contain("acct_3itc1T5KSo2GauOn")

    }
  }

}
