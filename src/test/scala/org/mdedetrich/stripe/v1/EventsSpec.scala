package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.v1.Customers.Customer
import org.mdedetrich.stripe.v1.Events.Event
import org.mdedetrich.stripe.v1.Events._
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class EventsSpec extends WordSpec with Matchers {
  "Events" should {
    "parse JSON correctly" in {
      val in = this.getClass.getResourceAsStream("/event.json")
      in should not be null
      val json = Json.parse(in)

      val JsSuccess(event, _) = json.validate[Event]
      event.id should be("evt_1A9re7J6y4jvjvHhEh0DfAAv")
      event.`type` should be(Events.Type.CustomerCreated)

      event.data.`object` shouldBe a[Customer]
      val customer = event.data.`object`.asInstanceOf[Customer]
      customer.id should be("cus_AUrMDo0MNqoKI3")
    }

    "parse event list" in {
      val in   = this.getClass.getResourceAsStream("/event-list.json")
      val json = Json.parse(in)

      val JsSuccess(eventList, _) = json.validate[EventList]

      eventList.data.size should be(100)
    }
  }

}
