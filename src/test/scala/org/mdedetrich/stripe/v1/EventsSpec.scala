package org.mdedetrich.stripe.v1

import java.io.{BufferedReader, InputStreamReader}
import java.util.stream.Collectors

import cats.syntax.either._
import io.circe.parser.parse
import org.mdedetrich.stripe.v1.Customers.Customer
import org.mdedetrich.stripe.v1.Events.{Event, _}
import org.scalatest.{Matchers, WordSpec}

class EventsSpec extends WordSpec with Matchers {
  "Events" should {

    "parse customer.create JSON correctly" in {
      val in = this.getClass.getResourceAsStream("/events/customer.created.json")
      in should not be null

      val string = scala.io.Source.fromInputStream(in).mkString
      val json   = parse(string).toOption
      val event  = json.flatMap(_.as[Event].toOption).get

      event.id should be("evt_1A9re7J6y4jvjvHhEh0DfAAv")
      event.`type` should be(Events.Type.CustomerCreated)

      event.data.`object` shouldBe a[Customer]
      val customer = event.data.`object`.asInstanceOf[Customer]
      customer.id should be("cus_AUrMDo0MNqoKI3")
    }

    (0 to 1).foreach { index =>
      val filename = s"account.updated-$index.json"
      s"parse accounts.update from $filename" in {
        val in = this.getClass.getResourceAsStream(s"/events/$filename")
        in should not be null

        val string = scala.io.Source.fromInputStream(in).mkString
        val json   = parse(string).toOption
        val event  = json.flatMap(_.as[Event].toOption).get
        event.`type` should be(Events.Type.AccountUpdated)
      }
    }

    "parse payment.created JSON correctly" in {
      val in = this.getClass.getResourceAsStream("/events/payment.created.json")
      in should not be null

      val string = new BufferedReader(new InputStreamReader(in))
        .lines()
        .collect(Collectors.joining("\n"))
      val json  = parse(string).toOption
      val event = json.flatMap(_.as[Event].toOption).get
      event.`type` should be(Events.Type.PaymentCreated)
    }

    "parse event list" in {
      val in = this.getClass.getResourceAsStream("/events/event-list.json")

      val string = new BufferedReader(new InputStreamReader(in))
        .lines()
        .collect(Collectors.joining("\n"))
      val json      = parse(string).toOption
      val eventList = json.flatMap(_.as[EventList].toOption).get
      eventList.data.size should be(100)
    }
  }

}
