package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.JsonObject
import io.circe.parser.parse
import io.circe.syntax._
import org.mdedetrich.stripe.PostParams
import org.mdedetrich.stripe.v1.Customers.Source.Token
import org.mdedetrich.stripe.v1.Customers.{Customer, CustomerUpdate}
import org.scalatest.{Matchers, WordSpec}

class CustomersSpec extends WordSpec with Matchers {

  "Customers" should {
    "parse JSON correctly" in {
      val in     = this.getClass.getResourceAsStream("/customer.json")
      val string = scala.io.Source.fromInputStream(in).mkString
      val json   = parse(string).toOption

      val customer = json.flatMap(_.as[Customer].toOption).get

      customer.id should be("cus_9OhFdQkoPCkZnw")
      customer.sources.data should have length 1
    }

    "convert to JSON" in {

      val in       = this.getClass.getResourceAsStream("/customer.json")
      val string   = scala.io.Source.fromInputStream(in).mkString
      val json     = parse(string).toOption
      val customer = json.flatMap(_.as[Customer].toOption).get

      val outputJson = customer.asJson

      outputJson.as[JsonObject].map(_.toMap("id")).toOption shouldBe defined
    }
  }

  "Customer update POST params" should {

    "convert payment source" in {
      val token      = "radiohead"
      val update     = CustomerUpdate(paymentSource = Some(Token(token)))
      val postParams = PostParams.toPostParams(update)
      postParams should be(Map("source" -> token))
    }

    "convert default source" in {
      val id         = "georgio-moroder"
      val update     = CustomerUpdate(defaultSource = Some(id))
      val postParams = PostParams.toPostParams(update)
      postParams should be(Map("default_source" -> id))
    }
  }
}
