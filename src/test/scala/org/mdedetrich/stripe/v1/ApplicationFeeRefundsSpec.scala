package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.parser.parse
import org.mdedetrich.stripe.v1.ApplicationFeeRefunds.ApplicationFeeRefund
import org.scalatest.{Matchers, WordSpec}

class ApplicationFeeRefundsSpec extends WordSpec with Matchers {

  "ApplicationFeeRefunds" should {
    "parse JSON correctly" in {
      val in     = this.getClass.getResourceAsStream("/application-fee-refund.json")
      val string = scala.io.Source.fromInputStream(in).mkString
      val json   = parse(string).toOption

      val applicationFeeRefund = json.flatMap(_.as[ApplicationFeeRefund].toOption).get

      applicationFeeRefund.id should be("fr_7iEwuXNLaHBGdZ")
      applicationFeeRefund.balanceTransaction should be(Option("123"))
    }
  }

}
