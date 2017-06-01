package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.parser.parse
import org.mdedetrich.stripe.PostParams
import org.mdedetrich.stripe.v1.Charges.Charge
import org.scalatest.{Matchers, WordSpec}

class ChargesSpec extends WordSpec with Matchers {

  "Charges" should {
    "parse JSON correctly" in {
      val in     = this.getClass.getResourceAsStream("/charge.json")
      val string = scala.io.Source.fromInputStream(in).mkString
      val json   = parse(string).toOption
      val charge = json.flatMap(_.as[Charge].toOption).get

      charge.id should be("ch_194UQpJ4y4jIjvHhJji6ElAp")
      charge.applicationFee should be(Option("fee_9OKMRHcB2CcVPD"))
      val source = charge.source.asInstanceOf[Charges.Source.MaskedCard]
      source.expYear should be(2018)
    }
  }

  "Charge create POST params" should {

    "put customer charge params" in {
      val customerId = "cus_9MwGfABXdXQKLD"

      val customerSource = Charges.SourceInput.Customer(customerId)

      // must be here for the next line to not throw an NPE
      Currency.lowerCaseNamesToValuesMap
      val input = Charges.ChargeInput(amount = BigDecimal(100),
                                      Currency.`Euro`,
                                      capture = true,
                                      customer = Option(customerSource))
      val params = PostParams.toPostParams(input)

      params("customer") should be(customerId)
      params should not contain key("source")
    }

  }

}
