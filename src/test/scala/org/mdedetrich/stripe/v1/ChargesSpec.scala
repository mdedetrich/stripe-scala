package org.mdedetrich.stripe.v1

import cats.syntax.either._
import io.circe.parser.parse
import org.mdedetrich.stripe.PostParams
import org.mdedetrich.stripe.v1.Charges.{Charge, Source}
import org.scalatest.{Matchers, WordSpec}

class ChargesSpec extends WordSpec with Matchers {

  "Charges" should {
    "parse JSON correctly" in {
      val charge = jsonToCharge("/charge.json")

      charge.id should be("ch_194UQpJ4y4jIjvHhJji6ElAp")
      charge.applicationFee should be(Some("fee_9OKMRHcB2CcVPD"))
      val source = charge.source.asInstanceOf[Source.MaskedCard]
      source.expYear should be(2018)
    }

    "parse card's brand into MaskedCard" in {
      val charge = jsonToCharge("/charge.json")

      val card = charge.source.asInstanceOf[Source.MaskedCard]
      card.brand should be("Visa")
    }
  }

  private def jsonToCharge(jsonFname: String): Charge = {
    val in     = this.getClass.getResourceAsStream(jsonFname)
    val string = scala.io.Source.fromInputStream(in).mkString
    val json   = parse(string).toOption
    json.flatMap(_.as[Charge].toOption).get
  }

  "Charge create POST params" should {

    "put customer charge params" in {
      val customerId = "cus_9MwGfABXdXQKLD"

      val customerSource = Charges.SourceInput.Customer(customerId)

      // must be here for the next line to not throw an NPE
      Currency.lowerCaseNamesToValuesMap
      val input = Charges.ChargeInput(
        amount = BigDecimal(100),
        Currency.`Euro`,
        capture = true,
        customer = Some(customerSource)
      )
      val params = PostParams.toPostParams(input)

      params("customer") should be(customerId)
      params should not contain key("source")
    }

    "put token in charge params" in {
      val token = "tok_4I1pWcGhJbyTLyau"

      val tokenSource = Charges.SourceInput.Token(token)

      val input =
        Charges.ChargeInput(amount = BigDecimal(100), Currency.`Euro`, capture = true, source = Some(tokenSource))
      val params = PostParams.toPostParams(input)

      params("source") should be(token)
      params should not contain key("customer")
    }

    "put card in charge params" ignore {
      val token = "tok_4I1pWcGhJbyTLyau"

      val tokenSource = Charges.SourceInput.Card(
        expMonth = 1,
        expYear = 2030,
        number = "4111111111111111",
        cvc = Some("123"),
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )

      val input =
        Charges.ChargeInput(amount = BigDecimal(100), Currency.`Euro`, capture = true, source = Some(tokenSource))
      val params = PostParams.toPostParams(input)

      params("source") should be(token)
      params should not contain key("customer")
    }
  }

}
