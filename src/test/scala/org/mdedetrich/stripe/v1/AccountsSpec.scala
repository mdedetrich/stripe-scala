package org.mdedetrich.stripe.v1

import java.time.{LocalDate, OffsetDateTime}

import org.mdedetrich.stripe.PostParams
import org.mdedetrich.stripe.v1.Accounts.{Account, AccountInput, AccountUpdate, LegalEntity, TosAcceptance}
import org.mdedetrich.stripe.v1.BankAccounts.BankAccountData
import org.mdedetrich.stripe.v1.Shippings.Address
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsSuccess, Json}

class AccountsSpec extends WordSpec with Matchers {
  val address = Address.default.copy(
        line1 = Some("Široka ulica"),
        line2 = Some("Apartman B1"),
        postalCode = Some("1234"),
        city = Some("Zadar"),
        country = Some("HR")
      )

  "Accounts" should {
    "parse JSON correctly" in {
      val in = this.getClass.getResourceAsStream("/account.json")
      val json = Json.parse(in)

      val JsSuccess(account, _) = json.validate[Account]
      account.id should be("acct_191dzJG5YhaiJXJY")
      account.legalEntity.address.country should be(Some("DE"))
    }
  }

  "Account create POST params" should {

    "convert tos acceptance" in {
      val now = OffsetDateTime.now()
      val ip = "120.0.0.1"
      val update = AccountInput.default.copy(tosAcceptance = Some(TosAcceptance(Some(now), Some(ip))))
      val map = PostParams.toPostParams(update)

      map("tos_acceptance[date]") should be(now.toEpochSecond.toString)
      map("tos_acceptance[ip]") should be(ip)
    }

    "convert address" in {
      val input = AccountInput.default.copy(legalEntity = Some(LegalEntity.default.copy(address = address)))
      val map = PostParams.toPostParams(input)

      map("legal_entity[address][line1]") should be(address.line1.get)
      map("legal_entity[address][line2]") should be(address.line2.get)
      map("legal_entity[address][city]") should be(address.city.get)
      map("legal_entity[address][postal_code]") should be(address.postalCode.get)
      map("legal_entity[address][country]") should be(address.country.get)
    }
  }

  "Account update POST params" should {

    "convert default currency" in {
      val update = AccountUpdate.default.copy(defaultCurrency = Some(Currency.`Algerian Dinar`))
      val map = PostParams.toPostParams(update)

      map("default_currency") should be("DZD")
    }

    "convert address" in {
      val update = AccountUpdate.default.copy(legalEntity = Some(LegalEntity.default.copy(address = address)))
      val map = PostParams.toPostParams(update)

      map("legal_entity[address][line1]") should be(address.line1.get)
      map("legal_entity[address][line2]") should be(address.line2.get)
      map("legal_entity[address][city]") should be(address.city.get)
      map("legal_entity[address][postal_code]") should be(address.postalCode.get)
      map("legal_entity[address][country]") should be(address.country.get)
    }

    "convert dob" in {
      val date = LocalDate.of(2016, 10, 6)
      val update = AccountUpdate.default.copy(legalEntity = Some(LegalEntity.default.copy(dob = Some(date))))
      val map = PostParams.toPostParams(update)

      map("legal_entity[dob][year]") should be(date.getYear.toString)
      map("legal_entity[dob][month]") should be(date.getMonthValue.toString)
      map("legal_entity[dob][day]") should be(date.getDayOfMonth.toString)
    }

    "convert name" in {
      val first = "Debbie"
      val last = "Harry"
      val update = AccountUpdate.default.copy(legalEntity = Some(LegalEntity.default.copy(firstName = Some(first), lastName = Some(last))))
      val map = PostParams.toPostParams(update)

      map("legal_entity[first_name]") should be(first)
      map("legal_entity[last_name]") should be(last)
    }

    "convert tos acceptance" in {
      val now = OffsetDateTime.parse("2016-10-06T13:40:43Z")
      val ipAddress = "fd45:69e1:4b4e::"
      val update = AccountUpdate.default.copy(tosAcceptance = Some(TosAcceptance(Some(now), Some(ipAddress))))
      val map = PostParams.toPostParams(update)

      map("tos_acceptance[date]") should be("1475761243")
      map("tos_acceptance[ip]") should be(ipAddress)
    }

    "convert external account data" in {
      val bankAccount = BankAccountData.Source.Object.default("DE89370400440532013000", "DE", Currency.`Euro`)
      val update = AccountUpdate.default.copy(externalAccount = Some(bankAccount))
      val map = PostParams.toPostParams(update)

      map("external_account[object]") should be("bank_account")
    }

    "convert external account token" in {
      val token = "bach:the-complete-organ-works"
      val bankAccount = BankAccountData.Source.Token(token)
      val update = AccountUpdate.default.copy(externalAccount = Some(bankAccount))
      val map = PostParams.toPostParams(update)

      map("external_account") should be(token)
    }
  }
}
