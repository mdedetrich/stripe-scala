package org.mdedetrich.stripe.v1

import java.time.{DayOfWeek, LocalDate, OffsetDateTime}

import cats.syntax.either._
import io.circe.Json
import io.circe.parser._
import org.mdedetrich.stripe.PostParams
import org.mdedetrich.stripe.v1.Accounts.{
  Account,
  AccountInput,
  AccountUpdate,
  LegalEntity,
  TosAcceptance,
  TransferInterval,
  TransferSchedule
}
import org.mdedetrich.stripe.v1.BankAccounts.BankAccountData
import org.mdedetrich.stripe.v1.BankAccountsPaymentSource.BankAccount
import org.mdedetrich.stripe.v1.Shippings.Address
import org.scalatest.{Matchers, WordSpec}

class AccountsSpec extends WordSpec with Matchers {
  val address = Address(
    line1 = Option("Široka ulica"),
    line2 = Option("Apartman B1"),
    postalCode = Option("1234"),
    city = Option("Zadar"),
    country = Option("HR")
  )

  "Accounts" should {
    "parse JSON correctly" in {
      val in     = this.getClass.getResourceAsStream("/account.json")
      val string = scala.io.Source.fromInputStream(in).mkString
      val json   = parse(string).toOption

      val account = json.flatMap(_.as[Account].toOption).get
      account.id should be("acct_191dzJG5YhaiJXJY")
      account.legalEntity.address.country should be(Option("DE"))

      val ba = account.externalAccounts.data.head.asInstanceOf[BankAccount]
      ba.id should be("ba_191dzLG5YhaiJXJYeWv9KHmR")
      ba.last4 should be("3000")

      account.transferSchedule.interval.get should be(TransferInterval.Daily)

      account.verification.fieldsNeeded should be(Seq("legal_entity.verification.document"))
    }
  }

  "Account create POST params" should {

    "convert tos acceptance" in {
      val now    = OffsetDateTime.now()
      val ip     = "120.0.0.1"
      val update = AccountInput(tosAcceptance = Option(TosAcceptance(Option(now), Option(ip))))
      val map    = PostParams.toPostParams(update)

      map("tos_acceptance[date]") should be(now.toEpochSecond.toString)
      map("tos_acceptance[ip]") should be(ip)
    }

    "convert address" in {
      val input = AccountInput(legalEntity = Option(LegalEntity(address = address)))
      val map   = PostParams.toPostParams(input)

      map("legal_entity[address][line1]") should be(address.line1.get)
      map("legal_entity[address][line2]") should be(address.line2.get)
      map("legal_entity[address][city]") should be(address.city.get)
      map("legal_entity[address][postal_code]") should be(address.postalCode.get)
      map("legal_entity[address][country]") should be(address.country.get)
    }

    "convert transfer schedule" in {
      val input = AccountInput(
        transferSchedule = Option(TransferSchedule(Option(TransferInterval.Manual), None, Option(DayOfWeek.SUNDAY)))
      )
      val map = PostParams.toPostParams(input)
      map("transfer_schedule[interval]") should be("manual")
      map("transfer_schedule[weekly_anchor]") should be("sunday")
    }
  }

  "Account update POST params" should {

    "convert default currency" in {
      val update = AccountUpdate(defaultCurrency = Option(Currency.`Algerian Dinar`))
      val map    = PostParams.toPostParams(update)

      map("default_currency") should be("DZD")
    }

    "convert address" in {
      val update = AccountUpdate(legalEntity = Option(LegalEntity(address = address)))
      val map    = PostParams.toPostParams(update)

      map("legal_entity[address][line1]") should be(address.line1.get)
      map("legal_entity[address][line2]") should be(address.line2.get)
      map("legal_entity[address][city]") should be(address.city.get)
      map("legal_entity[address][postal_code]") should be(address.postalCode.get)
      map("legal_entity[address][country]") should be(address.country.get)
    }

    "convert dob" in {
      val date   = LocalDate.of(2016, 10, 6)
      val update = AccountUpdate(legalEntity = Option(LegalEntity(dob = Option(date))))
      val map    = PostParams.toPostParams(update)

      map("legal_entity[dob][year]") should be(date.getYear.toString)
      map("legal_entity[dob][month]") should be(date.getMonthValue.toString)
      map("legal_entity[dob][day]") should be(date.getDayOfMonth.toString)
    }

    "convert name" in {
      val first  = "Debbie"
      val last   = "Harry"
      val update = AccountUpdate(legalEntity = Option(LegalEntity(firstName = Option(first), lastName = Option(last))))
      val map    = PostParams.toPostParams(update)

      map("legal_entity[first_name]") should be(first)
      map("legal_entity[last_name]") should be(last)
    }

    "convert tos acceptance" in {
      val now       = OffsetDateTime.parse("2016-10-06T13:40:43Z")
      val ipAddress = "fd45:69e1:4b4e::"
      val update    = AccountUpdate(tosAcceptance = Option(TosAcceptance(Option(now), Option(ipAddress))))
      val map       = PostParams.toPostParams(update)

      map("tos_acceptance[date]") should be("1475761243")
      map("tos_acceptance[ip]") should be(ipAddress)
    }

    "convert external account data" in {
      val bankAccount = BankAccountData.Source.Object("DE89370400440532013000", "DE", Currency.`Euro`)
      val update      = AccountUpdate(externalAccount = Option(bankAccount))
      val map         = PostParams.toPostParams(update)

      map("external_account[object]") should be("bank_account")
    }

    "convert external account token" in {
      val token       = "bach:the-complete-organ-works"
      val bankAccount = BankAccountData.Source.Token(token)
      val update      = AccountUpdate(externalAccount = Option(bankAccount))
      val map         = PostParams.toPostParams(update)

      map("external_account") should be(token)
    }
  }

  "Day of week" should {
    "parse correctly" in {
      Accounts.dayOfWeekDecoder(Json.fromString("monday").hcursor).right.get should be(DayOfWeek.MONDAY)
    }
  }
}
