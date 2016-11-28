package org.mdedetrich.stripe.v1

import java.time.{LocalDate, OffsetDateTime}

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.Accounts.LegalEntityType.Individual
import org.mdedetrich.stripe.v1.Accounts.{Account, LegalEntity, TosAcceptance, TransferInverval, TransferSchedule}
import org.mdedetrich.stripe.v1.BankAccounts.BankAccountData
import org.scalatest.ParallelTestExecution

import scala.concurrent.Future

class AccountIT extends IntegrationTest with ParallelTestExecution {

  "Account" should {
    "create managed account with non-ascii character" in {
      val dob = LocalDate.now().minusYears(30)
      val firstName = "Gaspard"
      val lastName  = "Augé"
      val legalEntity   = Some(LegalEntity.default.copy(`type` = Some(Individual),
        firstName = Some(firstName),
        lastName = Some(lastName),
        dob = Some(dob))
      )

      val accountInput = Accounts.AccountInput.default.copy(managed = true, legalEntity = legalEntity)
      handleIdempotent(Accounts.create(accountInput)).map({ account =>
        account.legalEntity.firstName.get should be(firstName)
        account.legalEntity.lastName.get should be(lastName)
      })
    }

    "create a managed account into which money can be paid" in {

      val managedAccount = AccountIT.createManagedAccountWithBankAccount

      managedAccount.map { account =>
        account shouldBe a[Accounts.Account]
        account.metadata should be(AccountIT.meta)
        account.transfersEnabled should be(true)
      }
    }
  }
}

object AccountIT extends DefaultExecutionContext{

  val meta = Map("foo" -> "bar")

  def createManagedAccountWithBankAccount: Future[Account] = {
    val dob           = LocalDate.now().minusYears(30)
    val tosAcceptance = Some(TosAcceptance(Some(OffsetDateTime.now()), Some("62.96.204.171")))
    val legalEntity   = Some(LegalEntity.default.copy(`type` = Some(Individual),
      firstName = Some("Horst"),
      lastName = Some("Kasuppke"),
      dob = Some(dob)))

    val transferSchedule = Some(TransferSchedule(Some(TransferInverval.Manual), None, None))

    // weirdly, this needs to be here in order for the following line not to throw a NullPointerException
    Currency.lowerCaseNamesToValuesMap
    val ba = Some(BankAccountData.Source.Object.default("DE89370400440532013000", "DE", Currency.`Euro`))

    val accountInput = Accounts.AccountInput.default.copy(managed = true, metadata = meta, transferSchedule = transferSchedule)
    val accountUpdate = Accounts.AccountUpdate.default.copy(
      tosAcceptance = tosAcceptance,
      legalEntity = legalEntity,
      externalAccount = ba,
      transferSchedule = transferSchedule
    )

    for {
      account <- handleIdempotent(Accounts.create(accountInput))
      updatedAccount <- handleIdempotent(Accounts.update(account.id, accountUpdate))
    } yield updatedAccount

  }
}
