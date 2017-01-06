package org.mdedetrich.stripe.v1

import org.mdedetrich.stripe.Config._
import org.mdedetrich.stripe.v1.Charges.Source.Customer
import org.mdedetrich.stripe.v1.Refunds.{Reason, RefundInput}
import org.scalatest.ParallelTestExecution

class RefundIT extends IntegrationTest with ParallelTestExecution {

  "Refund" should {
    "refund a charge" in {

      val customerF = CustomerIT.createCustomerWithCC(CustomerIT.cardBypassingPendingCheck)

      val f = for {
        customer <- customerF
        charge   <- handleIdempotent(Charges.create(ChargeIT.chargeInput(Customer(customer.id))))
        refund   <- handleIdempotent(Refunds.create(RefundInput.default(charge.id, Reason.RequestedByCustomer)))
      } yield refund

      f.map { refund =>
        refund.amount should be(1500)
      }
    }
  }
}
