package org.mdedetrich.stripe.v1

import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.ExecutionContext

trait IntegrationTest extends AsyncWordSpec with Matchers with DefaultExecutionContext

trait DefaultExecutionContext {
  implicit val ec = ExecutionContext.global
}
