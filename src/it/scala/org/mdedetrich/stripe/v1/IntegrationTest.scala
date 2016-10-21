package org.mdedetrich.stripe.v1

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext

trait IntegrationTest extends WordSpec with Matchers with ScalaFutures with DefaultExecutionContext{
  implicit val defaultPatience = PatienceConfig(timeout = Span(10, Seconds), interval = Span(100, Millis))
}

trait DefaultExecutionContext {
  implicit val ec = ExecutionContext.global
}
