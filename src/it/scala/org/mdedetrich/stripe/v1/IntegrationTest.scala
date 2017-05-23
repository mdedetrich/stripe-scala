package org.mdedetrich.stripe.v1

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.ExecutionContext

trait IntegrationTest extends AsyncWordSpec with Matchers with DefaultDependencies

trait DefaultDependencies {
  implicit val system       = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val http         = Http()
  implicit val ec           = ExecutionContext.global
}
