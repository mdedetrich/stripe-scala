package org.mdedetrich.stripe

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import org.mdedetrich.stripe.v1.FileUploadChunkTimeout

import scala.concurrent.duration.FiniteDuration

object Config {
  lazy val conf = ConfigFactory.load

  implicit lazy val endpoint           = Endpoint(conf.as[String]("stripe-scala.endpoint"))
  implicit lazy val fileUploadEndpoint = FileUploadEndpoint(conf.as[String]("stripe-scala.file-upload-endpoint"))

  implicit lazy val apiKey = ApiKey(conf.as[String]("stripe-scala.api-key"))

  implicit lazy val fileUploadChunkTimeout = FileUploadChunkTimeout(
    conf.as[FiniteDuration]("stripe-scala.file-upload-chunk-timeout"))

  val numberOfRetries = conf.as[Int]("stripe-scala.number-of-retries")
}
