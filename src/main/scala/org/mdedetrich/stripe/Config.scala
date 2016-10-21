package org.mdedetrich.stripe

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._

object Config {
  lazy val conf = ConfigFactory.load

  implicit lazy val endpoint           = Endpoint(conf.as[String]("stripe-scala.endpoint"))
  implicit lazy val fileUploadEndpoint = FileUploadEndpoint(conf.as[String]("stripe-scala.file-upload-endpoint"))

  implicit lazy val apiKey = ApiKey(conf.as[String]("stripe-scala.apiKey"))

  val numberOfRetries = conf.as[Int]("stripe-scala.numberOfRetries")
}
