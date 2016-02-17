package org.mdedetrich.stripe

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._

object Config {
  val conf = ConfigFactory.load
  
  implicit lazy val endpoint = Endpoint(conf.as[String]("stripe-scala.endpoint"))
  
  implicit lazy val apiKey = ApiKey(conf.as[String]("stripe-scala.apiKey"))
}
