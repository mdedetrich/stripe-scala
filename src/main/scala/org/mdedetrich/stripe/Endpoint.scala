package org.mdedetrich.stripe

/**
  * The stripe API endpoint. Make sure you leave out any
  * trailing '/' character.
  * @param url
  */
case class Endpoint(val url: String) extends AnyVal
