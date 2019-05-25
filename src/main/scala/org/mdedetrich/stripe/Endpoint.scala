package org.mdedetrich.stripe

/**
  * The stripe API endpoint. Make sure you leave out any
  * trailing '/' character.
  *
  * @param url
  */
final case class Endpoint(url: String) extends AnyVal

/**
  * The stripe file Upload API endpoint. Make sure you leave out any
  * trailing '/' character.
  *
  * @param url
  */
final case class FileUploadEndpoint(url: String) extends AnyVal
