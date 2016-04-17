package org.mdedetrich.stripe.v1

/**
  * Thrown in the statement descriptor is too long
  *
  * @param length The length of the requested statement descriptor
  */

case class StatementDescriptorTooLong(length: Int) extends Exception {
  override def getMessage = s"Statement Descriptor must not be longer than 22 characters, input was $length characters"
}
