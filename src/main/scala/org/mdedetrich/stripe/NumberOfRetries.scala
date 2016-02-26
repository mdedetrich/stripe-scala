package org.mdedetrich.stripe

case class NumberOfRetries(numberOfRetries: Long) extends AnyVal

case class MaxNumberOfRetries(val numberOfRetries: Int) extends Exception {
  override def getMessage = s"Wen't over max number of retries, retries is $numberOfRetries"
}