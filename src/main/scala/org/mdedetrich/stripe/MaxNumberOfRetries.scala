package org.mdedetrich.stripe

case class MaxNumberOfRetries(val numberOfRetries: Int) extends Exception {
  override def getMessage = s"Wen't over max number of retries, retries is $numberOfRetries"
}