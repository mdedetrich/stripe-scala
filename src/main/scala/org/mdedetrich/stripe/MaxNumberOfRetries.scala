package org.mdedetrich.stripe

final case class MaxNumberOfRetries(numberOfRetries: Int) extends Exception {
  override def getMessage =
    s"Wen't over max number of retries, retries is $numberOfRetries"
}
