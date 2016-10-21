package org.mdedetrich.stripe.v1

/**
  * Contains base classes for input sources
  */
object Sources {

  trait BaseCardSource {
    def expMonth: Int
    def expYear: Int
    def cvc: Option[String]
    def addressCountry: Option[String]
    def addressLine1: Option[String]
    def addressLine2: Option[String]
    def name: Option[String]
    def addressState: Option[String]
    def addressZip: Option[String]
  }

  trait NumberCardSource extends BaseCardSource {
    def number: String
  }

  trait MaskedCardSource extends BaseCardSource {
    def last4: String
  }
}
