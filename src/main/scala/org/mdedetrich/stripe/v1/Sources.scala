package org.mdedetrich.stripe.v1

/**
  * Contains base classes for input sources
  */

object Sources {

  trait BaseCardSource {
    def expMonth: Long

    def expYear: Long

    def number: String

    def cvc: Option[String]

    def addressCountry: Option[String]

    def addressLine1: Option[String]

    def addressLine2: Option[String]

    def name: Option[String]

    def addressState: Option[String]

    def addressZip: Option[String]
  }

}
