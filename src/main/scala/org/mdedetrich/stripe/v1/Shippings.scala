package org.mdedetrich.stripe.v1

import io.circe.{Decoder, Encoder}

object Shippings {

  /**
    * @see https://stripe.com/docs/api#charge_object-shipping-address
    * @param city       City/Suburb/Town/Village
    * @param country    2-letter country code
    * @param line1      Address line 1 (Street address/PO Box/Company name)
    * @param line2      Address line 2 (Apartment/Suite/Unit/Building)
    * @param postalCode Zip/Postal Code
    * @param state      State/Province/County
    */
  case class Address(
      city: Option[String] = None,
      country: Option[String] = None,
      line1: Option[String] = None,
      line2: Option[String] = None,
      postalCode: Option[String] = None,
      state: Option[String] = None
  )

  implicit val addressDecoder: Decoder[Address] = Decoder.forProduct6(
    "city",
    "country",
    "line1",
    "line2",
    "postal_code",
    "state"
  )(Address.apply)

  implicit val addressEncoder: Encoder[Address] = Encoder.forProduct6(
    "city",
    "country",
    "line1",
    "line2",
    "postal_code",
    "state"
  )(
    x =>
      (
        x.city,
        x.country,
        x.line1,
        x.line2,
        x.postalCode,
        x.state
      )
  )

  /**
    * @see https://stripe.com/docs/api#charge_object-shipping
    * @param address        Shipping address.
    * @param carrier        The delivery service that shipped a physical product,
    *                       such as Fedex, UPS, USPS, etc.
    * @param name           Recipient name.
    * @param phone          Recipient phone (including extension).
    * @param trackingNumber The tracking number for a physical product,
    *                       obtained from the delivery service.
    *                       If multiple tracking numbers
    *                       were generated for this purchase, please
    *                       separate them with commas.
    */
  case class Shipping(
      address: Option[Address],
      carrier: Option[String],
      name: Option[String],
      phone: Option[String],
      trackingNumber: Option[String]
  )

  implicit val shippingDecoder: Decoder[Shipping] = Decoder.forProduct5(
    "address",
    "carrier",
    "name",
    "phone",
    "tracking_number"
  )(Shipping.apply)

  implicit val shippingEncoder: Encoder[Shipping] = Encoder.forProduct5(
    "address",
    "carrier",
    "name",
    "phone",
    "tracking_number"
  )(
    x =>
      (
        x.address,
        x.carrier,
        x.name,
        x.phone,
        x.trackingNumber
      )
  )
}
