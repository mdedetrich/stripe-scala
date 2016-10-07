package org.mdedetrich.stripe.v1

import play.api.libs.json._
import play.api.libs.functional.syntax._

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
  case class Address(city: Option[String],
                     country: Option[String],
                     line1: Option[String],
                     line2: Option[String],
                     postalCode: Option[String],
                     state: Option[String])

  object Address {
    def default: Address = Address(None, None, None, None, None, None)
  }

  implicit val addressReads: Reads[Address] = (
      (__ \ "city").readNullable[String] ~
      (__ \ "country").readNullable[String] ~
      (__ \ "line1").readNullable[String] ~
      (__ \ "line2").readNullable[String] ~
      (__ \ "postal_code").readNullable[String] ~
      (__ \ "state").readNullable[String]
  ).tupled.map((Address.apply _).tupled)

  implicit val addressWrites: Writes[Address] = Writes(
      (address: Address) =>
        Json.obj(
            "city" -> address.city,
            "country" -> address.country,
            "line1" -> address.line1,
            "line2" -> address.line2,
            "postal_code" -> address.postalCode,
            "state" -> address.state
      ))

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
  case class Shipping(address: Option[Address],
                      carrier: Option[String],
                      name: Option[String],
                      phone: Option[String],
                      trackingNumber: Option[String])

  implicit val shippingReads: Reads[Shipping] = (
      (__ \ "address").readNullable[Address] ~
      (__ \ "carrier").readNullable[String] ~
      (__ \ "name").readNullable[String] ~
      (__ \ "phone").readNullable[String] ~
      (__ \ "tracking_number").readNullable[String]
  ).tupled.map((Shipping.apply _).tupled)

  implicit val shippingWrites: Writes[Shipping] = Writes(
      (shipping: Shipping) =>
        Json.obj(
            "address" -> shipping.address,
            "name" -> shipping.name,
            "phone" -> shipping.phone
      ))
}
