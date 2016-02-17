package org.mdedetrich.stripe.v1

import play.api.libs.json._
import play.api.libs.functional.syntax._

object Shippings {

  case class Address(city: String,
                     country: String,
                     line1: String,
                     line2: String,
                     postalCode: String,
                     state: String
                    )

  implicit val addressReads: Reads[Address] = (
    (__ \ "city").read[String] ~
      (__ \ "country").read[String] ~
      (__ \ "line1").read[String] ~
      (__ \ "line2").read[String] ~
      (__ \ "postal_code").read[String] ~
      (__ \ "state").read[String]
    ).tupled.map(Address.tupled)

  implicit val addressWrites: Writes[Address] =
    Writes((address: Address) =>
      Json.obj(
        "city" -> address.city,
        "country" -> address.country,
        "line1" -> address.line1,
        "line2" -> address.line2,
        "postal_code" -> address.postalCode,
        "state" -> address.state
      )
    )

  case class Shipping(address: Address,
                      carrier: Option[String],
                      name: String,
                      phone: String,
                      trackingNumber: Option[String]
                     )

  implicit val shippingReads: Reads[Shipping] = (
    (__ \ "address").read[Address] ~
      (__ \ "carrier").readNullable[String] ~
      (__ \ "name").read[String] ~
      (__ \ "phone").read[String] ~
      (__ \ "tracking_number").readNullable[String]
    ).tupled.map(Shipping.tupled)

  implicit val shippingWrites: Writes[Shipping] =
    Writes((shipping: Shipping) =>
      Json.obj(
        "address" -> shipping.address,
        "name" -> shipping.name,
        "phone" -> shipping.phone
      )
    )

}
