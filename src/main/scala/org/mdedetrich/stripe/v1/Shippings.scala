package org.mdedetrich.stripe.v1

import play.api.libs.json._
import play.api.libs.functional.syntax._

object Shippings {

  case class Address(city: Option[String],
                     country: Option[String],
                     line1: Option[String],
                     line2: Option[String],
                     postalCode: Option[String],
                     state: Option[String]
                    )

  implicit val addressReads: Reads[Address] = (
    (__ \ "city").readNullable[String] ~
      (__ \ "country").readNullable[String] ~
      (__ \ "line1").readNullable[String] ~
      (__ \ "line2").readNullable[String] ~
      (__ \ "postal_code").readNullable[String] ~
      (__ \ "state").readNullable[String]
    ).tupled.map((Address.apply _).tupled)

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

  case class Shipping(address: Option[Address],
                      carrier: Option[String],
                      name: Option[String],
                      phone: Option[String],
                      trackingNumber: Option[String]
                     )

  implicit val shippingReads: Reads[Shipping] = (
    (__ \ "address").readNullable[Address] ~
      (__ \ "carrier").readNullable[String] ~
      (__ \ "name").readNullable[String] ~
      (__ \ "phone").readNullable[String] ~
      (__ \ "tracking_number").readNullable[String]
    ).tupled.map((Shipping.apply _).tupled)

  implicit val shippingWrites: Writes[Shipping] =
    Writes((shipping: Shipping) =>
      Json.obj(
        "address" -> shipping.address,
        "name" -> shipping.name,
        "phone" -> shipping.phone
      )
    )

}
