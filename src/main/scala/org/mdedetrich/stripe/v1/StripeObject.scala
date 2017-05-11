package org.mdedetrich.stripe.v1

import io.circe.{Decoder, DecodingFailure, Encoder}
import cats.syntax.either._

abstract class StripeObject

object StripeObject {
  implicit val stripeObjectDecoder: Decoder[StripeObject] = Decoder.instance[StripeObject] { c =>
    c.downField("object").as[String].flatMap {
      case "customer"        => implicitly[Decoder[Customers.Customer]].map(x => x: StripeObject).apply(c)
      case "card"            => implicitly[Decoder[Cards.Card]].map(x => x: StripeObject).apply(c)
      case "transfer"        => implicitly[Decoder[Transfers.Transfer]].map(x => x: StripeObject).apply(c)
      case "balance"         => implicitly[Decoder[Balances.Balance]].map(x => x: StripeObject).apply(c)
      case "charge"          => implicitly[Decoder[Charges.Charge]].map(x => x: StripeObject).apply(c)
      case "application_fee" => implicitly[Decoder[ApplicationFees.ApplicationFee]].map(x => x: StripeObject).apply(c)
      case "account"         => implicitly[Decoder[Accounts.Account]].map(x => x: StripeObject).apply(c)
      case _                 => Left(DecodingFailure("Unknown Stripe Object", c.history))
    }
  }

  implicit val stripeObjectEncoder: Encoder[StripeObject] = Encoder.instance[StripeObject] {
    case obj: Customers.Customer             => implicitly[Encoder[Customers.Customer]].apply(obj)
    case obj: Cards.Card                     => implicitly[Encoder[Cards.Card]].apply(obj)
    case obj: Transfers.Transfer             => implicitly[Encoder[Transfers.Transfer]].apply(obj)
    case obj: Balances.Balance               => implicitly[Encoder[Balances.Balance]].apply(obj)
    case obj: Charges.Charge                 => implicitly[Encoder[Charges.Charge]].apply(obj)
    case obj: ApplicationFees.ApplicationFee => implicitly[Encoder[ApplicationFees.ApplicationFee]].apply(obj)
    case obj: Accounts.Account               => implicitly[Encoder[Accounts.Account]].apply(obj)
    case obj                                 => throw new IllegalArgumentException(s"Unable to encode object of type ${obj.getClass.getName}")
  }
}
