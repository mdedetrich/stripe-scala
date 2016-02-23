package org.mdedetrich.stripe.v1

import com.typesafe.scalalogging.LazyLogging
import dispatch.Defaults._
import dispatch._
import org.joda.time.DateTime
import org.mdedetrich.stripe.{IdempotencyKey, InvalidJsonModelException, Endpoint, ApiKey}
import org.mdedetrich.stripe.v1.BitcoinReceivers.BitcoinReceiver
import org.mdedetrich.stripe.v1.Cards.Card
import org.mdedetrich.utforsca.SealedContents
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.mdedetrich.playjson.Utils._

import scala.concurrent.Future
import scala.util.Try

sealed trait PaymentSource

object PaymentSource extends LazyLogging {

  implicit val paymentSourcesReads: Reads[PaymentSource] =
    __.read[JsObject].flatMap { o =>
      (__ \ "object").read[String].flatMap {
        case "card" => __.read[Card].map(x => x: PaymentSource)
        case "bitcoin_receiver" => __.read[BitcoinReceiver].map(x => x: PaymentSource)
        case _ => Reads[PaymentSource](_ => JsError(ValidationError("UnknownPaymentSource")))
      }
    }

  implicit val paymentSourceWrites: Writes[PaymentSource] =
    Writes((paymentSource: PaymentSource) =>
      paymentSource match {
        case c: Card => Json.toJson(c)
        case b: BitcoinReceiver => Json.toJson(b)
      }
    )
}

case class UnknownPaymentSource(val id: String) extends Exception {
  override def getMessage = s"Unknown Payment Source, received $id"
}

object Cards {

  sealed abstract class Brand(val id: String)

  case class UnknownBrand(val id: String) extends Exception {
    override def getMessage = s"Unknown Brand, received $id"
  }

  object Brand {

    case object Visa extends Brand("Visa")

    case object `American Express` extends Brand("American Express")

    case object MasterCard extends Brand("MasterCard")

    case object Discover extends Brand("Discover")

    case object JCB extends Brand("JCB")

    case object `Diners Club` extends Brand("Diners Club")

    case object Unknown extends Brand("Unknown")

    lazy val all: Set[Brand] = SealedContents.values[Brand]

    implicit val brandReads: Reads[Brand] = Reads.of[String].map { brandId =>
      Brand.all.find(_.id == brandId).getOrElse {
        throw new UnknownBrand(brandId)
      }
    }

    implicit val brandWrites: Writes[Brand] =
      Writes((brand: Brand) => JsString(brand.id))
  }

  sealed abstract class Check(val id: String)

  case class UnknownCheck(val id: String) extends Exception {
    override def getMessage = s"Unknown Check, received $id"
  }

  object Check {

    case object Pass extends Check("pass")

    case object Fail extends Check("fail")

    case object Unavailable extends Check("unavailable")

    case object Unchecked extends Check("unchecked")

    lazy val all: Set[Check] = SealedContents.values[Check]
  }

  implicit val addressReads: Reads[Check] =
    Reads.of[String].map { addressLineCheckId =>
      Check.all.find(_.id == addressLineCheckId).getOrElse {
        throw new UnknownCheck(addressLineCheckId)
      }
    }

  implicit val addressWrites: Writes[Check] =
    Writes((addressLineCheck: Check) => JsString(addressLineCheck.id))

  sealed abstract class Funding(val id: String)

  case class UnknownFunding(val id: String) extends Exception {
    override def getMessage = s"Unknown Funding, received $id"
  }

  object Funding {

    case object Credit extends Funding("credit")

    case object Debit extends Funding("debit")

    case object Prepaid extends Funding("prepaid")

    case object Unknown extends Funding("unknown")

    lazy val all: Set[Funding] = SealedContents.values[Funding]
  }

  implicit val fundingReads: Reads[Funding] = Reads.of[String]
    .map { fundingId => Funding.all.find(_.id == fundingId).getOrElse {
      throw new UnknownFunding(fundingId)
    }
    }

  implicit val fundingWrites: Writes[Funding] =
    Writes((funding: Funding) => JsString(funding.id))

  sealed abstract class TokenizationMethod(val id: String)

  case class UnknownTokenizationMethod(val id: String) extends Exception {
    override def getMessage = s"Unknown Tokenization Method, received $id"
  }

  object TokenizationMethod {

    case object ApplePay extends TokenizationMethod("apple_pay")

    case object AndroidPay extends TokenizationMethod("android_pay")

    lazy val all: Set[TokenizationMethod] = SealedContents.values[TokenizationMethod]
  }

  implicit val tokenizationMethodReads: Reads[TokenizationMethod] =
    Reads.of[String].map { tokenizationMethodId => TokenizationMethod.all.find(_.id == tokenizationMethodId)
      .getOrElse {
        throw new UnknownTokenizationMethod(tokenizationMethodId)
      }
    }

  implicit val tokenizationMethodWrites: Writes[TokenizationMethod] =
    Writes((tokenizationMethod: TokenizationMethod) => JsString(tokenizationMethod.id))

  case class Card(id: String,
                  addressCity: Option[String],
                  addressCountry: Option[String],
                  addressLine1: Option[String],
                  addressLine1Check: Option[Check],
                  addressLine2: Option[String],
                  addressState: Option[String],
                  addressZip: Option[String],
                  addressZipCheck: Option[Check],
                  brand: Brand,
                  country: Option[String],
                  customer: Option[String],
                  cvcCheck: Option[Check],
                  dynamicLast4: Option[String],
                  expMonth: Int,
                  expYear: Int,
                  funding: Funding,
                  last4: String,
                  metadata: Option[Map[String, String]],
                  name: Option[String],
                  tokenizationMethod: Option[TokenizationMethod]) extends StripeObject with PaymentSource

  implicit val cardReads: Reads[Card] = (
    (__ \ "id").read[String] ~
      (__ \ "address_city").readNullable[String] ~
      (__ \ "address_country").readNullable[String] ~
      (__ \ "address_line1").readNullable[String] ~
      (__ \ "address_line1_check").readNullable[Check] ~
      (__ \ "address_line2").readNullable[String] ~
      (__ \ "address_state").readNullable[String] ~
      (__ \ "address_zip").readNullable[String] ~
      (__ \ "address_zip_check").readNullable[Check] ~
      (__ \ "brand").read[Brand] ~
      (__ \ "country").readNullable[String] ~
      (__ \ "customer").readNullable[String] ~
      (__ \ "cvc_check").readNullable[Check] ~
      (__ \ "dynamic_last4").readNullable[String] ~
      (__ \ "exp_month").read[Int] ~
      (__ \ "exp_year").read[Int] ~
      (__ \ "funding").read[Funding] ~
      (__ \ "last4").read[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "name").readNullable[String] ~
      (__ \ "tokenization_method").readNullable[TokenizationMethod]
    ).tupled.map(Card.tupled)

  implicit val cardWrites: Writes[Card] =
    Writes((card: Card) =>
      Json.obj(
        "id" -> card.id,
        "object" -> "card",
        "address_city" -> card.addressCity,
        "address_country" -> card.addressCountry,
        "address_line1" -> card.addressLine1,
        "address_line1_check" -> card.addressLine1Check,
        "address_line2" -> card.addressLine2,
        "address_state" -> card.addressState,
        "address_zip" -> card.addressZip,
        "address_zip_check" -> card.addressZipCheck,
        "brand" -> card.brand,
        "country" -> card.country,
        "customer" -> card.customer,
        "cvc_check" -> card.cvcCheck,
        "dynamic_last4" -> card.dynamicLast4,
        "exp_month" -> card.expMonth,
        "exp_year" -> card.expYear,
        "funding" -> card.funding,
        "last4" -> card.last4,
        "metadata" -> card.metadata,
        "name" -> card.name,
        "tokenization_method" -> card.tokenizationMethod
      )
    )

}

object BitcoinReceivers extends LazyLogging {

  case class Transaction(id: String,
                         amount: BigDecimal,
                         bitcoinAmount: BigDecimal,
                         created: DateTime,
                         currency: Currency,
                         receiver: String
                        )

  implicit val transactionReads: Reads[Transaction] = (
    (__ \ "id").read[String] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "bitcoin_amount").read[BigDecimal] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").read[Currency] ~
      (__ \ "receiver").read[String]
    ).tupled.map(Transaction.tupled)

  implicit val transactionWrites: Writes[Transaction] =
    Writes((transaction: Transaction) =>
      Json.obj(
        "id" -> transaction.id,
        "object" -> "list",
        "amount" -> transaction.amount,
        "bitcoin_amount" -> transaction.bitcoinAmount,
        "created" -> transaction.created.getMillis / 1000,
        "currency" -> transaction.currency,
        "receiver" -> transaction.receiver
      )
    )

  case class BitcoinTransactions(data: List[Transaction],
                                 hasMore: Boolean,
                                 totalCount: Long,
                                 url: String
                                )

  implicit val bitcoinTransactionsReads: Reads[BitcoinTransactions] = (
    (__ \ "data").read[List[Transaction]] ~
      (__ \ "has_more").read[Boolean] ~
      (__ \ "total_count").read[Long] ~
      (__ \ "url").read[String]
    ).tupled.map(BitcoinTransactions.tupled)

  implicit val bitcoinTransactionsWrites: Writes[BitcoinTransactions] =
    Writes((bitcoinTransactions: BitcoinTransactions) =>
      Json.obj(
        "data" -> bitcoinTransactions.data,
        "has_more" -> bitcoinTransactions.hasMore,
        "total_count" -> bitcoinTransactions.totalCount,
        "url" -> bitcoinTransactions.url
      )
    )

  case class BitcoinReceiver(id: String,
                             active: Boolean,
                             amount: BigDecimal,
                             amountReceived: BigDecimal,
                             bitcoinAmount: BigDecimal,
                             bitcoinAmountReceived: BigDecimal,
                             bitcoinUri: String,
                             created: DateTime,
                             currency: Currency,
                             customer: String,
                             description: String,
                             email: String,
                             filled: Boolean,
                             inboundAddress: String,
                             livemode: Boolean,
                             metadata: Option[Map[String, String]],
                             payment: Option[String],
                             refundAddress: Option[String],
                             transactions: Option[BitcoinTransactions],
                             uncapturedFunds: Boolean,
                             usedForPayment: Boolean
                            ) extends StripeObject with PaymentSource

  implicit val bitcoinReceiverReads: Reads[BitcoinReceiver] = (
    (__ \ "id").read[String] ~
      (__ \ "active").read[Boolean] ~
      (__ \ "amount").read[BigDecimal] ~
      (__ \ "amount_received").read[BigDecimal] ~
      (__ \ "bitcoin_amount").read[BigDecimal] ~
      (__ \ "bitcoin_amount_received").read[BigDecimal] ~
      (__ \ "bitcoin_uri").read[String] ~
      (__ \ "created").read[Long].map { timestamp => new DateTime(timestamp * 1000) } ~
      (__ \ "currency").read[Currency] ~
      (__ \ "customer").read[String] ~
      (__ \ "description").read[String] ~
      (__ \ "email").read[String] ~
      (__ \ "filled").read[Boolean] ~
      (__ \ "inbound_address").read[String] ~
      (__ \ "livemode").read[Boolean] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "payment").readNullable[String] ~
      (__ \ "refund_address").readNullable[String] ~
      (__ \ "transactions").readNullable[BitcoinTransactions] ~
      (__ \ "uncaptured_funds").read[Boolean] ~
      (__ \ "used_for_payment").read[Boolean]
    ).tupled.map(BitcoinReceiver.tupled)

  implicit val bitcoinReceiverWrites: Writes[BitcoinReceiver] =
    Writes((bitcoinReceiver: BitcoinReceiver) =>
      Json.obj(
        "id" -> bitcoinReceiver.id,
        "object" -> "bitcoin_receiver",
        "active" -> bitcoinReceiver.active,
        "amount" -> bitcoinReceiver.amount,
        "amount_received" -> bitcoinReceiver.amountReceived,
        "bitcoin_amount" -> bitcoinReceiver.bitcoinAmount,
        "bitcoin_amount_received" -> bitcoinReceiver.bitcoinAmountReceived,
        "bitcoin_uri" -> bitcoinReceiver.bitcoinUri,
        "created" -> bitcoinReceiver.created.getMillis / 1000,
        "currency" -> bitcoinReceiver.currency,
        "customer" -> bitcoinReceiver.customer,
        "description" -> bitcoinReceiver.description,
        "email" -> bitcoinReceiver.email,
        "filled" -> bitcoinReceiver.filled,
        "inbound_address" -> bitcoinReceiver.inboundAddress,
        "livemode" -> bitcoinReceiver.livemode,
        "metadata" -> bitcoinReceiver.metadata,
        "payment" -> bitcoinReceiver.payment,
        "refund_address" -> bitcoinReceiver.refundAddress,
        "transactions" -> bitcoinReceiver.transactions,
        "uncaptured_funds" -> bitcoinReceiver.uncapturedFunds,
        "used_for_payment" -> bitcoinReceiver.usedForPayment
      )
    )

  case class BitcoinReceiverInput(amount: BigDecimal,
                                  currency: Currency,
                                  email: String,
                                  description: Option[String],
                                  metadata: Option[Map[String, String]],
                                  refundMispayments: Boolean
                                 )

  implicit val bitcoinReceiverInputReads: Reads[BitcoinReceiverInput] = (
    (__ \ "amount").read[BigDecimal] ~
      (__ \ "currency").read[Currency] ~
      (__ \ "email").read[String] ~
      (__ \ "description").readNullable[String] ~
      (__ \ "metadata").readNullableOrEmptyJsObject[Map[String, String]] ~
      (__ \ "refund_mispayments").read[Boolean]
    ).tupled.map(BitcoinReceiverInput.tupled)

  implicit val bitcoinReceiverInputWrites: Writes[BitcoinReceiverInput] =
    Writes((bitcoinReceiverInput: BitcoinReceiverInput) =>
      Json.obj(
        "amount" -> bitcoinReceiverInput.amount,
        "currency" -> bitcoinReceiverInput.currency,
        "email" -> bitcoinReceiverInput.email,
        "description" -> bitcoinReceiverInput.description,
        "metadata" -> bitcoinReceiverInput.metadata,
        "refund_mispayments" -> bitcoinReceiverInput.refundMispayments
      )
    )

  def create(bitcoinReceiverInput: BitcoinReceiverInput,
             idempotencyKey: Option[IdempotencyKey] = None
            )
            (implicit apiKey: ApiKey,
             endpoint: Endpoint): Future[Try[BitcoinReceiver]] = {

    val postFormParameters: Map[String, String] = {
      Map(
        "amount" -> Option(bitcoinReceiverInput.amount.toString()),
        "currency" -> Option(bitcoinReceiverInput.currency.iso.toLowerCase()),
        "email" -> Option(bitcoinReceiverInput.email),
        "description" -> bitcoinReceiverInput.description,
        "refund_mispayments" -> Option(bitcoinReceiverInput.refundMispayments.toString)
      ).collect {
        case (k, Some(v)) => (k, v)
      }
    } ++ mapToPostParams(bitcoinReceiverInput.metadata, "metadata")

    logger.debug(s"Generated POST form parameters is $postFormParameters")

    val finalUrl = endpoint.url + "/v1/bitcoin/receivers"

    val req = {
      val r = (
        url(finalUrl)
          .addHeader("Content-Type", "application/x-www-form-urlencoded")
          << postFormParameters
        ).POST.as(apiKey.apiKey, "")

      idempotencyKey match {
        case Some(key) =>
          r.addHeader(idempotencyKeyHeader, key.key)
        case None =>
          r
      }
    }

    Http(req).map { response =>

      parseStripeServerError(response, finalUrl, Option(postFormParameters), None)(logger) match {
        case Right(triedJsValue) =>
          triedJsValue.map { jsValue =>
            val jsResult = Json.fromJson[BitcoinReceiver](jsValue)
            jsResult.fold(
              errors => {
                throw InvalidJsonModelException(response.getStatusCode, finalUrl, Option(postFormParameters), None, jsValue, errors)
              }, bitcoinReceiver => bitcoinReceiver
            )
          }
        case Left(error) =>
          scala.util.Failure(error)
      }
    }
  }

}