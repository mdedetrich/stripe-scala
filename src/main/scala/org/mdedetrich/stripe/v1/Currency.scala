package org.mdedetrich.stripe.v1

import enumeratum._

sealed abstract class CurrencyGroup

case object CurrencyGroup1 extends CurrencyGroup

case object CurrencyGroup2 extends CurrencyGroup

/**
  * List of currency codes supported by Stripe
  * @see https://support.stripe.com/questions/which-currencies-does-stripe-support
  * @param iso             ISO code for the currency
  * @param currencyGroup   Which currency group the currency is in                       
  * @param americanExpress Whether the code supports American Express on Stripe
  */

sealed abstract class Currency(val iso: String,
                               val currencyGroup: CurrencyGroup,
                               val americanExpress: Boolean = true) extends EnumEntry {
  override val entryName = iso
}

object Currency extends Enum[Currency] {

  val values = findValues

  case object `United Arab Emirates Dirham` extends Currency("AED", CurrencyGroup1)

  case object `Albanian Lek` extends Currency("ALL", CurrencyGroup1)

  case object `Netherlands Antillean Gulden` extends Currency("ANG", CurrencyGroup1)

  case object `Argentine Peso` extends Currency("ARS", CurrencyGroup1, false)

  case object `Australian Dollar` extends Currency("AUD", CurrencyGroup1)

  case object `Aruban Florin` extends Currency("AWG", CurrencyGroup1)

  case object `Barbadian Dollar` extends Currency("BBD", CurrencyGroup1)

  case object `Bangladeshi Taka` extends Currency("BDT", CurrencyGroup1)

  case object `Burundian Franc` extends Currency("BIF", CurrencyGroup1)

  case object `Bermudian Dollar` extends Currency("BMD", CurrencyGroup1)

  case object `Brunei Dollar` extends Currency("BND", CurrencyGroup1)

  case object `Bolivian Boliviano` extends Currency("BOB", CurrencyGroup1, false)

  case object `Brazilian Real` extends Currency("BRL", CurrencyGroup1, false)

  case object `Bahamian Dollar` extends Currency("BSD", CurrencyGroup1)

  case object `Botswana Pula` extends Currency("BWP", CurrencyGroup1)

  case object `Belize Dollar` extends Currency("BZD", CurrencyGroup1)

  case object `Canadian Dollar` extends Currency("CAD", CurrencyGroup1)

  case object `Swiss Franc` extends Currency("CHF", CurrencyGroup1)

  case object `Chilean Peso` extends Currency("CLP", CurrencyGroup1, false)

  case object `Chinese Renminbi Yuan` extends Currency("CNY", CurrencyGroup1, false)

  case object `Colombian Peso` extends Currency("COP", CurrencyGroup1, false)

  case object `Costa Rican Colón` extends Currency("CRC", CurrencyGroup1, false)

  case object `Cape Verdean Escudo` extends Currency("CVE", CurrencyGroup1, false)

  case object `Czech Koruna` extends Currency("CZK", CurrencyGroup1, false)

  case object `Djiboutian Franc` extends Currency("DJF", CurrencyGroup1, false)

  case object `Danish Krone` extends Currency("DKK", CurrencyGroup1)

  case object `Dominican Peso` extends Currency("DOP", CurrencyGroup1)

  case object `Algerian Dinar` extends Currency("DZD", CurrencyGroup1)

  case object `Egyptian Pound` extends Currency("EGP", CurrencyGroup1)

  case object `Ethiopian Birr` extends Currency("ETC", CurrencyGroup1)

  case object `Euro` extends Currency("EUR", CurrencyGroup1)

  case object `Fijian Dollar` extends Currency("FJD", CurrencyGroup1)

  case object `Falkland Islands Pound` extends Currency("FKP", CurrencyGroup1, false)

  case object `British Pound` extends Currency("GBP", CurrencyGroup1)

  case object `Gibraltar Pound` extends Currency("GIP", CurrencyGroup1)

  case object `GMD` extends Currency("GMD", CurrencyGroup1)

  case object `Guinean Franc` extends Currency("GNF", CurrencyGroup1, false)

  case object `Guatemalan Quetzal` extends Currency("GTQ", CurrencyGroup1, false)

  case object `Guyanese Dollar` extends Currency("GYD", CurrencyGroup1)

  case object `Hong Kong Dollar` extends Currency("HKD", CurrencyGroup1)

  case object `Honduran Lempira` extends Currency("HNL", CurrencyGroup1, false)

  case object `Croatian Kuna` extends Currency("HRK", CurrencyGroup1)

  case object `Haitian Gourde` extends Currency("HTG", CurrencyGroup1)

  case object `Hungarian Forint` extends Currency("HUF", CurrencyGroup1, false)

  case object `Indonesian Rupiah` extends Currency("IDR", CurrencyGroup1)

  case object `Israeli New Sheqel` extends Currency("ILS", CurrencyGroup1)

  case object `Indian Rupee` extends Currency("INR", CurrencyGroup1, false)

  case object `Icelandic Króna` extends Currency("ISK", CurrencyGroup1)

  case object `Jamaican Dollar` extends Currency("JMD", CurrencyGroup1)

  case object `Japanese Yen` extends Currency("JPY", CurrencyGroup1)

  case object `Kenyan Shilling` extends Currency("KES", CurrencyGroup1)

  case object `Cambodian Riel` extends Currency("KHR", CurrencyGroup1)

  case object `Comorian Franc` extends Currency("KMF", CurrencyGroup1)

  case object `South Korean Won` extends Currency("KRW", CurrencyGroup1)

  case object `Cayman Islands Dollar` extends Currency("KYD", CurrencyGroup1)

  case object `Kazakhstani Tenge` extends Currency("KZT", CurrencyGroup1)

  case object `Lao Kip` extends Currency("LAK", CurrencyGroup1, false)

  case object `Lebanese Pound` extends Currency("LBP", CurrencyGroup1)

  case object `Sri Lankan Rupee` extends Currency("LKR", CurrencyGroup1)

  case object `Liberian Dollar` extends Currency("LRD", CurrencyGroup1)

  case object `Moroccan Dirham` extends Currency("MAD", CurrencyGroup1)

  case object `Moldovan Leu` extends Currency("MDL", CurrencyGroup1)

  case object `Mongolian Tögrög` extends Currency("MNT", CurrencyGroup1)

  case object `Macanese Pataca` extends Currency("MOP", CurrencyGroup1)

  case object `Mauritanian Ouguiya` extends Currency("MRO", CurrencyGroup1)

  case object `Mauritian Rupee` extends Currency("MUR", CurrencyGroup1, false)

  case object `Maldivian Rufiyaa` extends Currency("MVR", CurrencyGroup1)

  case object `Malawian Kwacha` extends Currency("MWK", CurrencyGroup1)

  case object `Mexican Peso` extends Currency("MXN", CurrencyGroup1, false)

  case object `Malaysian Ringgit` extends Currency("MYR", CurrencyGroup1)

  case object `Namibian Dollar` extends Currency("NAD", CurrencyGroup1)

  case object `Nigerian Naira` extends Currency("NGN", CurrencyGroup1)

  case object `Nicaraguan Córdoba` extends Currency("NIO", CurrencyGroup1)

  case object `Norwegian Krone` extends Currency("NOK", CurrencyGroup1)

  case object `Nepalese Rupee` extends Currency("NPR", CurrencyGroup1)

  case object `New Zealand Dollar` extends Currency("NZD", CurrencyGroup1)

  case object `Panamanian Balboa` extends Currency("PAB", CurrencyGroup1, false)

  case object `Peruvian Nuevo Sol` extends Currency("PEN", CurrencyGroup1, false)

  case object `Papua New Guinean Kina` extends Currency("PGK", CurrencyGroup1, false)

  case object `Philippine Peso` extends Currency("PHP", CurrencyGroup1)

  case object `Pakistani Rupee` extends Currency("PKR", CurrencyGroup1)

  case object `Polish Złoty` extends Currency("PLN", CurrencyGroup1)

  case object `Paraguayan Guaraní` extends Currency("PYG", CurrencyGroup1, false)

  case object `Qatari Riyal` extends Currency("QAR", CurrencyGroup1, false)

  case object `Russian Ruble` extends Currency("RUB", CurrencyGroup1)

  case object `Saudi Riyal` extends Currency("SAR", CurrencyGroup1)

  case object `Solomon Islands Dollar` extends Currency("SBD", CurrencyGroup1)

  case object `Seychellois Rupee` extends Currency("SCR", CurrencyGroup1)

  case object `Swedish Krona` extends Currency("SEK", CurrencyGroup1)

  case object `Singapore Dollar` extends Currency("SGD", CurrencyGroup1)

  case object `Saint Helenian Pound` extends Currency("SHP", CurrencyGroup1)

  case object `Sierra Leonean Leone` extends Currency("SLL", CurrencyGroup1)

  case object `Somali Shilling` extends Currency("SOS", CurrencyGroup1)

  case object `São Tomé and Príncipe Dobra` extends Currency("STD", CurrencyGroup1)

  case object `Salvadoran Colón` extends Currency("SVC", CurrencyGroup1, false)

  case object `Swazi Lilangeni` extends Currency("SZL", CurrencyGroup1)

  case object `Thai Baht` extends Currency("THB", CurrencyGroup1)

  case object `Tongan Paʻanga` extends Currency("TOP", CurrencyGroup1)

  case object `Trinidad and Tobago Dollar` extends Currency("TTD", CurrencyGroup1)

  case object `New Taiwan Dollar` extends Currency("TWD", CurrencyGroup1)

  case object `Tanzanian Shilling` extends Currency("TZS", CurrencyGroup1)

  case object `Ukrainian Hryvnia` extends Currency("UAH", CurrencyGroup1)

  case object `Ugandan Shilling` extends Currency("UGX", CurrencyGroup1)

  case object `United States Dollar` extends Currency("USD", CurrencyGroup1)

  case object `Uruguayan Peso` extends Currency("UYU", CurrencyGroup1, false)

  case object `Uzbekistani Som` extends Currency("UZS", CurrencyGroup1)

  case object `Vietnamese Đồng` extends Currency("VND", CurrencyGroup1)

  case object `Vanuatu Vatu` extends Currency("VUV", CurrencyGroup1)

  case object `Samoan Tala` extends Currency("WST", CurrencyGroup1)

  case object `Central African Cfa Franc` extends Currency("XAF", CurrencyGroup1)

  case object `West African Cfa Franc` extends Currency("XOF", CurrencyGroup1, false)

  case object `Cfp Franc` extends Currency("XPF", CurrencyGroup1, false)

  case object `Yemeni Rial` extends Currency("YER", CurrencyGroup1, false)

  case object `South African Rand` extends Currency("ZAR", CurrencyGroup1)

  case object `Afghan Afghani` extends Currency("AFN", CurrencyGroup2, false)

  case object `Armenian Dram` extends Currency("AMD", CurrencyGroup2, false)

  case object `Angolan Kwanza` extends Currency("AOA", CurrencyGroup2, false)

  case object `Azerbaijani Manat` extends Currency("AZN", CurrencyGroup2)

  case object `Bosnia & Herzegovina Convertible Mark` extends Currency("BAM", CurrencyGroup2)

  case object `Bulgarian Lev` extends Currency("BGN", CurrencyGroup2)

  case object `Georgian Lari` extends Currency("GEL", CurrencyGroup2)

  case object `Kyrgyzstani Som` extends Currency("KGS", CurrencyGroup2)

  case object `Lesotho Loti` extends Currency("LSL", CurrencyGroup2)

  case object `Malagasy Ariary` extends Currency("MGA", CurrencyGroup2)

  case object `Macedonian Denar` extends Currency("MKD", CurrencyGroup2)

  case object `Mozambican Metical` extends Currency("MZN", CurrencyGroup2)

  case object `Romanian Leu` extends Currency("RON", CurrencyGroup2)

  case object `Serbian Dinar` extends Currency("RSD", CurrencyGroup2)

  case object `Rwandan Franc` extends Currency("RWF", CurrencyGroup2)

  case object `Surinamese Dollar` extends Currency("SRD", CurrencyGroup2)

  case object `Tajikistani Somoni` extends Currency("TJS", CurrencyGroup2)

  case object `Turkish Lira` extends Currency("TRY", CurrencyGroup2)

  case object `East Caribbean Dollar` extends Currency("XCD", CurrencyGroup2)

  case object `Zambian Kwacha` extends Currency("ZMW", CurrencyGroup2)

  implicit val currencyFormats = EnumFormats.formats(Currency, insensitive = true)

}
