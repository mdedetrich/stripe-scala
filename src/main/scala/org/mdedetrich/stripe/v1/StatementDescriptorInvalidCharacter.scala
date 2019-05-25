package org.mdedetrich.stripe.v1

final case class StatementDescriptorInvalidCharacter(character: String) extends Exception {
  override def getMessage =
    s"Statement Descriptor must not contain invalid characters, found $character"
}
