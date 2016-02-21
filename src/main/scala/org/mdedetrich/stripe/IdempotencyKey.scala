package org.mdedetrich.stripe

/**
  * Allows you to specify an idempotent key to prevent duplicate
  * requests (so you don't accidentally charge a customer twice in
  * case of network failures).
  * https://stripe.com/docs/api#idempotent_requests
  *
  * @param key
  */
case class IdempotencyKey(key: String) extends AnyVal
