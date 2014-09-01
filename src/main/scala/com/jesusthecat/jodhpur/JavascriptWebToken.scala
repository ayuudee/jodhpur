package com.jesusthecat.jodhpur

/** A Javascript Web Token representation.
  * @see http://tools.ietf.org/html/draft-ietf-oauth-json-web-token-19 */
sealed trait JavascriptWebToken {

  def header: Header

  def claims: Claims

}

/** A token that has been verified (where verified means validated
  * against a signature). */
case class VerifiedJwt(header: Header, claims: Claims) extends JavascriptWebToken

/** A token that has not been verified. */
case class UnverifiedJwt(header: Header, claims: Claims) extends JavascriptWebToken