package com.jesusthecat.jodhpur

/**
 * Describe the cryptographic operations applied to the JWT and optionally,
 * additional properties of the JWT.
 *
 * @param typ used to declare the MIME Media Type of this complete JWT in
 *            contexts where this is useful to the application. It should
 *            generally be "JWT".
 * @param cty conveys structural information about the JWT. In the case
 *            that nested signing or encryption is employed, this Header
 *            Parameter MUST be present; in this case, the value MUST be "JWT",
 *            to indicate that a Nested JWT is carried in this JWT.
 * @param alg identifies the cryptographic algorithm used to secure the JWS.
 *            The signature, MAC, or plaintext value is not valid if the "alg"
 *            value does not represent a supported algorithm, or if there is
 *            not a key for use with that algorithm associated with the party
 *            that digitally signed or MACed the content.
 */
case class Header(alg: SigningAlgorithm,
                  typ: Option[String] = None,
                  cty: Option[String] = None)

object Header {

  import argonaut.Argonaut._
  import argonaut._

  implicit val HeaderCodecJson: CodecJson[Header] = {
    CodecJson(
      jencode3L((h: Header) => (h.alg, h.typ, h.cty))("alg", "typ", "cty").encode,
      c => for {
        alg <- (c --\ "alg").as[SigningAlgorithm]
        typ <- (c --\ "typ").as[String].option
        cty <- (c --\ "cty").as[String].option
      } yield Header(alg, typ, cty)
    )
  }

}