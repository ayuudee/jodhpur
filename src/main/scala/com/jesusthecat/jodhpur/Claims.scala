package com.jesusthecat.jodhpur

/**
 * The JWT Claims Set represents a JSON object whose members are the
 * claims conveyed by the JWT.
 *
 * Consumers of this class should note that all claims that are not
 * understood by implementations MUST be ignored. There are three
 * classes of JWT Claim Names: Registered Claim Names, Public Claim
 * Names, and Private Claim Names.
 *
 * @param iss identifies the principal that issued the JWT.
 * @param sub identifies the principal that is the subject of the JWT. The
 *            Claims in a JWT are normally statements  about the subject.
 *            The subject value MAY be scoped to be locally unique in the
 *            context of the issuer or MAY be globally unique.
 * @param aud The "aud" (audience) claim identifies the recipients that
 *            the JWT is intended for.  Each principal intended to process
 *            the JWT MUST identify itself with a value in the audience claim.
 *            If the principal processing the claim does not identify itself
 *            with a value in the "aud" claim when this claim is present, then
 *            the JWT MUST be rejected.
 * @param exp identifies the expiration time on or after which the JWT MUST
 *            NOT be accepted for processing
 * @param nbf The "nbf" (not before) claim identifies the time before which
 *            the JWT MUST NOT be accepted for processing.
 * @param iat the time at which the JWT was issued.
 * @param jti provides a unique identifier for the JWT. The identifier value
 *            MUST be assigned in a manner that ensures that there is a
 *            negligible probability that the same value will be accidentally
 *            assigned to a different data object.
 */
case class Claims(iss: Option[StringOrUri] = None,
                  sub: Option[StringOrUri] = None,
                  aud: Set[StringOrUri] = Set(),
                  exp: Option[IntDate] = None,
                  nbf: Option[IntDate] = None,
                  iat: Option[IntDate] = None,
                  jti: Option[String] = None) {

}

object Claims {

  import scalaz._, Scalaz._
  import argonaut._, Argonaut._

  implicit val ClaimsCodecJson: CodecJson[Claims] = {

    // TODO(AD): Come back and figure out the right way to use the hcursor
    // to deal with the aud kerfuffle below.
    def setOrSingle(c: ACursor): DecodeResult[Set[StringOrUri]] = {
      if (c.focus.exists(_.isArray)) {
        c.as[Set[StringOrUri]]
      } else if (c.focus.exists(_.isString)) {
        c.as[StringOrUri].map(v => Set(v))
      } else {
        // Default, just make it empty.
        DecodeResult.ok(Set())
      }
    }

    CodecJson(
      jencode7L((c: Claims) => (c.iss, c.sub, c.aud, c.exp, c.nbf, c.iat, c.jti))("iss", "sub", "aud", "exp", "nbf", "iat", "jti").encode,
      c => for {
        iss <- (c --\ "iss").as[StringOrUri].option
        sub <- (c --\ "sub").as[StringOrUri].option
        aud <- setOrSingle(c --\ "aud")
        exp <- (c --\ "exp").as[IntDate].option
        nbf <- (c --\ "nbf").as[IntDate].option
        iat <- (c --\ "iat").as[IntDate].option
        jti <- (c --\ "jti").as[String].option
      } yield Claims(
          iss,
          sub,
          aud,
          exp,
          nbf,
          iat,
          jti)
    )
  }

}