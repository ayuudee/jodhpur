package com.jesusthecat.jodhpur

import java.time.{Clock, Instant}

import com.jesusthecat.jodhpur.util.Base64

import scalaz._, Scalaz._
import argonaut._

/** Reads [[JavascriptWebToken]] from JSON.
  * 
  * @param cryptoParams The cryptographic parameters that are expected to have been used
  * to sign a JWT. If `cryptoParams` is [[com.jesusthecat.jodhpur.NoCrypto]], only
  * unverified JWTs are accepted; otherwise, unverified JWTs are not accepted.
  */
class JWTReader(cryptoParams: CryptoParams,
                audience: Option[StringOrUri] = None,
                issuer: Option[StringOrUri] = None)
               (implicit clock: Clock = Clock.systemUTC()) {

  import JWTReader._

  /** Try read a [[JavascriptWebToken]] or provide errors (via validation). */
  def read(str: String): ValidationNel[String, JavascriptWebToken] = {
    parseToComponents(str) flatMap decodeToJwt
  }

  // --------------------------------------------------------------------------

  /**
   * 1. The JWT MUST contain at least one period ('.') character.
   * 2. Let the Encoded JWT Header be the portion of the JWT before the
   * first period ('.') character.
   * 3. The Encoded JWT Header MUST be successfully base64url decoded
   * following the restriction given in this specification that no
   * padding characters have been used.
   */
  private def parseToComponents(raw: String): ErrorsOr[EncodedRep] = {
    (Option(raw) map (_.split('.')) filter (_.size <= 3) flatMap {
      case Array(h, c, s) => Some(EncodedRep(h, c, Some(s)))
      case Array(h, c) => Some(EncodedRep(h, c))
      case v => None
    }).toSuccess(MsgCouldntParse).toValidationNel
  }

  /** Decode the JWT from Base64 to JSON and then validate the claims
    * and headers as apporiate. */
  private def decodeToJwt(rep: EncodedRep): ValidationNel[String, JavascriptWebToken] = {
    (rep.header |@| rep.claims) {
      case (h, c) => (h, c)
    } flatMap {
      case (h, c) =>
        val header = Parse.decodeValidation[Header](h).toValidationNel
        val claims = Parse.decodeValidation[Claims](c).toValidationNel
        for {
          unverified <- (header |@| claims)(UnverifiedJwt.apply)
          jwt <- verify(unverified, rep)
        } yield jwt
    }
  }

  /** Check the signature (as appropriate) */
  private def verify(unverified: UnverifiedJwt,
    rep: EncodedRep): ValidationNel[String, JavascriptWebToken] = {
    if(unverified.header.alg != cryptoParams.algorithm) {
      MsgSecretRequired.failNel
    } else {
      ((unverified.header.alg, rep.signatureBytesDecoded) match {

        case (NoAlgorithm, _) => unverified.successNel

        case (alg, Some(sbx)) =>
          import java.util.Arrays
          cryptoParams match {
            case HasCrypto(secret, expectedAlg) if
              Arrays.equals(alg.sign(secret, rep.signingInput), sbx) =>
            VerifiedJwt(unverified.header, unverified.claims).successNel
          case _ => MsgSecretRequired.failNel
        }

        // Not enough information to verify.
        case _ => MsgSecretRequired.failNel
      }) flatMap {jwt =>
        (validateExpiry(jwt) |@| validateIssuer(jwt) |@| validateAudience(jwt)) {
          case (_, _, v) => v
        }
      }
    }
  }

  private def validateExpiry(jwt: JavascriptWebToken): ValidationNel[String, JavascriptWebToken] = {
    if (jwt.claims.exp.fold(true)(_.isAfter(clock.instant(), ThresholdSeconds))) jwt.successNel
    else MsgExpired.failNel
  }

  private def validateIssuer(jwt: JavascriptWebToken): ValidationNel[String, JavascriptWebToken] = {
    if (issuer == jwt.claims.iss) jwt.successNel
    else MsgInvalidIssuer.failNel
  }

  private def validateAudience(jwt: JavascriptWebToken): ValidationNel[String, JavascriptWebToken] = {
    if (audience.fold(true)(a => jwt.claims.aud.contains(a))) jwt.successNel
    else MsgInvalidAudience.failNel
  }

}

object JWTReader {

  // Constants.
  val UTF8 = "UTF-8"
  val ThresholdSeconds = 60 * 2

  // Messages.
  val MsgCouldntParse = "Couldn't parse JWT components from provided token"
  val MsgMalformedHeader = "Malformed header: couldn't wrangle JSON from Base64"
  val MsgMalformedClaims = "Malformed Claims Set: couldn't wrangle JSON from Base64"
  val MsgExpired = "The provided token is expired"
  val MsgInvalidIssuer = "The provided issuer was not recognised"
  val MsgInvalidAudience = "The provided audience was invalid"
  val MsgSecretRequired = "The signature of the provided token could not be verified"

  // Helper types.
  type ErrorsOr[A] = ValidationNel[String, A]
  type Validator[A] = String => ErrorsOr[A]

  // A convenience class for the Base64 encoded form of the read JWT.
  private case class EncodedRep(h64: String, c64: String, s64: Option[String] = None) {

    val header = Base64.decode(h64).toSuccess(MsgMalformedHeader).toValidationNel

    val claims = Base64.decode(c64).toSuccess(MsgMalformedClaims).toValidationNel

    def signatureBytesDecoded = s64 flatMap Base64.decodeToBytes

    def signingInput = s"$h64.$c64".getBytes(UTF8)

  }

}
