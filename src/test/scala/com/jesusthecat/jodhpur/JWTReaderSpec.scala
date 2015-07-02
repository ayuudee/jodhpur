package com.jesusthecat.jodhpur

import java.time.{Clock, Instant, ZoneOffset, ZonedDateTime}

import com.jesusthecat.jodhpur.util.Base64
import org.scalatest.{FunSpec, Matchers, OptionValues}

class JWTReaderSpec
  extends FunSpec
  with Matchers
  with OptionValues {

  import scalaz._

  // From the JWS spec. (http://tools.ietf.org/html/draft-ietf-jose-json-web-signature-31#section-4.1.1)
  val specSecret = Base64.decodeToBytes("AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow").get
  val specCrypto = HasCrypto(specSecret)
  // {"typ":"JWT",\n "alg":"HS256"}
  val specHeader = "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9"
  // {"iss":"joe",\n "exp":1300819380,\n "http://example.com/is_root":true}
  val specClaims = "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ"
  val specSig = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
  val specToken = s"$specHeader.$specClaims.$specSig"
  val specExpiryInstant = Instant.ofEpochSecond(1300819380)

  def createSignedToken(header: String, claims: String) = {
    val payload = s"${Base64.encode(header)}.${Base64.encode(claims)}"
    val signature = Base64.encode(HmacSHA256.sign(specSecret, payload.getBytes))
    s"$payload.$signature"
  }

  // --------------------------------------------------------------------------

  describe("JWTReader") {

    describe("Validation") {

      it("should fail to parse JWT with no periods") {
        val res = new JWTReader(NoCrypto).read("AAAA")
        res should be(Failure(NonEmptyList(JWTReader.MsgCouldntParse)))
      }

      it("should fail to parse JWT with three periods") {
        val res = new JWTReader(NoCrypto).read("AAAA.BBBB.CCCC.DDDD")
        res should be(Failure(NonEmptyList(JWTReader.MsgCouldntParse)))
      }

      it("should fail if signatures don't match") {
        // Appending arbitrary characters to the signature invalidates it.
        val res = new JWTReader(specCrypto).read(specToken + "ASDF")
        res should be(Failure(NonEmptyList(JWTReader.MsgSecretRequired)))
      }

      it("should fail if there _is no_ signature") {
        val res = new JWTReader(specCrypto).read(s"$specHeader.$specClaims")
        res should be(Failure(NonEmptyList(JWTReader.MsgSecretRequired)))
      }

      it("should fail if there is no recognised algorithm") {
        val noMatchingAlgoHeader = Base64.encode("""{"typ":"JWT", "alg":"FOO"}""")
        val res = new JWTReader(specCrypto).read(s"$noMatchingAlgoHeader.$specClaims")
        res.isFailure should be(true)
      }

      it("should fail if there is no secret") {
        val res = new JWTReader(NoCrypto).read(specToken)
        res should be(Failure(NonEmptyList(JWTReader.MsgSecretRequired)))
      }

      it("should fail if token is expired") {
        val expiredInstant = Instant.now().minusSeconds(JWTReader.ThresholdSeconds * 2)
        val token = createSignedToken(
          header = """{"typ":"JWT", "alg":"HS256"}""",
          claims = s"""{"exp":${expiredInstant.getEpochSecond}}""")
        val res = new JWTReader(specCrypto).read(token)
        res should be(Failure(NonEmptyList(JWTReader.MsgExpired)))
      }

      it("should _not_ fail if token is within expiration threshold") {
        val expiredInstant = ZonedDateTime.now(ZoneOffset.UTC).toInstant
        val token = createSignedToken(
          header = """{"typ":"JWT", "alg":"HS256"}""",
          claims = s"""{"exp":${expiredInstant.getEpochSecond}}""")
        val res = new JWTReader(specCrypto).read(token)
        res.isSuccess should be(true)
      }

      it("should fail if issuer and token don't match"){
        val token = createSignedToken(
          header = """{"typ":"JWT", "alg":"HS256"}""",
          claims = s"""{"iss":"foo"}""")
        val res = new JWTReader(specCrypto, issuer = Some("bar")).read(token)
        res should be(Failure(NonEmptyList(JWTReader.MsgInvalidIssuer)))
      }

      it("should fail if token does not contain audience") {
        val token = createSignedToken(
          header = """{"typ":"JWT", "alg":"HS256"}""",
          claims = s"""{"aud":"foo"}""")
        val res = new JWTReader(specCrypto, audience = Some("bar")).read(token)
        res should be(Failure(NonEmptyList(JWTReader.MsgInvalidAudience)))
      }
    }

    describe("Plaintext JWT support") {

      implicit val clock = Clock.fixed(specExpiryInstant, ZoneOffset.UTC)

      it("should parse Plaintext JWT") {
        val token = "eyJ0eXAiOiJKV1QiLCAiYWxnIjoibm9uZSJ9.eyJpc3MiOiJqb2UiLCJleHAiOjEzMDA4MTkzODB9"
        val v = new JWTReader(NoCrypto, issuer = Some("joe")).read(token)
        val UnverifiedJwt(header, claims) = v.toOption.value
        header.alg should be(NoAlgorithm)
        header.typ.value should be("JWT")
        claims.iss.value should be(Left("joe"))
        claims.exp.value should be(IntDate(1300819380))
      }

    }

    describe("JWS token support") {

      implicit val clock = Clock.fixed(specExpiryInstant, ZoneOffset.UTC)

      it("should parse Signed JWT (with known secret)") {
        val jwt = new JWTReader(specCrypto, issuer = Some("joe")).read(specToken)
        val VerifiedJwt(header, claims) = jwt.toOption.value
        header.alg should be(HmacSHA256)
        header.typ.value should be("JWT")
        claims.iss.value should be(Left("joe"))
        claims.exp.value should be(IntDate(1300819380))
      }

      it("should parse Signed JWT (with embedded secret)") {
        // Not entirely sure how to handle this guy yet.
        pending
      }
    }

  }

}
