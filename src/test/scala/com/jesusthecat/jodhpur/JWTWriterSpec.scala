package com.jesusthecat.jodhpur

import java.time.Instant

import com.jesusthecat.jodhpur.test.ModelFactory
import com.jesusthecat.jodhpur.util.Base64
import org.scalatest.{FunSpec, Matchers, OptionValues}

class JWTWriterSpec
  extends FunSpec
  with Matchers
  with OptionValues {

  val secret =
    Base64.decodeToBytes("AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow").get

  val header = ModelFactory.aHeader
  val claims = ModelFactory.aClaimsSet.copy(
    exp = Some(Instant.now().getEpochSecond.toInt),
    nbf = Some(Instant.now().getEpochSecond.toInt)
  )

  describe("JWTWriter") {

    // These are hopeless tests.
    describe("JWS support") {
      it("should symmetrically write a JavascriptWebToken") {
        val token = VerifiedJwt(header, claims)
        val encoded = new JWTWriter(Some(secret)).encode(token)
        val decoded = new JWTReader(HasCrypto(secret), issuer = token.claims.iss, audience = token.claims.aud.headOption).read(encoded)
        decoded.toOption.value should be(VerifiedJwt(token.header, token.claims))
      }
    }

    describe("Plaintext") {
      it("should symmetrically write a JavascriptWebToken") {
        val token = UnverifiedJwt(header.copy(alg = NoAlgorithm), claims)
        val encoded = new JWTWriter().encode(token)
        val decoded = new JWTReader(NoCrypto, issuer = token.claims.iss, audience = token.claims.aud.headOption).read(encoded)
        decoded.toOption.value should be(UnverifiedJwt(token.header, token.claims))
      }
    }

  }


}
