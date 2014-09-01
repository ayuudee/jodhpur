package com.jesusthecat.jodhpur

import java.net.URI

import org.scalatest.{FunSpec, Matchers}

class StringOrUriSpec extends FunSpec with Matchers {

  describe("StringOrUri") {

    it("should decode to Right if the string contains a colon") {
      stringToStringOrUri("foo:1234") should be(Right(URI.create("foo:1234")))
    }

    it("should decode to Left if the string does not contain a colon") {
      stringToStringOrUri("foo") should be(Left("foo"))
    }
  }

}
