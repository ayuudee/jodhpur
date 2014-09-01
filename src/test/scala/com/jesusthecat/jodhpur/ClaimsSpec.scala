package com.jesusthecat.jodhpur

import com.jesusthecat.jodhpur.test.ModelFactory
import org.scalatest.{FunSpec, Matchers, OptionValues}

class ClaimsSpec
  extends FunSpec
  with Matchers
  with OptionValues {

  import argonaut.Argonaut._
  import argonaut._

  describe("Claims") {

    describe("JSON codec") {

      it("should symmetrically de/serialise a full Claims set") {
        val claims = ModelFactory.aClaimsSet

        val json = claims.asJson.nospaces
        Parse.decodeOption[Claims](json).value should be(claims)
      }
    }

  }
}

