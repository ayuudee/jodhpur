package com.jesusthecat.jodhpur

import com.jesusthecat.jodhpur.test.ModelFactory
import org.scalatest.{OptionValues, FunSpec, Matchers}

class HeaderSpec
  extends FunSpec
  with Matchers
  with OptionValues {

  import argonaut.Argonaut._
  import argonaut._

  describe("Header") {

    describe("JSON codec") {

      it("should symmetrically de/serialise a Header") {
        val header = ModelFactory.aHeader
        val json = header.asJson.nospaces
        Parse.decodeOption[Header](json).value should be(header)
      }
    }

  }
}
