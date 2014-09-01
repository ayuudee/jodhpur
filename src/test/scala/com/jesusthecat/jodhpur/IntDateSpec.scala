package com.jesusthecat.jodhpur

import java.time.Instant

import org.scalatest.{FunSpec, Matchers}

class IntDateSpec extends FunSpec with Matchers{

  describe("IntDate") {

    val in = Instant.ofEpochSecond(1300819380)

    it("should parse to a UTC instant") {
      IntDate(in.getEpochSecond.toInt).instant should be(in)
    }

    it("should correctly assert 'isAfter'") {
      val that = Instant.ofEpochSecond(1300819370)
      val d = IntDate(in.getEpochSecond.toInt)
      d.isAfter(that, 0) should be(true)
    }

    it("should use threshold with 'isAfter'") {
      val that = Instant.ofEpochSecond(1300819390)
      val d = IntDate(in.getEpochSecond.toInt)
      d.isAfter(that, 11) should be(true)
    }
  }

}
