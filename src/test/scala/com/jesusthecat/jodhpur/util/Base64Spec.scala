package com.jesusthecat.jodhpur.util

import org.scalatest.{FunSpec, Matchers}

class Base64Spec extends FunSpec with Matchers{

  describe("Base64Spec") {

    it("should encode to URL-safe String") {
      val encoded = Base64.encode("1234567890--[]\\;',./QWERTYUIOP:LKJHGFDSAZXCVBNM<>?~")
      encoded should be("MTIzNDU2Nzg5MC0tW11cOycsLi9RV0VSVFlVSU9QOkxLSkhHRkRTQVpYQ1ZCTk08Pj9-")
    }

    it("should not include padding") {
      Base64.encode("sd") should be("c2Q")
    }

    it("should decode from URL-safe String") {
      val decoded = Base64.decode("MTIzNDU2Nzg5MC0tW11cOycsLi9RV0VSVFlVSU9QOkxLSkhHRkRTQVpYQ1ZCTk08Pj9-")
      decoded.get should be("1234567890--[]\\;',./QWERTYUIOP:LKJHGFDSAZXCVBNM<>?~")
    }

    it("should return option if there are illegal characters") {
      Base64.decode("+") should be(None)
    }
  }

}
