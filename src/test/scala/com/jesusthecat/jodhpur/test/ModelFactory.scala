package com.jesusthecat.jodhpur.test

import java.time.Instant

object ModelFactory {

  import com.jesusthecat.jodhpur._

  def aHeader = Header(
    alg = HmacSHA256,
    typ = Some("typ"),
    cty = Some("cty")
  )

  def aClaimsSet = Claims(
    iss = Some("joe:1234"),
    sub = Some("joe"),
    aud = Set("joe", "bob", "jac"),
    exp = Some(Instant.now().getEpochSecond.toInt),
    nbf = Some(Instant.now().getEpochSecond.toInt),
    iat = Some(1500),
    jti = Some("JTI"))

}
