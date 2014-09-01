package com.jesusthecat.jodhpur

import com.jesusthecat.jodhpur.util.Base64

/** Write [[JavascriptWebToken]] to JSON. */
class JWTWriter(secret: Option[Array[Byte]] = None) {

  import argonaut.Argonaut._

  /** Encode the provided JavascriptWebToken and sign the result (if appropriate) to
    * produce a Base64 encoded JWT. */
  def encode(jwt: JavascriptWebToken): String = {
    require(secret.isDefined || jwt.header.alg == NoAlgorithm)
    val h = Base64.encode(jwt.header.asJson.nospaces)
    val c = Base64.encode(jwt.claims.asJson.nospaces)
    val payload = s"$h.$c"
    secret.fold(payload) {
      sx =>
        val sig = Base64.encode(jwt.header.alg.sign(sx, payload.getBytes("UTF8")))
        s"$payload.$sig"
    }
  }

}
