package com.jesusthecat.jodhpur

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

sealed trait SigningAlgorithm {

  def key: String

  def sign(secret: Array[Byte], bx: Array[Byte]): Array[Byte]
}

object SigningAlgorithm {

  def algorithms = Set(NoAlgorithm, HmacSHA256)

  def parse(v: StringOrUri): Option[SigningAlgorithm] = {
    val norm = v.asString.toLowerCase
    algorithms.find(_.key == norm)
  }
}

// ----------------------------------------------------------------------------

// TODO (AD): Improper semantics.
object NoAlgorithm extends SigningAlgorithm {

  override def key: String = "none"

  override def sign(secret: Array[Byte], bx: Array[Byte]): Array[Byte] = bx

}

object HmacSHA256 extends SigningAlgorithm {

  override def key: String = "hs256"

  override def sign(secret: Array[Byte], bx: Array[Byte]): Array[Byte] = {
    val hmac = Mac.getInstance("HmacSHA256")
    hmac.init(new SecretKeySpec(secret, hmac.getAlgorithm))
    hmac.doFinal(bx)
  }

}
