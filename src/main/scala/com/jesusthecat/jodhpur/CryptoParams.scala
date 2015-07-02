package com.jesusthecat.jodhpur

trait CryptoParams {
  val algorithm: SigningAlgorithm
}

case object NoCrypto extends CryptoParams {
  override val algorithm = NoAlgorithm
}

case class HasCrypto(secret: Array[Byte],
  override val algorithm: SigningAlgorithm = HmacSHA256)
    extends CryptoParams
