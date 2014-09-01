package com.jesusthecat.jodhpur.util

/** Internal utility to encode Base64 using Sun's sneaky internal
  * encoder/decoder. Appropriate substitutions are made to support
  * 'url safe' encodings.
  *
  * The spec wants:
  * Base64 encoding using the URL- and filename-safe character set
  * defined in Section 5 of RFC 4648 [RFC4648], with all trailing '='
  * characters omitted (as permitted by Section 3.2) and without the
  * inclusion of any line breaks, white space, or other additional
  * characters. */
private[jodhpur] object Base64 {

  def encode(str: String): String = encode(str.getBytes)

  def encode(bx: Array[Byte]): String = {
    java.util.Base64.getUrlEncoder
      .withoutPadding()
      .encodeToString(bx)
  }

  def decode(str: String): Option[String] = decodeToBytes(str) map(new String(_))

  def decodeToBytes(str: String): Option[Array[Byte]] = try {
    Some(java.util.Base64.getUrlDecoder.decode(str))
  } catch {
    case ex: Exception => None
  }

}
