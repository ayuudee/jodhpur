package com.jesusthecat

import java.net.URI
import java.time.Instant

import argonaut.Argonaut._
import argonaut.{DecodeResult, CodecJson}
import com.jesusthecat.jodhpur.util.Base64

import scalaz.NonEmptyList

package object jodhpur {

  /**
   * A JSON string value, with the additional requirement that while
   * arbitrary string values MAY be used, any value containing a ":"
   * character MUST be a URI [RFC3986].  StringOrURI values are
   * compared as case-sensitive strings with no transformations or
   * canonicalizations applied.
   * @see http://tools.ietf.org/html/draft-ietf-oauth-json-web-token-19#section-2
   */
  type StringOrUri = Either[String, URI]

  // TODO (AD): This is overly restrictive. It's right for the type, but wrong
  // in practice.
  implicit def stringToStringOrUri(s: String): StringOrUri = {
    if (s.contains(":")) Right(URI.create(s))
    else Left(s)
  }

  implicit class PimpedStringOrUri(val si: StringOrUri) extends AnyVal {

    def asString = si match {
      case Left(s) => s.toString
      case Right(u) => u.toString
    }
  }

  /**
   * A JSON numeric value representing the number of seconds from 1970-
   * 01-01T0:0:0Z UTC until the specified UTC date/time.  See RFC 3339
   * [RFC3339] for details regarding date/times in general and UTC in
   * particular.
   * @see http://tools.ietf.org/html/draft-ietf-oauth-json-web-token-19#section-2
   */
  case class IntDate(value: Int) extends AnyVal {
    def instant = Instant.ofEpochSecond(value)

    def isAfter(that: Instant, thresholdSec: Int) =
      instant.isAfter(that.minusSeconds(thresholdSec))
  }

  implicit def intToIntDate(value: Int) = IntDate(value)

  case class Base64Str(value: String) extends AnyVal {

    def asStr = Base64.decode(value).get

    def asBytes = Base64.decodeToBytes(value).get
  }

  // --------------------------------------------------------------------------
  // Codecs.

  implicit val StringOrUriCodecJson: CodecJson[StringOrUri] = {
    CodecJson(
      (s: StringOrUri) => jString(s.asString),
      c => c.as[String].map(stringToStringOrUri)
    )
  }

  implicit val IntDateCodecJson: CodecJson[IntDate] = {
    CodecJson(
      (i: IntDate) => jNumber(i.value),
      c => c.as[Int].map(intToIntDate)
    )
  }

  implicit val SigningAlgorithmCodecJson: CodecJson[SigningAlgorithm] = {
    CodecJson(
      (a: SigningAlgorithm) => jString(a.key),
      c =>
        // TODO (AD): Come back and figure this out.
        c.as[StringOrUri]
          .flatMap(v => SigningAlgorithm.parse(v)
          .map(DecodeResult.ok)
          .getOrElse(DecodeResult.fail("No matching algorithm", c.history)))
    )
  }

}
