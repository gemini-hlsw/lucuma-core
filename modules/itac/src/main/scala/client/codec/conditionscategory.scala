// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.codec

import cats.implicits._
import edu.gemini.tac.qengine.api.config.ConditionsCategory
import edu.gemini.tac.qengine.api.config.ConditionsCategory._
import io.circe.{ Decoder, Encoder }

/** Encode/decoder unnamed ConditionsCategory in the form "CC50 IQ20 <=SB50". */
object conditionscategory:

  given Encoder[ConditionsCategory] =
    Encoder[String].contramap(formatDiscardingName)

  given Decoder[ConditionsCategory] =
    Decoder[String].emap(parseUnnamed)

  private sealed trait Comparator
  private case object Gte extends Comparator
  private case object Lte extends Comparator

  // private val comparator: Parser[Option[Comparator]] =
  //   opt(string(">=").as(Gte) | string("<=").as(Lte))

  // private def fromToString[A](as: List[A]): Parser[A] =
  //   as.foldRight(err[A](s"Expected one of ${as.mkString(" ")}")) { (cc, p) =>
  //     string(cc.toString).as(cc) | p
  //   }

  // private def spec[A <: ObservingCondition: Ordering](cond: Parser[A]): Parser[Specification[A]] =
  //   (comparator, cond).mapN {
  //     case (None,      c) => Eq(c)
  //     case (Some(Gte), c) => Ge(c)
  //     case (Some(Lte), c) => Le(c)
  //   }

  // private val cc: Parser[Specification[CloudCover]]    =
  //   opt(spec[CloudCover](fromToString(CloudCover.values))).map(_.getOrElse(UnspecifiedCC))

  // private val iq: Parser[Specification[ImageQuality]]  =
  //   opt(spec[ImageQuality](fromToString(ImageQuality.values))).map(_.getOrElse(UnspecifiedIQ))

  // private val sb: Parser[Specification[SkyBackground]] =
  //   opt(spec[SkyBackground](fromToString(SkyBackground.values))).map(_.getOrElse(UnspecifiedSB))

  // private val wv: Parser[Specification[WaterVapor]]    =
  //   opt(spec[WaterVapor](fromToString(WaterVapor.values))).map(_.getOrElse(UnspecifiedWV))

  // private val cat: Parser[ConditionsCategory] =
  //   (cc.token, iq.token, sb.token, wv.token).mapN(ConditionsCategory(_, _, _, _, None))

  // private def formatSpec(a: Specification[_]): String =
  //   a match {
  //     case _: Unspecified[_] => ""
  //     case Eq(a) => a.toString
  //     case Le(a) => s"<=$a"
  //     case Ge(a) => s">=$a"
  //   }

  def parseUnnamed(s: String): Either[String, ConditionsCategory] =
    ???
    // (cat <~ endOfInput).parseOnly(s).either

  def formatDiscardingName(cat: ConditionsCategory): String =
    ???
    // List(cat.ccSpec, cat.iqSpec, cat.sbSpec, cat.wvSpec).filter {
    //   case _: Unspecified[_] => false
    //   case _                 => true
    // } .map(formatSpec).mkString(" ")



