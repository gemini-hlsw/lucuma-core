// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.codec

import cats.implicits.*
import cats.parse.Parser
import cats.parse.Parser.*
import cats.parse.Parser0
import edu.gemini.tac.qengine.api.config.ConditionsCategory
import edu.gemini.tac.qengine.api.config.ConditionsCategory.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.util.Enumerated

/** Encode/decoder unnamed ConditionsCategory in the form "CC50 IQ20 <=SB50". */
object conditionscategory:

  given Encoder[ConditionsCategory] =
    Encoder[String].contramap(formatDiscardingName)

  given Decoder[ConditionsCategory] =
    Decoder[String].emap(parseUnnamed)

  private sealed trait Comparator
  private case object Gte extends Comparator
  private case object Lte extends Comparator

  private val comparator: Parser0[Option[Comparator]] =
    (string(">=").as(Gte) | string("<=").as(Lte)).?

  private def enumerated[A](using e: Enumerated[A]): Parser[A] =
    e.all.foldRight(fail[A].withContext(s"Expected one of ${e.all.map(e.tag).mkString(" ")}")) { (a, p) =>
      string(e.tag(a)).as(a) | p
    }

  private def specificationWithDefault[A: Enumerated](
    orElse: Unspecified[A]
  ): Parser0[Specification[A]] =
    (comparator, enumerated[A]).mapN {
      case (None,      c) => Eq(c)
      case (Some(Gte), c) => Ge(c)
      case (Some(Lte), c) => Le(c)
    }.?.map(_.getOrElse(orElse))

  private val cc: Parser0[Specification[CloudExtinction.Preset]] = specificationWithDefault(UnspecifiedCC)
  private val iq: Parser0[Specification[ImageQuality.Preset]]    = specificationWithDefault(UnspecifiedIQ)
  private val sb: Parser0[Specification[SkyBackground]]          = specificationWithDefault(UnspecifiedSB)
  private val wv: Parser0[Specification[WaterVapor]]             = specificationWithDefault(UnspecifiedWV)

  extension [A](p: Parser0[A]) def token: Parser0[A] =
    p.surroundedBy(char(' ').rep0)

  private val cat: Parser0[ConditionsCategory] =
    (cc.token, iq.token, sb.token, wv.token).mapN(ConditionsCategory(_, _, _, _, None))

  // private def formatSpec(a: Specification[_]): String =
  //   a match {
  //     case _: Unspecified[_] => ""
  //     case Eq(a) => a.toString
  //     case Le(a) => s"<=$a"
  //     case Ge(a) => s">=$a"
  //   }

  def parseUnnamed(s: String): Either[String, ConditionsCategory] =
    cat.parseAll(s).leftMap(_.toString)

  def formatDiscardingName(cat: ConditionsCategory): String =
    ???
    // List(cat.ccSpec, cat.iqSpec, cat.sbSpec, cat.wvSpec).filter {
    //   case _: Unspecified[_] => false
    //   case _                 => true
    // } .map(formatSpec).mkString(" ")

// zero
// point_one
// point_three
// point_five
// one_point_zero
// two_point_zero
// three_point_zero

  @main def test: Unit =
    println:
      (cc.token, iq.token, sb.token, wv.token).tupled.parse("zero point_one <=dark")