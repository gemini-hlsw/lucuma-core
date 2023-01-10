// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.laws

import cats.kernel._
import cats.kernel.laws.OrderLaws
import cats.kernel.laws.discipline.OrderTests
import eu.timepit.refined.api.Refined
import eu.timepit.refined.char.Letter
import eu.timepit.refined.scalacheck.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.testing.CodecLaws
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.core.util.Uid
import monocle.law.discipline.IsoTests
import monocle.law.discipline.PrismTests
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck._

import java.util.UUID
import scala.util.matching.Regex

trait UidLaws[A] extends CodecLaws[A] with OrderLaws[A] {
  def uidA: Uid[A]
}

object UidLaws {
  def apply[A](implicit ev: Uid[A]): UidLaws[A] = new UidLaws[A] {
    val uidA               = ev
    val decode: Decoder[A] = ev
    val encode: Encoder[A] = ev
    val E: Order[A]        = ev
  }
}

trait UidTests[A] extends CodecTests[A] with OrderTests[A] {

  def laws: UidLaws[A]

  private final lazy val regex: Regex = laws.uidA.regexPattern.r

  private final val genTag: Gen[Char Refined Letter] = arbitrary[Char Refined Letter]

  private final def genUidString(c: Char Refined Letter): Gen[String] =
    arbitrary[UUID].map(n => s"${c.value}-$n")

  def uid(implicit
    arbitraryA: Arbitrary[A],
    eqA:        Eq[A],
    cog:        Cogen[A]
  ): RuleSet =
    new RuleSet {
      val name: String                  = "Uid"
      val bases: Seq[(String, RuleSet)] = Seq(
        "fromString" -> new SimpleRuleSet("Prism", PrismTests(laws.uidA.fromString).props: _*),
        "isoUuid"    -> new SimpleRuleSet("Iso", IsoTests(laws.uidA.isoUuid).props: _*)
      )
      val parents: Seq[RuleSet]         = Seq(
        unserializableCodec,
        order
      )
      val props: Seq[(String, Prop)]    = Seq(
        "fromString (tag)" -> Prop.forAll(
          genTag.flatMap(c => genUidString(c).map(s => (c, s)))
        ) { case (c, s) =>
          laws.uidA.fromString.getOption(s).isDefined == (c.value == laws.uidA.tag.value)
        },
        "regexPattern"     -> Prop.forAll(
          Gen.frequency(
            9 -> genTag.flatMap(genUidString),
            1 -> arbitrary[String]
          )
        ) { s =>
          laws.uidA.fromString.getOption(s).isDefined == regex.matches(s)
        }
      )
    }

}

object UidTests {
  def apply[A: Uid]: UidTests[A] =
    new UidTests[A] {
      val laws: UidLaws[A] = UidLaws[A]
    }
}
