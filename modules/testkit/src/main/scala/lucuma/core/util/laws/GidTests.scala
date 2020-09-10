// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.laws

import cats.kernel._
import cats.kernel.laws.BoundedEnumerableLaws
import cats.kernel.laws.discipline.{ OrderTests, BoundedEnumerableTests }
import cats.kernel.laws.OrderLaws
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats._
import eu.timepit.refined.char.Letter
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.numeric.PosLong
import io.circe.{ Encoder, Decoder }
import io.circe.testing.{ CodecLaws, CodecTests }
import io.circe.testing.instances.arbitraryJson
import lucuma.core.util.Gid
import monocle.law.discipline.{ IsoTests, PrismTests }
import org.scalacheck._

trait GidLaws[A] extends CodecLaws[A] with OrderLaws[A] with BoundedEnumerableLaws[A] {
  def gidA: Gid[A]
}

object GidLaws {
  def apply[A](implicit ev: Gid[A]): GidLaws[A] = new GidLaws[A] {
    val gidA = ev
    val decode: Decoder[A] = ev
    val encode: Encoder[A] = ev
    val E: Order[A] = ev
    override def B: LowerBounded[A] with UpperBounded[A] = ev
    implicit def N: PartialNext[A] = ev
    implicit def P: PartialPrevious[A] = ev
  }
}

trait GidTests[A] extends CodecTests[A] with OrderTests[A] with BoundedEnumerableTests[A] {

  def laws: GidLaws[A]

  def gid(
    implicit
    arbitraryA: Arbitrary[A],
    shrinkA: Shrink[A],
    eqA: Eq[A],
    cog: Cogen[A],
  ): RuleSet =
    new RuleSet {
      val name: String = "Gid"
      val bases: Seq[(String, RuleSet)] = Seq(
        "fromString" -> new SimpleRuleSet("Prism", PrismTests(laws.gidA.fromString).props: _*),
        "isoPosLong" -> new SimpleRuleSet("Iso", IsoTests(laws.gidA.isoPosLong).props: _*)
      )
      val parents: Seq[RuleSet] = Seq(
        unserializableCodec,
        order,
        boundedEnumerable,
      )
      val props: Seq[(String, Prop)] = Seq(
        "fromString (tag)" -> Prop.forAll { (c: Char Refined Letter, n: PosLong) =>
          val s = s"${c.value}-${n.value.toHexString}"
          laws.gidA.fromString.getOption(s).isDefined == (c.value == laws.gidA.tag.value)
        }
      )
    }

}

object GidTests {
  def apply[A: Gid]: GidTests[A] =
    new GidTests[A] {
      val laws: GidLaws[A] = GidLaws[A]
    }
}