// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.laws

import cats.*
import cats.kernel.laws.OrderLaws
import cats.kernel.laws.discipline.OrderTests
import cats.laws.discipline.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.testing.CodecLaws
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.core.util.Enumerated
import org.scalacheck.*

trait EnumeratedLaws[A] extends CodecLaws[A] with OrderLaws[A] {

  def enumeratedA: Enumerated[A]

  def tagRoundTrip(a: A): IsEq[Option[A]] =
    enumeratedA.fromTag(enumeratedA.tag(a)) <-> Some(a)

}

object EnumeratedLaws {
  def apply[A](implicit ev: Enumerated[A]): EnumeratedLaws[A] = new EnumeratedLaws[A] {
    val enumeratedA = ev
    val decode: Decoder[A] = ev
    val encode: Encoder[A] = ev
    val E: Order[A] = ev
  }
}

trait EnumeratedTests[A] extends CodecTests[A] with OrderTests[A] {

  def laws: EnumeratedLaws[A]

  def enumerated(
    implicit
    arbitraryA: Arbitrary[A],
    shrinkA: Shrink[A],
    eqA: Eq[A],
    cog: Cogen[A],
  ): RuleSet =
    new RuleSet {
      val name: String = "enumerated"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(unserializableCodec, order)
      val props: Seq[(String, Prop)] = Seq(
        "tag round-trip" -> Prop.forAll(laws.tagRoundTrip),
      )
    }

}

object EnumeratedTests {
  def apply[A: Enumerated]: EnumeratedTests[A] =
    new EnumeratedTests[A] {
      val laws: EnumeratedLaws[A] = EnumeratedLaws[A]
    }
}