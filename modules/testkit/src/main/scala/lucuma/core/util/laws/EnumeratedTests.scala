package lucuma.core.util.laws

import io.circe.testing.CodecLaws
import cats.kernel.laws.OrderLaws
import io.circe.{ Encoder, Decoder }
import cats.kernel.Order
import lucuma.core.util.Enumerated
import io.circe.testing.CodecTests
import cats.kernel.laws.discipline.OrderTests
import org.scalacheck.Prop
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import cats.kernel.Eq
import io.circe.Json
import org.scalacheck.Cogen
import cats.laws.discipline._

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
    arbitraryJson: Arbitrary[Json],
    shrinkJson: Shrink[Json],
    cog: Cogen[A],
  ): RuleSet =
    new RuleSet {
      val name: String = "enumerated"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(unserializableCodec, order)
      val props: Seq[(String, Prop)] = Seq(
        "tag round-trip" -> Prop.forAll(laws.tagRoundTrip _),
      )
    }

}

object EnumeratedTests {
  def apply[A: Enumerated]: EnumeratedTests[A] =
    new EnumeratedTests[A] {
      val laws: EnumeratedLaws[A] = EnumeratedLaws[A]
    }
}