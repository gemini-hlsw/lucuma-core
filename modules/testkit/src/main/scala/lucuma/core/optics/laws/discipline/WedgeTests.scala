// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws
package discipline

import cats.Eq
import cats.laws.discipline._
import lucuma.core.optics.Wedge
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait WedgeTests[A, B] extends Laws {
  val wedgeProps: WedgeProps[A, B]

  private def lawsProps(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = List(
    "normalize A"                     -> forAll((a: A) => wedgeProps.laws.normalizeA(a)),
    "normalize B"                     -> forAll((b: B) => wedgeProps.laws.normalizeB(b)),
    "reverseGet reverseGet roundtrip" -> forAll((a: A) =>
      wedgeProps.laws.normalizedGetRoundTrip(a)
    ),
    "normalized get roundtrip"        -> forAll((b: B) => wedgeProps.laws.normalizedReverseGetRoundTrip(b))
  )

  private def allProps(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = lawsProps ++ List(
    "coverage A" -> exists((a: A) => wedgeProps.demonstratesCoverageA(a)),
    "coverage B" -> exists((b: B) => wedgeProps.demonstratesCoverageB(b))
  )

  def wedge(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("Wedge", allProps*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def wedgeWith(ga: Gen[A])(implicit
    ea:             Eq[A],
    ab:             Arbitrary[B],
    eb:             Eq[B]
  ): RuleSet =
    wedge(Arbitrary(ga), ea, ab, eb)

  def wedgeLaws(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("Wedge", lawsProps*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def wedgeLawsWith(ga: Gen[A])(implicit
    ea:                 Eq[A],
    ab:                 Arbitrary[B],
    eb:                 Eq[B]
  ): RuleSet =
    wedgeLaws(Arbitrary(ga), ea, ab, eb)
}

object WedgeTests extends Laws {
  def apply[A, B](fab: Wedge[A, B]): WedgeTests[A, B] =
    new WedgeTests[A, B] {
      val wedgeProps = new WedgeProps(fab)
    }
}
