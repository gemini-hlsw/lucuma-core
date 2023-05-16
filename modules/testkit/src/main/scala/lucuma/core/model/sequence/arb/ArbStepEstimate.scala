// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import cats.syntax.option.*
import lucuma.core.data.Zipper
import lucuma.core.data.arb.ArbZipper
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbStepEstimate {

  import ArbConfigChangeEstimate.given
  import ArbDetectorEstimate.given
  import ArbZipper.arbZipper

  private given [A: Arbitrary]: Arbitrary[Zipper[A]] =
    Arbitrary {arbitrary(arbZipper[A](3))}

  private given [A: Cogen]: Cogen[Zipper[A]] =
    ArbZipper.given_Cogen_Zipper

  given Arbitrary[StepEstimate] =
    Arbitrary {
      for {
        c <- arbitrary[Option[Zipper[ConfigChangeEstimate]]]
        d <- arbitrary[Option[Zipper[DetectorEstimate]]]
      } yield StepEstimate(c, d)
    }

  given Cogen[StepEstimate] =
    Cogen[(
      Option[Zipper[ConfigChangeEstimate]],
      Option[Zipper[DetectorEstimate]]
    )].contramap { a =>
      (a.configChange, a.detector)
    }

}

object ArbStepEstimate extends ArbStepEstimate
