// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math
package arb

import gsp.math.Parallax
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Cogen

trait ArbParallax {

  implicit val arbParallax: Arbitrary[Parallax] =
    Arbitrary {
      Gen
        .choose(Parallax.MinValue.μas.value, Parallax.MaxValue.μas.value)
        .map(Parallax.fromMicroarcseconds(_))
    }

  implicit val cogParallax: Cogen[Parallax] =
    Cogen[Long].contramap(_.μas.value)
}

object ArbParallax extends ArbParallax
