// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.units.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbParallax {

  given Arbitrary[Parallax] =
    Arbitrary {
      arbitrary[Parallax.LongParallaxμas].map(μas => Parallax(μas.withUnit[MicroArcSecond]))
    }

  given Cogen[Parallax] =
    Cogen[Long].contramap(_.μas.value.value)
}

object ArbParallax extends ArbParallax
