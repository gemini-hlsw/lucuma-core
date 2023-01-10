// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.math.Parallax
import lucuma.core.math.units._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbParallax {

  implicit val arbParallax: Arbitrary[Parallax] =
    Arbitrary {
      arbitrary[Parallax.LongParallaxμas].map(μas => Parallax(μas.withUnit[MicroArcSecond]))
    }

  implicit val cogParallax: Cogen[Parallax] =
    Cogen[Long].contramap(_.μas.value.value)
}

object ArbParallax extends ArbParallax
