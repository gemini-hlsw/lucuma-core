// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import coulomb.*
import eu.timepit.refined.scalacheck.numeric.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbParallax:

  given Arbitrary[Parallax] =
    Arbitrary {
      arbitrary[Parallax.LongParallaxμas].map(Parallax.apply)
    }

  given Cogen[Parallax] =
    Cogen[Long].contramap(_.μas.value.value)

object ArbParallax extends ArbParallax
