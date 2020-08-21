// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math
package arb

import gsp.math.Parallax
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbParallax {

  implicit val arbParallax: Arbitrary[Parallax] =
    Arbitrary {
      arbitrary[Long].map(Parallax.μas.get)
    }

  implicit val cogParallax: Cogen[Parallax] =
    Cogen[Long].contramap(_.μas.value)
}

object ArbParallax extends ArbParallax
