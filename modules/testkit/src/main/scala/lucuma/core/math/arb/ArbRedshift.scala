// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import lucuma.core.math.Redshift
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbRedshift {

  implicit val arbRedshift: Arbitrary[Redshift] =
    Arbitrary {
      for {
        rs <- Gen.chooseNum(-10.0, 10.0)
      } yield Redshift(rs)
    }

  implicit val cogRedshift: Cogen[Redshift] =
    Cogen[BigDecimal].contramap(_.z)
}

object ArbRedshift extends ArbRedshift
