// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package util
package arb

import org.scalacheck.*

trait ArbEnumerated {

  given arbEnumerated[A](using en: Enumerated[A]): Arbitrary[A] =
    Arbitrary(Gen.oneOf(en.all))

  given cogEnumerated[A](using en: Enumerated[A]): Cogen[A] =
    Cogen[String].contramap(en.tag)

}

object ArbEnumerated extends ArbEnumerated
