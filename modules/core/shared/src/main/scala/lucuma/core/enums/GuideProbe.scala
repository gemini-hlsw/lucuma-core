// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for Guiding probes, only GMOS and PWFS1/2 so far
 * @group Enumerations
 */
sealed abstract class GuideProbe(val tag: String)
    extends Product
    with Serializable

object GuideProbe {

  /** @group Constructors */
  case object GmosOIWFS extends GuideProbe("GMOS OIWFS")

  /** @group Constructors */
  case object PWFS1 extends GuideProbe("PWFS1")

  /** @group Constructors */
  case object PWFS2 extends GuideProbe("PWFS2")

  /** @group Typeclass Instances */
  implicit val GuideProbeEnumerated: Enumerated[GuideProbe] =
    Enumerated.of[GuideProbe](GmosOIWFS, PWFS1, PWFS2)

}
