// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.math.syntax.units.*
import lucuma.core.math.units.PixelScale
import lucuma.core.math.units.PlateScale
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 Lyot wheel.
 *
 * f/16:  plate scale = 1.61 arcsec/mm;  pixel scale=0.18 arcsec/pixel
 * f/32:  plate scale = 0.805 arcsec/mm; pixel scale =0.09 arcsec/pixel
 * If the Lyot wheel is set to HartmannA or HartmannB, the
 * FOV should just be a point at the base position (this is not a
 * scientifically useful option, but is used for focusing)
 * @group Enumerations (Generated)
 */
enum F2LyotWheel(
  val tag: String,
  val shortName: String,
  val longName: String,
  val plateScale: PlateScale, // arcsec/mm
  val pixelScale: PixelScale  // arcsec/pixel
) derives Enumerated:

  case F16       extends F2LyotWheel("F16",       "f/16",            "f/16 (Open)",             1.61.plateScale,  0.18.pixelScale)
  case GemsUnder extends F2LyotWheel("GemsUnder", "GeMS Under",      "f/33 (GeMS under-sized)", 0.784.plateScale, 0.09.pixelScale)
  case GemsOver  extends F2LyotWheel("GemsOver",  "GeMS Over",       "f/33 (GeMS over-sized)",  0.784.plateScale, 0.09.pixelScale)
  case HartmannA extends F2LyotWheel("HartmannA", "Hartmann A (H1)", "Hartmann A (H1)",         0.0.plateScale,   0.0.pixelScale)
  case HartmannB extends F2LyotWheel("HartmannB", "Hartmann B (H2)", "Hartmann B (H2)",         0.0.plateScale,   0.0.pixelScale)
