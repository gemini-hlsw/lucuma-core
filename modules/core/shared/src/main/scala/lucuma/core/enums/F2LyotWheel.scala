// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
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
  val plateScale: Double, // arcsec/mm
  val pixelScale: Double, // arcsec/pixel
  val obsolete: Boolean
) derives Enumerated:

  case F16       extends F2LyotWheel("F16",       "f/16",            "f/16 (Open)",               1.61,  0.18, false)
  case F32High   extends F2LyotWheel("F32High",   "f/32 High",       "f/32 MCAO high background", 0.805, 0.09, true)
  case F32Low    extends F2LyotWheel("F32Low",    "f/32 Low",        "f/32 MCAO low background",  0.805, 0.09, true)
  case F33Gems   extends F2LyotWheel("F33Gems",   "f/33 GeMS",       "f/33 (GeMS)",               0.784, 0.09, true)
  case GemsUnder extends F2LyotWheel("GemsUnder", "GeMS Under",      "f/33 (GeMS under-sized)",   0.784, 0.09, false)
  case GemsOver  extends F2LyotWheel("GemsOver",  "GeMS Over",       "f/33 (GeMS over-sized)",    0.784, 0.09, false)
  case HartmannA extends F2LyotWheel("HartmannA", "Hartmann A (H1)", "Hartmann A (H1)",           0.0,   0.0,  false)
  case HartmannB extends F2LyotWheel("HartmannB", "Hartmann B (H2)", "Hartmann B (H2)",           0.0,   0.0,  false)
