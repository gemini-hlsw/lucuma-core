// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.data.NonEmptySet
import coulomb.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.units.*
import lucuma.core.math.units.refined.*
import lucuma.core.util.Enumerated
import lucuma.refined.*

/**
 * Enumerated type for GMOS detector.
 * @group Enumerations (Generated)
 * @see https://www.gemini.edu/instrumentation/gmos/components#Detector
 */
enum GmosSouthDetector(
  val tag: String,
  val shortName: String,
  val longName: String,
  val pixelSize: Angle,
  val shuffleOffset: Quantity[PosInt, Pixels],
  val xSize: Quantity[PosInt, Pixels],
  val ySize: Quantity[PosInt, Pixels],
  val gapSize: Quantity[PosInt, Pixels],
  val maxRois: PosInt,
  val ampCounts: NonEmptySet[GmosAmpCount]
) derives Enumerated:

  case E2V extends GmosSouthDetector(
    "E2V",
    "E2V",
    "E2V",
    Angle.fromMicroarcseconds(73000),
    1536.withRefinedUnit[Positive, Pixels],
    6144.withRefinedUnit[Positive, Pixels],
    4608.withRefinedUnit[Positive, Pixels],
    37.withRefinedUnit[Positive, Pixels],
    4.refined,
    NonEmptySet.of(GmosAmpCount.Three, GmosAmpCount.Six)
  )

  case Hamamatsu extends GmosSouthDetector(
    "HAMAMATSU",
    "Hamamatsu",
    "Hamamatsu",
    Angle.fromMicroarcseconds(80000),
    1392.withRefinedUnit[Positive, Pixels],
    6255.withRefinedUnit[Positive, Pixels],
    4176.withRefinedUnit[Positive, Pixels],
    61.withRefinedUnit[Positive, Pixels],
    5.refined,
    NonEmptySet.of(GmosAmpCount.Six, GmosAmpCount.Twelve)
  )
