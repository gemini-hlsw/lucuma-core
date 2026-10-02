// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.data.NonEmptyList
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

import ConvenienceOps.*

/**
 * Enumerated type for SCORPIO filters.
 *
 * These are not interchangeable filters: SCORPIO splits light with fixed dichroics into eight
 * channels, each with its own detector. We call them filters to match the other instruments and
 * the internal component names (g_G1301, etc).
 *
 * Each can be used in both imaging and long-slit spectroscopy.
 *
 * `wavelength` and `spectroscopyWidth` come from the long-slit spectral coverage table
 * (Table 4-4 of the ConOps document, 50% cutoffs). `imagingWidth` comes from the imaging
 * channel table, where the edges are set by dichroics and filter cutoffs and differ from
 * the spectroscopic ones.
 *
 * @group Enumerations
 */
enum ScorpioFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val imagingWidth: BoundedInterval[Wavelength],
  val spectroscopyWidth: BoundedInterval[Wavelength],
  val filterType: FilterType,
  val channel: ScorpioChannel
) derives Enumerated:
  case G  extends ScorpioFilter("G",  "g",  "g_G1301",    469_000.pm,   (400_000,   552_000).pmRange,   (385_000,   552_000).pmRange, FilterType.BroadBand, ScorpioChannel.Vis)
  case R  extends ScorpioFilter("R",  "r",  "r_G1302",    622_000.pm,   (552_000,   691_000).pmRange,   (552_000,   691_000).pmRange, FilterType.BroadBand, ScorpioChannel.Vis)
  case I  extends ScorpioFilter("I",  "i",  "i_G1303",    755_000.pm,   (691_000,   818_000).pmRange,   (691_000,   818_000).pmRange, FilterType.BroadBand, ScorpioChannel.Vis)
  case Z  extends ScorpioFilter("Z",  "z",  "z_G1304",    889_000.pm,   (818_000,   922_000).pmRange,   (818_000,   960_000).pmRange, FilterType.BroadBand, ScorpioChannel.Vis)
  case Y  extends ScorpioFilter("Y",  "Y",  "Y_G1305",  1_040_000.pm,   (960_000, 1_120_000).pmRange,   (960_000, 1_120_000).pmRange, FilterType.BroadBand, ScorpioChannel.Ir)
  case J  extends ScorpioFilter("J",  "J",  "J_G1306",  1_250_000.pm, (1_150_000, 1_350_000).pmRange, (1_150_000, 1_350_000).pmRange, FilterType.BroadBand, ScorpioChannel.Ir)
  case H  extends ScorpioFilter("H",  "H",  "H_G1307",  1_630_000.pm, (1_500_000, 1_760_000).pmRange, (1_500_000, 1_760_000).pmRange, FilterType.BroadBand, ScorpioChannel.Ir)
  case Ks extends ScorpioFilter("Ks", "Ks", "Ks_G1308", 2_175_000.pm, (2_000_000, 2_280_000).pmRange, (2_000_000, 2_350_000).pmRange, FilterType.BroadBand, ScorpioChannel.Ir)

object ScorpioFilter:

  val visible: NonEmptyList[ScorpioFilter] =
    NonEmptyList.of(G, R, I, Z)

  val infrared: NonEmptyList[ScorpioFilter] =
    NonEmptyList.of(Y, J, H, Ks)
