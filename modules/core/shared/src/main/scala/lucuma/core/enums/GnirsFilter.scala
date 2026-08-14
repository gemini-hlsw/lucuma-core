// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

import ConvenienceOps.*

/**
 * Enumerated type for GNIRS Filter.
 *
 * @group Enumerations (Generated)
 */
enum GnirsFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  // ATTENTION: The optimal wavelength and spectroscopy range are duplicated in the DB view in the ODB. Modify it there too if it's changed here.
  val optimalWavelength: Option[Wavelength],
  val spectroscopyRange: Option[BoundedInterval[Wavelength]], // Range of the spectroscopy and acquisition filters.
  // `imagingWidth` is the imaging bandpass of the standard GNIRS imaging filters
  // (https://www.gemini.edu/instrumentation/gnirs/capability#Imaging)
  val imagingWidth: Option[BoundedInterval[Wavelength]],      // Imaging bandpass.
  val filterType: FilterType
) derives Enumerated:
  // There are two J filters (ORDER5 and J-MK) and two K filters (ORDER3 and K-MK).  The "ORDER" filters are for spectroscopy and have wider
  // wavelength coverage, while the "MK" filters are for imaging and are matched to the bandpasses of the Maunakea photometric system.
  // The "ORDER" filters are physically large and cover the length of the slit, while the MK filters are small and only cover the inner circular region.
  // Thus for spectroscopic acquisitions we use the ORDER filters while image science observations use the MK filters. The one outlier
  // is the ORDER4 filter which has approximately the H-MK bandpass so it will be used for both spectroscopic acquisitions and imaging science.
  // Note that only the "ORDER" filters are valid for spectroscopy science. The other ones with spectroscopyRange defined (H2 and PAH)
  // can still be used for acquisition.
  case CrossDispersed extends GnirsFilter("CrossDispersed", "XD",        "Cross dispersed",     none,              none,                                none,                                FilterType.BroadBand)
  case Order6 extends         GnirsFilter("Order6",         "X",         "Order 6 (X)",         1_100_000.pm.some, (1_030_000, 1_175_400).pmRange.some, (1_030_000, 1_170_000).pmRange.some, FilterType.BroadBand)     // Imaging bandpass: 1.03-1.17µm (Δλ = 140nm)
  case Order5 extends         GnirsFilter("Order5",         "J",         "Order 5 (J)",         1_250_000.pm.some, (1_175_400, 1_370_000).pmRange.some, none,                                FilterType.Spectroscopic)
  case Order4 extends         GnirsFilter("Order4",         "H",         "Order 4 (H: 1.65µm)", 1_650_000.pm.some, (1_490_000, 1_800_000).pmRange.some, (1_490_000, 1_800_000).pmRange.some, FilterType.BroadBand)     // Imaging bandpass: 1.49-1.80µm (Δλ = 310nm)
  case H2 extends             GnirsFilter("H2",             "H2",        "H2: 2.12µm",          2_120_000.pm.some, (2_105_000, 2_136_000).pmRange.some, none,                                FilterType.NarrowBand)
  case Order3 extends         GnirsFilter("Order3",         "K",         "Order 3 (K)",         2_200_000.pm.some, (1_910_000, 2_490_000).pmRange.some, none,                                FilterType.Spectroscopic)
  case Order2 extends         GnirsFilter("Order2",         "L",         "Order 2 (L)",         3_500_000.pm.some, (2_800_000, 4_200_000).pmRange.some, none,                                FilterType.BroadBand)
  case Order1 extends         GnirsFilter("Order1",         "M",         "Order 1 (M)",         4_800_000.pm.some, (4_400_000, 6_000_000).pmRange.some, none,                                FilterType.BroadBand)
  case HNd100x extends        GnirsFilter("HNd100x",        "H+ND100X",  "H + ND100X",          1_650_000.pm.some, none,                                none,                                FilterType.BroadBand)
  case H2Nd100x extends       GnirsFilter("H2Nd100x",       "H2+ND100X", "H2 + ND100X",         2_120_000.pm.some, none,                                none,                                FilterType.NarrowBand)
  case PAH extends            GnirsFilter("PAH",            "PAH",       "PAH: 3.3µm",          3_300_000.pm.some, (3_266_000, 3_321_000).pmRange.some, none,                                FilterType.NarrowBand)
  case Y extends              GnirsFilter("Y",              "Y",         "Y: 1.03µm",           1_030_000.pm.some, none,                                (970_000,   1_070_000).pmRange.some, FilterType.BroadBand)     // Imaging bandpass: 0.97-1.07µm (Δλ = 100nm)
  case J extends              GnirsFilter("J",              "J",         "J: 1.25µm",           1_250_000.pm.some, none,                                (1_170_000, 1_340_000).pmRange.some, FilterType.BroadBand)     // Imaging bandpass: 1.17-1.34µm (Δλ = 170nm)
  case K extends              GnirsFilter("K",              "K",         "K: 2.20µm",           2_200_000.pm.some, none,                                (2_030_000, 2_370_000).pmRange.some, FilterType.BroadBand)     // Imaging bandpass: 2.03-2.37µm (Δλ = 340nm)

  def centralWavelength: Wavelength =
    // The only case the filter optimalWavelength is none is for XD, where we fix to 1.65um.
    optimalWavelength.getOrElse(Wavelength.unsafeFromIntPicometers(1_650_000))

object GnirsFilter:
  val SpectroscopyScienceFilters: NonEmptyList[GnirsFilter] =
    NonEmptyList.of(Order6, Order5, Order4, Order3, Order2, Order1)

  /** Every filter that may be used for a spectroscopic acquisition, explicitly or automatically. */
  // Declaration order matters for range match. Since H2 is completely contained in Order3, it needs to come before or it will never be selected.
  val AcquisitionFilters: NonEmptyList[GnirsFilter] =
    NonEmptyList.of(Order6, Order5, Order4, H2, Order3, PAH)

  /**
   * The filters that automatic acquisition selection may produce by wavelength coverage.
   * PAH is excluded: thermal-IR science is acquired on the blue camera, where a 3.3µm filter
   * does not belong, and above `ThermalAcquisitionCutoff` `ThermalAcquisitionFilter` is used
   * instead of a coverage match.  It remains available as an explicit choice.
   */
  // Declaration order matters here too (see AcquisitionFilters).
  val AutoAcquisitionFilters: NonEmptyList[GnirsFilter] =
    NonEmptyList.of(Order6, Order5, Order4, H2, Order3)

  /** The broadband subset of the automatic filters, used when no range contains the wavelength. */
  private val BroadbandAutoAcquisitionFilters: NonEmptyList[GnirsFilter] =
    NonEmptyList.of(Order6, Order5, Order4, Order3)

  /** The blue/red boundary: science above this wavelength is a thermal-IR observation. */
  val ThermalAcquisitionCutoff: Wavelength =
    Wavelength.unsafeFromIntPicometers(2_500_000)

  /**
   * The automatic acquisition filter for thermal-IR science (L and M), where no acquisition
   * filter covers the science wavelength.  The broad L and M filters cannot be used: they
   * saturate on the sky background.
   *
   * Per the GNIRS instrument scientists, red-camera science is acquired with the blue camera
   * (`GnirsCamera.blue`) in H, or in H2 when the target is very bright — and that second case
   * is just the ordinary brightness rule, so only the Bright/Faint choice is fixed here.  The
   * legacy OCS did the same: its templates imaged the slit in H and the field in H or H2 in
   * every band.
   */
  val ThermalAcquisitionFilter: GnirsFilter =
    Order4

  def fromSpectroscopyScienceWavelength(wavelength: Wavelength): Either[String, GnirsFilter] =
    SpectroscopyScienceFilters.find(_.spectroscopyRange.exists(_.contains(wavelength)))
      .toRight(s"No Gnirs spectroscopy science filter available for wavelength: $wavelength")

  /**
   * The automatic acquisition filter for science at the given wavelength: the filter whose
   * range contains it, else `ThermalAcquisitionFilter` for thermal-IR science, else the
   * nearest broadband filter (which covers the gaps between the order filter bandpasses).
   * Total: acquisition must always be possible for an observable wavelength.
   */
  def fromAcquisitionWavelength(wavelength: Wavelength): GnirsFilter =
    AutoAcquisitionFilters
      .find(_.spectroscopyRange.exists(_.contains(wavelength)))
      .getOrElse:
        if wavelength >= ThermalAcquisitionCutoff then ThermalAcquisitionFilter
        else
          // Ties keep the earlier (shorter) filter, hence the strict comparison.
          def distance(f: GnirsFilter): Int =
            (wavelength.toPicometers.value.value - f.centralWavelength.toPicometers.value.value).abs
          BroadbandAutoAcquisitionFilters.reduceLeft: (a, b) =>
            if distance(b) < distance(a) then b else a

  private def toShortDisplayName(f: GnirsFilter): String =
    f match
      case Order6 | Order5 | Order3 | Order2 | Order1 => s"${f.shortName} (spec)"
      case Y | J | K => s"${f.shortName} (phot)"
      case _ => f.shortName

  // Andy has requested that the display names be unambiguous. However, shortName is used for parsing the 
  // phase 0 tables in explore, so we don't want to change those.
  given Display[GnirsFilter] = Display.by(toShortDisplayName, _.longName)
