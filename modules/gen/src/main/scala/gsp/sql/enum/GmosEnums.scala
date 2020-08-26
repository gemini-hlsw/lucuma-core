// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.sql
package enum

import doobie._, doobie.implicits._
import shapeless.record._

object GmosEnums {
  import Angle.Arcseconds

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("GmosAdc", "GMOS ADC") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_adc""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosAmpCount", "GMOS amp count") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_count""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosAmpGain", "GMOS amp gain") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_gain""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosAmpReadMode", "GMOS amp read mode") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_read_mode""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosCustomSlitWidth", "GMOS custom slit width") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("width")-> Arcseconds`.T
        sql"""SELECT id, id tag, short_name, long_name, width FROM e_gmos_custom_slit_width""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosDetector", "GMOS detector") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("northPixelSize")-> Arcseconds, Symbol("southPixelSize")-> Arcseconds, Symbol("shuffleOffset")-> Int, Symbol("xSize")-> Int, Symbol("ySize")-> Int, Symbol("maxRois")-> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, north_pixel_size, south_pixel_size, shuffle_offset, x_size, y_size, max_rois FROM e_gmos_detector""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosDisperserOrder", "GMOS disperser order") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("count")-> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_disperser_order""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosDtax", "GMOS detector translation X offset") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("dtax")-> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, dtax FROM e_gmos_dtax""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosEOffsetting", "GMOS Electric Offsetting") {
        type R = Record.`Symbol("tag")-> String, Symbol("description")-> String, Symbol("toBoolean")-> Boolean`.T
        sql"select id, id tag, description, to_boolean FROM e_gmos_e_offsetting".query[(String, R)]
      },

      EnumDef.fromQuery("GmosNorthDisperser", "GMOS North dispersers") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("rulingDensity")-> Int, Symbol("obsolete")-> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, ruling_density, obsolete FROM e_gmos_north_disperser""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosNorthFilter", "GMOS North filters") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("wavelength")-> Wavelength.Um, Symbol("obsolete")-> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, wavelength, obsolete FROM e_gmos_north_filter""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosNorthFpu", "GMOS North focal plane units") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("slitWidth")-> Option[Arcseconds], Symbol("xOffset")-> Arcseconds`.T
        sql"""SELECT id, id tag, short_name, long_name, slit_width, x_offset FROM e_gmos_north_fpu""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosNorthStageMode", "GMOS North stage modes") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("obsolete")-> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gmos_north_stage_mode""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosRoi", "GMOS ROI (region of interest)") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("obsolete")-> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gmos_roi""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosSouthDisperser", "GMOS South dispersers") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("rulingDensity")-> Int, Symbol("obsolete")-> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, ruling_density, obsolete FROM e_gmos_south_disperser""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosSouthFilter", "GMOS South filters") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("wavelength")-> Wavelength.Um, Symbol("obsolete")-> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, wavelength, obsolete FROM e_gmos_south_filter""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosSouthFpu", "GMOS South focal plane units") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("slitWidth")-> Option[Arcseconds], Symbol("xOffset")-> Arcseconds`.T
        sql"""SELECT id, id tag, short_name, long_name, slit_width, x_offset FROM e_gmos_south_fpu""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosSouthStageMode", "GMOS South stage mode") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("obsolete")-> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gmos_south_stage_mode""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosXBinning", "GMOS X-binning") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("count")-> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_binning""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosYBinning", "GMOS Y-binning") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("count")-> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_binning""".query[(String, R)]
      }

    )

}
