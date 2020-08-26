// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.sql
package enum

import doobie._, doobie.implicits._
import shapeless.record._
import shapeless.Witness
import gsp.sql.FiniteDuration

object GsaoiEnums {
  import EnumRefs._

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("GsaoiReadMode", "GSAOI Read Mode") {
        type E = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("ndr")-> Int, Symbol("readNoise")-> Int, Symbol("minimumExposureTime")-> FiniteDuration.Seconds, Symbol("overhead")-> FiniteDuration.Seconds`.T
        sql"""SELECT id, id tag, short_name, long_name, ndr, read_noise, minimum_exposure_time, overhead FROM e_gsaoi_read_mode""".query[(String, E)]
      },

      EnumDef.fromQuery("GsaoiFilter", "GSAOI Filter") {
        val  m = Witness(Symbol("MagnitudeBand"))
        val  r = Witness(Symbol("GsaoiReadMode"))
        type M = m.T
        type R = r.T
        type E = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("wavelength")-> Wavelength.Um, Symbol("readMode")-> EnumRef[R], Symbol("exposureTime5050")-> FiniteDuration.Seconds, Symbol("exposureTimeHalfWell")-> FiniteDuration.Seconds, Symbol("band")-> Option[EnumRef[M]]`.T
        val ret = sql"""SELECT id, id tag, short_name, long_name, wavelength, read_mode_id, exposure_time_50_50, exposure_time_half_well, band_id FROM e_gsaoi_filter""".query[(String, E)]
        (ret, m.value: M, r.value: R)._1 // convince scalac that we really do use M and R
      },

      EnumDef.fromQuery("GsaoiUtilityWheel", "Gsaoi Utility Wheel") {
        type E = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gsaoi_utility_wheel""".query[(String, E)]
      },

      EnumDef.fromQuery("GsaoiRoi", "Gsaoi Region of Interest") {
        type E = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gsaoi_roi""".query[(String, E)]
      },

      EnumDef.fromQuery("GsaoiOdgwSize", "Gsaoi ODGW Size") {
        type E = Record.`Symbol("tag")-> String, Symbol("pixels")-> Int`.T
        sql"""SELECT id, id tag, pixels FROM e_gsaoi_odgw_pixels""".query[(String, E)]
      }
    )

}
