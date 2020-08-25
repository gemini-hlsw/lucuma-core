// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.sql
package enum

import doobie._
import doobie.implicits._
import java.time.ZoneId

import gsp.sql.EnumDef
import shapeless.record._
import shapeless.Witness

object MiscEnums {
  import Angle._
  import EnumRefs._

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("PortDisposition", "ISS Port Disposition") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String`.T
        sql"SELECT id, id tag, short_name FROM e_port_disposition".query[(String, R)]
      },

      EnumDef.fromQuery("MosPreImaging", "MOS pre-imaging category") {
        type R = Record.`Symbol("tag")-> String, Symbol("description")-> String, Symbol("toBoolean")-> Boolean`.T
        sql"SELECT id, id tag, description, to_boolean FROM e_mos_preimaging".query[(String, R)]
      },

      EnumDef.fromQuery("StepType", "step types") {
        type R = Record.`Symbol("tag")-> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'step_type'
         """.query[(String, R)]
      },

      EnumDef.fromQuery("EventType", "observe [[gsp.Event Event]]) types") {
        type R = Record.`Symbol("tag")-> String`.T
        sql"""
          SELECT enumlabel a, enumlabel b
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'evt_type'
        """.query[(String, R)]
      },

      EnumDef.fromQuery("Instrument", "instruments") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("obsolete")-> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_instrument".query[(String, R)]
      },

      EnumDef.fromQuery("ProgramType", "program types (see [[gsp.ProgramId ProgramId]])") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("obsolete")-> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_program_type".query[(String, R)]
      },

      EnumDef.fromQuery("Site", "Gemini observing sites") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("mountain")-> String, Symbol("latitude")-> Degrees, Symbol("longitude")-> Degrees, Symbol("altitude")-> Int, Symbol("timezone")-> ZoneId`.T
        sql"""SELECT id, id tag, short_name, long_name, mountain, latitude, longitude, altitude, timezone FROM e_site""".query[(String, R)]
      },

      EnumDef.fromQuery("ProgramRole", "user roles with respect to a given program") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String`.T
        sql"SELECT id, id tag, short_name, long_name FROM e_program_role".query[(String, R)]
      },

      EnumDef.fromQuery("Half", "semester half") {
        type R = Record.`Symbol("tag")-> String, Symbol("toInt")-> Int`.T
        sql"""
          SELECT enumlabel x, enumlabel y, enumsortorder - 1
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'half'
         """.query[(String, R)]
      },

      EnumDef.fromQuery("EphemerisKeyType", "Non-sidereal target lookup type") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String`.T
        sql"SELECT id, id tag, short_name, long_name FROM e_ephemeris_type".query[(String, R)]
      },

      EnumDef.fromQuery("KeywordName", "Fits Keyword names") {
        type R = Record.`Symbol("tag")-> String, Symbol("name")-> String`.T
        sql"SELECT id, id tag, name FROM e_fits_keyword_names".query[(String, R)]
      },

      EnumDef.fromQuery("DhsKeywordName", "DHS Keyword names") {
        type R = Record.`Symbol("tag")-> String, Symbol("keyword")-> KeywordName, Symbol("name")-> String`.T
        sql"SELECT keyword, keyword tag, keyword tag, name FROM e_dhs_keyword_names".query[(String, R)]
      },

      EnumDef.fromQuery("LightSinkName", "SF Sink names") {
        type R = Record.`Symbol("tag")-> String, Symbol("name")-> String`.T
        sql"SELECT id, id tag, name FROM e_light_sink_names".query[(String, R)]
      },

      EnumDef.fromQuery("GiapiType", "giapi status types") {
        type R = Record.`Symbol("tag")-> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'giapi_type'
         """.query[(String, R)]
      },

      EnumDef.fromQuery("GiapiStatusApply", "Giapi Status Apply") {
        val (a, b) = (Witness(Symbol("Instrument")), Witness(Symbol("GiapiType")))
        type A = a.T
        type B = b.T
        type R = Record.`Symbol("tag")-> String, Symbol("instrument")-> EnumRef[A], Symbol("statusType")-> EnumRef[B], Symbol("statusItem")-> String, Symbol("applyItem")-> String, Symbol("tolerance")-> Option[BigDecimal]`.T
        val ret = sql"SELECT concat(instrument_id, id), concat(instrument_id, id) tag, instrument_id, type, status_item, apply_item, tolerance FROM e_giapi_status_apply".query[(String, R)]
        (ret, a.value: A, b.value: B)._1 // suppress unused warnigs
      },

      EnumDef.fromQuery("GiapiStatus", "Giapi Status") {
        val (a, b) = (Witness(Symbol("Instrument")), Witness(Symbol("GiapiType")))
        type A = a.T
        type B = b.T
        type R = Record.`Symbol("tag")-> String, Symbol("instrument")-> EnumRef[A], Symbol("statusType")-> EnumRef[B], Symbol("statusItem")-> String`.T
        val ret = sql"SELECT concat(instrument_id, id), concat(instrument_id, id) tag, instrument_id, type, status_item FROM e_giapi_status".query[(String, R)]
        (ret, a.value: A, b.value: B)._1 // suppress unused warnigs
      }
    )

}
