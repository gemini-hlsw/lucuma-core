// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.sql
package enum

import doobie._, doobie.implicits._
import shapeless.record._

object TargetEnums {

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("AsterismType", "asterism types") {
        type R = Record.`Symbol("tag")-> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'asterism_type'
         """.query[(String, R)]
      },

      EnumDef.fromQuery("TrackType", "track types") {
        type R = Record.`Symbol("tag")-> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'e_track_type'
         """.query[(String, R)]
      },

      EnumDef.fromQuery("MagnitudeSystem", "magnitude system") {
        type R = Record.`Symbol("tag")-> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'magnitude_system'
         """.query[(String, R)]
      },

      EnumDef.fromQuery("MagnitudeBand", "magnitude band") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("center")-> Wavelength.Nm, Symbol("width")-> Int, Symbol("magnitudeSystem")-> MagnitudeSystem`.T
        sql"""SELECT id, id tag, short_name, long_name, center, width, default_system FROM e_magnitude_band""".query[(String, R)]
      },

      EnumDef.fromQuery("UserTargetType", "user target type") {
        type R = Record.`Symbol("tag")-> String, Symbol("shortName")-> String, Symbol("longName")-> String, Symbol("obsolete")-> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_user_target_type".query[(String, R)]
      }

    )

}
