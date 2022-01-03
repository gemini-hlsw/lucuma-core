// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import coulomb.UnitTypeName
import coulomb.define.BaseUnit
import coulomb.define.DerivedUnit
import coulomb.unitops.UnitString
import lucuma.core.syntax.string._
import lucuma.core.util.TypeString
import spire.math.Rational

object BaseUnitDef {

  /** Convenience method to create a coulomb `BaseUnit` and `TypeString` in one go. */
  def apply[U](name: String = "", abbv: String = "", serialized: String = "")(implicit
    ut:              UnitTypeName[U]
  ): BaseUnit[U] with TypeString[U] = {
    val base = BaseUnit[U](name, abbv)

    val _serialized = if (serialized != "") serialized else base.name.toScreamingSnakeCase

    new BaseUnit[U](base.name, base.abbv) with TypeString[U] {
      override val serialized: String = _serialized
    }
  }
}

object DerivedUnitDef {

  /** Convenience method to create a coulomb `DerivedUnit` and `TypeString` in one go. */
  def apply[U, D](
    coef:        Rational = Rational(1),
    name:        String = "",
    abbv:        String = "",
    serialized:  String
  )(implicit ut: UnitTypeName[U]): DerivedUnit[U, D] with TypeString[U] = {
    val base = DerivedUnit[U, D](coef, name, abbv)

    val _serialized = if (serialized != "") serialized else base.name.toScreamingSnakeCase

    new DerivedUnit[U, D](base.coef, base.name, base.abbv) with TypeString[U] {
      override val serialized: String = _serialized
    }
  }
}

/** Convenience class to create a coulomb `UnitString` and `TypeString` in one go. */

final case class UnitStringDef[U](full: String, abbv: String, serialized: String)
    extends UnitString[U]
    with TypeString[U]

object UnitStringDef {

  /** Convenience method to create a coulomb `UnitString` and `TypeString` in one go. */
  def apply[U](abbv: String, serialized: String): UnitStringDef[U] =
    UnitStringDef[U](abbv, abbv, serialized)
}
