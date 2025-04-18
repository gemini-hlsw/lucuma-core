// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import coulomb.*
import coulomb.syntax.*
import lucuma.core.math.units.*

val TelescopePlateScale: Quantity[BigDecimal, ArcSecondPerMillimeter] =
  BigDecimal(1.611444).withUnit[ArcSecondPerMillimeter]

