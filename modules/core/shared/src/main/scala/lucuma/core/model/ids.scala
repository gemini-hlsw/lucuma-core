// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import eu.timepit.refined.auto._

object Asterism      extends WithId('a')
object Atom          extends WithId('m')
object Configuration extends WithId('x')
object ConstraintSet extends WithId('c')
object Observation   extends WithId('o')
object Program       extends WithId('p')
object Step          extends WithId('s')
