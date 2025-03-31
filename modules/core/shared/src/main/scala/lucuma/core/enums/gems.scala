// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.NewBoolean

private trait UsageNewType extends NewBoolean { val Use = True; val DontUse = False }

object Cwfs1Usage extends UsageNewType
type Cwfs1Usage = Cwfs1Usage.Type

object Cwfs2Usage extends UsageNewType
type Cwfs2Usage = Cwfs2Usage.Type

object Cwfs3Usage extends UsageNewType
type Cwfs3Usage = Cwfs3Usage.Type

object Odgw1Usage extends UsageNewType
type Odgw1Usage = Odgw1Usage.Type

object Odgw2Usage extends UsageNewType
type Odgw2Usage = Odgw2Usage.Type

object Odgw3Usage extends UsageNewType
type Odgw3Usage = Odgw3Usage.Type

object Odgw4Usage extends UsageNewType
type Odgw4Usage = Odgw4Usage.Type

object P1Usage extends UsageNewType
type P1Usage = P1Usage.Type

object OIUsage extends UsageNewType
type OIUsage = OIUsage.Type
