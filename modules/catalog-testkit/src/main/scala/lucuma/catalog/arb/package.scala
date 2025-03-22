// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.arb

import lucuma.catalog.votable

object all
    extends votable.arb.ArbUcd
    with votable.arb.ArbFieldId
    with ArbAngularSize
    with ArbBrightnessConstraints
    with ArbCatalogTargetResult
