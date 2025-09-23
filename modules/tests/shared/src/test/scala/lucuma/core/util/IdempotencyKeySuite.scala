// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import lucuma.core.optics.laws.discipline.*
import lucuma.core.util.arb.ArbIdempotencyKey
import lucuma.core.util.arb.ArbIdempotencyKey.uuidStrings
import munit.*

class IdempotencyKeySuite extends DisciplineSuite:
  import ArbIdempotencyKey.given

  checkAll("IdempotencyKey.FromString", FormatTests(IdempotencyKey.FromString).formatWith(uuidStrings))
