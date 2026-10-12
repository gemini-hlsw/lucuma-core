// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma

import munit.CatsEffectSuite

class MainSuite extends CatsEffectSuite:

  test("run against local ocs workspace"):
    itac.Main.run(List("-d", "/tmp/itac", "queue"))