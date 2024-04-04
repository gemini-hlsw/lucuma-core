// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.OrderTests
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.core.model.arb.*
import monocle.law.discipline.PrismTests
import munit.*

class UserInvitationSuite extends DisciplineSuite {
  import ArbUserInvitation.given

  // Laws
  checkAll("UserInvitation", OrderTests[UserInvitation].order)
  checkAll("UserInvitationCodec", CodecTests[UserInvitation].codec)
  checkAll("UserInvitationPrism", PrismTests(UserInvitation.fromString))
}
