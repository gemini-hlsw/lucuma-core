// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.*
import lucuma.core.util.arb.ArbGid
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbUserInvitation:
  import ArbGid.given

  val bodyGen: Gen[String] =
    Gen.stringOfN(96, Gen.oneOf(Gen.hexChar.map(_.toLower), Gen.numChar))

  given Arbitrary[UserInvitation] =
    Arbitrary:
      for
        id <- arbitrary[ProgramUser.Id]
        b  <- bodyGen
      yield UserInvitation(id, b)

  given Cogen[UserInvitation] =
    Cogen[(ProgramUser.Id, String)].contramap(x => (x.id, x.body))

object ArbUserInvitation extends ArbUserInvitation