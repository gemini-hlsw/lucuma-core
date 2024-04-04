// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.model.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbUserInvitation {

  val bodyGen: Gen[String] =
    Gen.stringOfN(96, Gen.oneOf(Gen.hexChar.map(_.toLower), Gen.numChar))

  Gen.posNum[Long]

  given Arbitrary[UserInvitation] =
    Arbitrary {
      for {
        id <- Gen.chooseNum[Long](256, Long.MaxValue)
        b  <- bodyGen
      } yield UserInvitation(UserInvitation.Id(PosLong.unsafeFrom(id)), b)
    }

  given Cogen[UserInvitation] =
    Cogen[(Long, String)].contramap(x => (x.id.value.value, x.body))

}

object ArbUserInvitation extends ArbUserInvitation
