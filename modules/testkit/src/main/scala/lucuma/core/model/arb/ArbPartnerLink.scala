// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.const


trait ArbPartnerLink {

  import ArbEnumerated.given

  given Arbitrary[PartnerLink.HasPartner] =
    Arbitrary {
      arbitrary[Partner].map(PartnerLink.HasPartner.apply)
    }

  given Cogen[PartnerLink.HasPartner] =
    Cogen[Partner].contramap(_.partner)

  given Arbitrary[PartnerLink] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[PartnerLink.HasPartner],
        const(PartnerLink.HasNonPartner),
        const(PartnerLink.HasUnspecifiedPartner)
      )
    }

  given Cogen[PartnerLink] =
    Cogen[(PartnerLinkType, Option[Partner])].contramap { a => (
      a.linkType,
      a.partnerOption
    )}

}

object ArbPartnerLink extends ArbPartnerLink