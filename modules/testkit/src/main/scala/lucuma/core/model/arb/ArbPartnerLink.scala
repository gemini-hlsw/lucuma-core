// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.const


trait ArbPartnerLink:

  import ArbEnumerated.given

  given Arbitrary[PartnerLink.HasGeminiPartner] =
    Arbitrary:
      arbitrary[Partner].map(PartnerLink.HasGeminiPartner.apply)

  given Cogen[PartnerLink.HasGeminiPartner] =
    Cogen[Partner].contramap(_.partner)

  given Arbitrary[PartnerLink.HasExchangePartner] =
    Arbitrary:
      arbitrary[ExchangePartner].map(PartnerLink.HasExchangePartner.apply)

  given Cogen[PartnerLink.HasExchangePartner] =
    Cogen[ExchangePartner].contramap(_.partner)

  given Arbitrary[PartnerLink] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[PartnerLink.HasGeminiPartner],
        arbitrary[PartnerLink.HasExchangePartner],
        const(PartnerLink.HasNonPartner),
        const(PartnerLink.HasUnspecifiedPartner)
      )

  given Cogen[PartnerLink] =
    Cogen[(PartnerLinkType, Option[Partner], Option[ExchangePartner])].contramap { a => (
      a.linkType,
      a.geminiPartnerOption,
      a.exchangePartnerOption
    )}

object ArbPartnerLink extends ArbPartnerLink