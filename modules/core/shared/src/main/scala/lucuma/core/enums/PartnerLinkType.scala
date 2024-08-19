// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum PartnerLinkType(val tag: String) derives Enumerated:

  case HasPartner            extends PartnerLinkType("has_partner")
  case HasNonPartner         extends PartnerLinkType("has_non_partner")
  case HasUnspecifiedPartner extends PartnerLinkType("has_unspecified_partner")

end PartnerLinkType
