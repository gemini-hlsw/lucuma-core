// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbConfigurationRequest:
  import ArbConfiguration.given

  given Arbitrary[ConfigurationRequest] =
    Arbitrary:
      for
        id <- arbitrary[ConfigurationRequest.Id]
        st <- arbitrary[ConfigurationRequestStatus]
        ju <- arbitrary[Option[NonEmptyString]]
        co <- arbitrary[Configuration]
      yield ConfigurationRequest(id, st, ju, co)

  given Cogen[ConfigurationRequest] = 
    Cogen[(ConfigurationRequest.Id, ConfigurationRequestStatus, Option[NonEmptyString], Configuration)]
      .contramap(cr => (cr.id, cr.status, cr.justification, cr.configuration))

object ArbConfigurationRequest extends ArbConfigurationRequest
