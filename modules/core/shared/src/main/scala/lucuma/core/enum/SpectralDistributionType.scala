// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Enumerated

sealed trait SpectralDistributionType extends Product with Serializable

object SpectralDistributionType {
  case object BlackBody  extends SpectralDistributionType
  case object PowerLaw   extends SpectralDistributionType
  case object Stellar    extends SpectralDistributionType
  case object NonStellar extends SpectralDistributionType


  /** @group Typeclass Instances */
  implicit val SpectralDistributionTypeEnumerated: Enumerated[SpectralDistributionType] =
    Enumerated.of(BlackBody, PowerLaw, Stellar, NonStellar)

}
