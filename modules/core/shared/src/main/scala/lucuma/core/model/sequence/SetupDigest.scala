// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.cats.given
import lucuma.core.util.TimeSpan

final case class SetupDigest(
  setupTime:          SetupTime,
  acquisitionCount:   NonNegInt,
  reacquisitionCount: NonNegInt
):

  def addAcquisition: SetupDigest =
    copy(acquisitionCount = NonNegInt.unsafeFrom(acquisitionCount.value + 1))

  def addReacquisition: SetupDigest =
    copy(reacquisitionCount = NonNegInt.unsafeFrom(reacquisitionCount.value + 1))

  val timeEstimate: TimeSpan =
    (setupTime.acquisition   *| acquisitionCount.value  ) +|
    (setupTime.reacquisition *| reacquisitionCount.value)

object SetupDigest:

  val Zero: SetupDigest =
    SetupDigest(SetupTime.Zero, NonNegInt.MinValue, NonNegInt.MinValue)

  def fromSetupTime(s: SetupTime): SetupDigest =
    SetupDigest(s, NonNegInt.MinValue, NonNegInt.MinValue)

  given Eq[SetupDigest] =
    Eq.by(a => (a.setupTime, a.acquisitionCount, a.reacquisitionCount))