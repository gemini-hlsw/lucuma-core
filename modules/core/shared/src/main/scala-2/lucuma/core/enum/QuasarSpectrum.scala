// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class QuasarSpectrum(
  val tag:  String,
  val name: String
) extends Product
    with Serializable

object QuasarSpectrum {
  case object QS0  extends QuasarSpectrum("QS0", "QSO (80nm - 0.855μm)")
  case object QS02 extends QuasarSpectrum("QS02", "QSO (276nm - 3.52μm)")

  implicit val enumQuasarSpectrum: Enumerated[QuasarSpectrum] =
    Enumerated.from(QS0, QS02).withTag(_.tag)

  implicit val displayQuasarSpectrum: Display[QuasarSpectrum] =
    Display.byShortName(_.name)
}
