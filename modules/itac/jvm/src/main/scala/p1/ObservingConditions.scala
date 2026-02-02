// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import scala.Ordering.Implicits._

case class ObservingConditions(
  cc: CloudCover    = CloudCover.CCAny,
  iq: ImageQuality  = ImageQuality.IQAny,
  sb: SkyBackground = SkyBackground.SBAny,
  wv: WaterVapor    = WaterVapor.WVAny
) {

  def isPoorWeather: Boolean =
    wv == WaterVapor.WVAny && (
      cc >= CloudCover.CC80 || (iq == ImageQuality.IQAny && cc > CloudCover.CC50)
    )

  override def toString: String =
    s"$cc, $iq, $sb, $wv"

}

object ObservingConditions {

  val AnyConditions =
    ObservingConditions(
      CloudCover.CCAny,
      ImageQuality.IQAny,
      SkyBackground.SBAny,
      WaterVapor.WVAny
    )

}
