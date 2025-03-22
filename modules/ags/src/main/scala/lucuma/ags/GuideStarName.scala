// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.RefinedNewType
import monocle.Prism

import scala.util.matching.Regex

// For now, at least, we only support Gaia.
// The same regex used in the odb for a database check. When changing one, both should be updated.
private val regexStr = """^Gaia DR3 (-?\d+)$"""
type GuideStarNamePred = MatchesRegex[regexStr.type]

object GuideStarName extends RefinedNewType[String, GuideStarNamePred]:
  def gaiaSourceId: Prism[GuideStarName, Long] =
    Prism[GuideStarName, Long](_.toGaiaSourceId)(id => GuideStarName.unsafeFrom(s"Gaia DR3 $id"))

  extension (self: GuideStarName)
    def toGaiaSourceId: Option[Long]     =
      val regex = Regex(regexStr)
      self.value.value match
        case regex(d) => d.toLongOption
        case _        => None
    def toNonEmptyString: NonEmptyString = NonEmptyString.unsafeFrom(self.value.value)

type GuideStarName = GuideStarName.Type
