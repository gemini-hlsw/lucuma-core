// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.Eq
import cats.derived.*
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

/** A guide star read from the catalog, with its Gaia RUWE when the catalog has one. */
final case class GuideStarCatalogResult(target: Target.Sidereal, ruwe: Option[BigDecimal])
    derives Eq

object GuideStarCatalogResult:
  val target: Lens[GuideStarCatalogResult, Target.Sidereal] =
    Focus[GuideStarCatalogResult](_.target)

  val ruwe: Lens[GuideStarCatalogResult, Option[BigDecimal]] =
    Focus[GuideStarCatalogResult](_.ruwe)
