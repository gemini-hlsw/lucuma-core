// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.implicits._
import coulomb.refined._
import coulomb.si.Kelvin
import eu.timepit.refined.numeric.Positive
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.enum.NonStellarLibrarySpectrum
import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbSpectralDistribution {

  import SpectralDistribution._

  implicit val arbBlackBody: Arbitrary[SpectralDistribution.BlackBody] =
    Arbitrary {
      for {
        a <- Gen.choose(BigDecimal(1), BigDecimal(10000))
      } yield BlackBody(a.withRefinedUnit[Positive, Kelvin])
    }

  implicit val cogBlackBody: Cogen[BlackBody] =
    Cogen[BigDecimal].contramap(_.temperature.value.value)

  implicit val arbPowerLaw: Arbitrary[SpectralDistribution.PowerLaw] =
    Arbitrary {
      for {
        a <- arbitrary[BigDecimal]
      } yield PowerLaw(a)
    }

  implicit val cogPowerLaw: Cogen[PowerLaw] =
    Cogen[BigDecimal].contramap(_.index)

  implicit val arbLibrary: Arbitrary[SpectralDistribution.Library] =
    Arbitrary {
      for {
        s <- arbitrary[StellarLibrarySpectrum].map(_.asLeft)
        l <- arbitrary[NonStellarLibrarySpectrum].map(_.asRight)
        a <- Gen.oneOf(s, l)
      } yield Library(a)
    }

  implicit val cogLibrary: Cogen[Library] =
    Cogen[Either[StellarLibrarySpectrum, NonStellarLibrarySpectrum]].contramap(_.librarySpectrum)

  implicit val arbSpectralDistribution: Arbitrary[SpectralDistribution] =
    Arbitrary {
      for {
        b <- arbitrary[BlackBody]
        p <- arbitrary[PowerLaw]
        l <- arbitrary[Library]
        r <- Gen.oneOf(b, p, l)
      } yield r
    }

  implicit val cogSpectralDistribution: Cogen[SpectralDistribution] =
    Cogen[Either[BigDecimal, Either[BigDecimal, Either[StellarLibrarySpectrum, NonStellarLibrarySpectrum]]]].contramap {
      case BlackBody(t) => t.value.value.asLeft
      case PowerLaw(i)  => i.asLeft.asRight
      case Library(l)   => l.asRight.asRight
    }
}

object ArbSpectralDistribution extends ArbSpectralDistribution
