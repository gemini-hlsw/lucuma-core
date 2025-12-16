// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom
package arb

import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.arb.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbOffsetGenerator:
  import ArbAngle.given
  import ArbOffset.given

  val genRandomOffsetGenerator: Gen[OffsetGenerator.Random] =
    for 
      size   <- arbitrary[Angle]
      center <- arbitrary[Offset]
    yield OffsetGenerator.Random(size, center)

  val genSpiralOffsetGenerator: Gen[OffsetGenerator.Spiral] =
    for 
      size   <- arbitrary[Angle]
      center <- arbitrary[Offset]
    yield OffsetGenerator.Spiral(size, center)

  val genUniformOffsetGenerator: Gen[OffsetGenerator.Uniform] =
    for
      cornerA <- arbitrary[Offset]
      cornerB <- arbitrary[Offset]
    yield OffsetGenerator.Uniform(cornerA, cornerB)

  val genOffsetGenerator: Gen[OffsetGenerator] =
    Gen.oneOf(
      genRandomOffsetGenerator,
      genSpiralOffsetGenerator,
      genUniformOffsetGenerator
    )

  given Arbitrary[OffsetGenerator] =
    Arbitrary(genOffsetGenerator)

  given Cogen[OffsetGenerator.Random] =
    Cogen[(Angle, Offset)].contramap(r => (r.size, r.center))

  given Cogen[OffsetGenerator.Spiral] =
    Cogen[(Angle, Offset)].contramap(r => (r.size, r.center))

  given Cogen[OffsetGenerator.Uniform] =
    Cogen[(Offset, Offset)].contramap(r => (r.cornerA, r.cornerB))

  given Cogen[OffsetGenerator] =
    Cogen[
      Either[
        OffsetGenerator.Random,
        Either[
          OffsetGenerator.Spiral,
          OffsetGenerator.Uniform
        ]
      ]
    ].contramap:
      case r @ OffsetGenerator.Random(_, _)  => Left(r)
      case s @ OffsetGenerator.Spiral(_, _)  => Right(Left(s))
      case u @ OffsetGenerator.Uniform(_, _) => Right(Right(u))


object ArbOffsetGenerator extends ArbOffsetGenerator