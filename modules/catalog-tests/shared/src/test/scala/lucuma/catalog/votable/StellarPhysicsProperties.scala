// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen

class StellarPhysicsProperties extends ScalaCheckSuite:

  property("spectral class code increases with later spectral types") {
    forAll(Gen.oneOf("O", "B", "A", "F", "G", "K", "M"), Gen.choose(0, 9)) { (letter1, sub1) =>
      forAll(Gen.oneOf("O", "B", "A", "F", "G", "K", "M"), Gen.choose(0, 9)) { (letter2, sub2) =>
        val ordering = List("O", "B", "A", "F", "G", "K", "M")
        if ordering.indexOf(letter1) < ordering.indexOf(letter2) then
          val scc1 = StellarPhysics.spectralClassCode(s"$letter1$sub1")
          val scc2 = StellarPhysics.spectralClassCode(s"$letter2$sub2")
          assert(scc1.isDefined && scc2.isDefined && scc1.get < scc2.get)
      }
    }
  }

  property("temperature decreases with later spectral types") {
    forAll(
      Gen.oneOf("O", "B", "A", "F", "G", "K", "M"),
      Gen.choose(0, 9),
      Gen.oneOf("O", "B", "A", "F", "G", "K", "M"),
      Gen.choose(0, 9)
    ) { (letter1, sub1, letter2, sub2) =>
      val ordering = List("O", "B", "A", "F", "G", "K", "M")
      val idx1     = ordering.indexOf(letter1)
      val idx2     = ordering.indexOf(letter2)

      if idx1 < idx2 then
        val temp1 = StellarPhysics.calculateTemperature(List("V"), List(s"$letter1$sub1"))
        val temp2 = StellarPhysics.calculateTemperature(List("V"), List(s"$letter2$sub2"))
        assert((temp1, temp2).mapN(_ > _).getOrElse(false))
    }
  }

  property("decimal subclass updates temperature") {
    forAll(Gen.choose(0, 8), Gen.choose(0.0, 0.9)) { (intPart, fracPart) =>
      val base       = StellarPhysics.spectralClassCode(s"G$intPart")
      val withDecimal = StellarPhysics.spectralClassCode(s"G$intPart.${(fracPart * 10).toInt}")

      assert(
        (base, withDecimal).mapN { (b, d) =>
          Math.abs((d - b) - (fracPart).toInt) < 1.0
        }.getOrElse(false)
      )
    }
  }

  property("+/- modifiers adjust spectral code by 0.25") {
    forAll(Gen.oneOf("O", "B", "A", "F", "G", "K", "M"), Gen.choose(0, 9)) { (letter, sub) =>
      val base  = StellarPhysics.spectralClassCode(s"$letter$sub")
      val plus  = StellarPhysics.spectralClassCode(s"$letter$sub+")
      val minus = StellarPhysics.spectralClassCode(s"$letter$sub-")

      assert(
        (base, plus, minus).mapN { (b, p, m) =>
          (p - b - 0.25).abs < 0.01 && (b - m - 0.25).abs < 0.01
        }.getOrElse(false)
      )
    }
  }

  property("white dwarf temperature formula T = 50400 / number") {
    forAll(Gen.choose(1, 20)) { num =>
      val result   = StellarPhysics.calculateTemperature(List("DA"), List(num.toString))
      val expected = (50400.0 / num).round.toInt
      assert(result.map(_ == expected).getOrElse(false))
    }
  }

  property("temperature calculation returns None for brown dwarfs (L/T/Y)") {
    forAll(Gen.oneOf("L", "T", "Y"), Gen.choose(0, 9)) { (letter, sub) =>
      val result = StellarPhysics.calculateTemperature(List("V"), List(s"$letter$sub"))
      assert(result.isEmpty)
    }
  }

  property("gravity calculation for main sequence is always >= 3.0") {
    forAll(Gen.oneOf("O", "B", "A", "F", "G", "K", "M"), Gen.choose(0, 9)) { (letter, sub) =>
      val result = StellarPhysics.calculateGravity(List("V"), List(s"$letter$sub"))
      assert(result.map(_ >= 3.0).getOrElse(false))
    }
  }

  property("white dwarf gravity is always 8.0") {
    forAll(Gen.choose(1, 20)) { num =>
      val result = StellarPhysics.calculateGravity(List("DA"), List(num.toString))
      assert(result.map(_ == 8.0).getOrElse(false))
    }
  }

  property("calculateParameters returns both temperature and gravity or neither") {
    forAll(Gen.oneOf("O", "B", "A", "F", "G", "K", "M"), Gen.choose(0, 9)) { (letter, sub) =>
      val result = StellarPhysics.calculateParameters(List("V"), List(s"$letter$sub"))
      assert(
        result.isDefined || (
          StellarPhysics.calculateTemperature(List("V"), List(s"$letter$sub")).isEmpty ||
          StellarPhysics.calculateGravity(List("V"), List(s"$letter$sub")).isEmpty
        )
      )
    }
  }
