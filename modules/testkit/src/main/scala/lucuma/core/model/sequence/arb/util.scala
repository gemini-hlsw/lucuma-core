// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

def genBoundedList[A: Arbitrary](limit: Int): Gen[List[A]] =
  Gen.choose(0, limit).flatMap(sz => Gen.listOfN(sz, arbitrary[A]))

