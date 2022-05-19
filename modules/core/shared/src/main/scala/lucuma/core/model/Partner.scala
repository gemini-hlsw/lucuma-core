// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.Enumerated

/** NGO users are associated with a partner. */
sealed abstract class Partner(
  val tag:          String,
  val abbreviation: String,
  val name:         String,
  val shortName:    String
) extends Product
    with Serializable

case object Partner {

  case object Ar extends Partner("ar", "AR", "Argentina", "Argentina")
  case object Br extends Partner("br", "BR", "Brazil", "Brazil")
  case object Ca extends Partner("ca", "CA", "Canada", "Canada")
  case object Cl extends Partner("cl", "CL", "Chile", "Chile")
  case object Kr extends Partner("kr", "KR", "Korea", "Korea")
  case object Uh extends Partner("uh", "UH", "University of Hawaii", "U of H")
  case object Us extends Partner("us", "US", "United States", "USA")

  implicit val EnumeratedPartner: Enumerated[Partner] =
    new Enumerated[Partner] {
      def all: List[Partner] = List(Ar, Br, Ca, Cl, Kr, Uh, Us)
      def tag(a: Partner): String = a.tag
    }

}
