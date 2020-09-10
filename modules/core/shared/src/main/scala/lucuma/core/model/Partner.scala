// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.Enumerated

/** NGO users are associated with a partner. */
sealed abstract class Partner(
  val tag:  String,
  val name: String,
) extends Product with Serializable

case object Partner {

  case object Ar extends Partner("ar", "Argentina")
  case object Br extends Partner("br", "Brazil")
  case object Ca extends Partner("ca", "Canada")
  case object Cl extends Partner("cl", "Chile")
  case object Kr extends Partner("kr", "Korea")
  case object Uh extends Partner("uh", "University of Hawaii")
  case object Us extends Partner("us", "United States")

  implicit val EnumeratedPartner: Enumerated[Partner] =
    new Enumerated[Partner] {
      def all: List[Partner] = List(Ar, Br, Ca, Cl, Kr, Uh, Us)
      def tag(a: Partner): String = a.tag
    }

}