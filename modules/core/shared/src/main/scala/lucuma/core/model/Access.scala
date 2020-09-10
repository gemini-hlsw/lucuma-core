// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.Enumerated

/** Much of the time we only care about a user's access level. */
sealed abstract class Access(val name: String, val tag: String) extends Product with Serializable
object Access {

  case object Guest   extends Access("Guest",   "guest")
  case object Pi      extends Access("PI",      "pi")
  case object Ngo     extends Access("NGO",     "ngo")
  case object Staff   extends Access("Staff",   "staff")
  case object Admin   extends Access("Admin",   "admin")
  case object Service extends Access("Service", "service")

  implicit val EnumeratedType: Enumerated[Access] =
    new Enumerated[Access] {
      def all: List[Access] = List(Guest, Pi, Ngo, Staff, Admin, Service) // ordered by increasing power
      def tag(a: Access): String = a.tag
    }

}