// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.ctx

import lucuma.core.enums.TimeAccountingCategory

type Partner = TimeAccountingCategory
val Partner = TimeAccountingCategory

// sealed abstract class Partner(
//   val id: String,
//   val sites: Set[Site]
// ) {
//   final override def toString = id
// }

// object Partner {
//   import Site.{ GN, GS }

//   case object AR     extends Partner("AR",     Set(GN, GS))
//   case object BR     extends Partner("BR",     Set(GN, GS))
//   case object CA     extends Partner("CA",     Set(GN, GS))
//   case object CFH    extends Partner("CFH",    Set(GN, GS))
//   case object CL     extends Partner("CL",     Set(GS))
//   case object KECK   extends Partner("KECK",   Set(GN, GS))
//   case object KR     extends Partner("KR",     Set(GN, GS))
//   case object LP     extends Partner("LP",     Set(GN, GS))
//   case object SUBARU extends Partner("SUBARU", Set(GN, GS))
//   case object UH     extends Partner("UH",     Set(GN))
//   case object US     extends Partner("US",     Set(GN, GS))
//   case object GT     extends Partner("GT",     Set(GN, GS))

//   def all: List[Partner] =
//     List(AR, BR, CA, CFH, CL, KECK, KR, LP, SUBARU, UH, US, GT)

//   def fromString(id: String): Option[Partner] =
//     all.find(_.id == id)

//   /**
//    * Creates a map with entries for all Partners according to the value
//    * returned by the supplied function.
//    */
//   def mkMap[T](values: List[Partner], f: Partner => T): Map[Partner, T] =
//     values.map(p => p -> f(p)).toMap

//   // Completes the given PartialFunction by returning a default value for any
//   // Partner for which pf is not defined.
//   private def complete[T](pf: PartialFunction[Partner, T], default: T): Partner => T =
//     p => pf.lift(p).getOrElse(default)

//   /**
//    * Creates a map with entries for all Partners according to the value
//    * returned by the supplied partial function, using the supplied default
//    * for any values not in pf's domain.
//    */
//   def mkMap[T](values: List[Partner], pf: PartialFunction[Partner, T], default: T): Map[Partner, T] =
//     mkMap(values, complete(pf, default))

// }
