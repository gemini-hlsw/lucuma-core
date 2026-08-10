// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order
import cats.syntax.eq.*

/**
 * How a target is tracked: the persisted counterpart of [[Tracking]], which is the computed one. A
 * nonsidereal target stores an [[Ephemeris.Key]] and only becomes an `EphemerisTracking` once the
 * ephemeris has been fetched, so the two cannot be the same type.
 *
 * Every target has one of these except a [[Target.Opportunity]] still waiting for its alert, which
 * is why [[Target.resolution]] is optional.
 */
enum TargetResolution:
  case Sidereal(tracking: SiderealTracking, catalogInfo: Option[CatalogInfo])
  case Nonsidereal(ephemerisKey: Ephemeris.Key)

object TargetResolution:

  given Eq[TargetResolution] = Eq.instance {
    case (Sidereal(a, ac), Sidereal(b, bc)) => a === b && ac === bc
    case (Nonsidereal(a), Nonsidereal(b))   => a === b
    case _                                  => false
  }

  /**
   * Ordered as the target subtypes are: nonsidereal first, then sidereal.
   *
   * Not implicit.
   */
  val TrackOrder: Order[TargetResolution] =
    Order.from {
      case (Sidereal(a, _), Sidereal(b, _)) => Order[SiderealTracking].compare(a, b)
      case (Nonsidereal(a), Nonsidereal(b)) => Order[Ephemeris.Key].compare(a, b)
      case (Nonsidereal(_), Sidereal(_, _)) => -1
      case (Sidereal(_, _), Nonsidereal(_)) => 1
    }
