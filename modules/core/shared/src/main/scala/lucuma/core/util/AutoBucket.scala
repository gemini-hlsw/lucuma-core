// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.implicits.*
import eu.timepit.refined.auto.autoUnwrap
import eu.timepit.refined.cats.*
import lucuma.core.model.IntPercent

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

// map from percentage to (remaining, allocated)
case class AutoBucket private (value: SortedMap[IntPercent, AutoBucket.Element]):

  private def ensureBucket(at: IntPercent): AutoBucket =
    if value.contains(at) then this
    else 
      val (k, e) = value.find(_._1 > at).get // there will always be one
      ???

  def +(entry: (IntPercent, AutoBucket.Element)): AutoBucket =
    AutoBucket(value + entry)

  def minus(at: IntPercent, amount: FiniteDuration): Option[AutoBucket] =
    @tailrec def go(keys: List[IntPercent], remaining: FiniteDuration, accum: AutoBucket): Option[AutoBucket] =
      keys match
        case Nil => None
        case k :: tail =>
          val (e, r) = accum.value(k).allocate(remaining)
          val accumʹ = accum + (k -> e)
          r match
            case Some(d) => go(tail, d, accumʹ)
            case None => Some(accumʹ)          
    go(ensureBucket(at).value.keySet.filter(_ <= at).toList.reverse, amount, this)

object AutoBucket:

  case class Element(remaining: FiniteDuration, allocated: FiniteDuration):
    def allocate(amount: FiniteDuration): (Element, Option[FiniteDuration]) =
      if remaining > amount then (Element(remaining - amount, allocated + amount), None)
      else (Element(0.hours, allocated + remaining), Some(amount - remaining))

  def apply(total: FiniteDuration): AutoBucket =
    new AutoBucket(SortedMap(IntPercent.unsafeFrom(100) -> Element(total, 0.hours)))


