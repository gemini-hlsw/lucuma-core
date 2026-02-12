// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.queue.time

import cats.syntax.all.*
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.Enumerated

/** A total mapping from TimeAccountingCategory to Time. */
final class TimeAccountingCategoryTime private (private val f: TimeAccountingCategory => Time) extends (TimeAccountingCategory => Time) {
  def map(op: Time => Time): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(f map op)
  def filter(g: TimeAccountingCategory => Boolean): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(p => if (g(p)) f(p) else Time.Zero)
  def apply(p: TimeAccountingCategory): Time = f(p)
  def zipWith(p: TimeAccountingCategoryTime)(g: (Time, Time) => Time): TimeAccountingCategoryTime = new TimeAccountingCategoryTime((f, p.f).mapN(g))
  def +(p: TimeAccountingCategoryTime): TimeAccountingCategoryTime = zipWith(p)(_ + _)
  def -(p: TimeAccountingCategoryTime): TimeAccountingCategoryTime = zipWith(p)(_ - _)
  def *(p: Percent): TimeAccountingCategoryTime = map(_ * p)
  def total: Time = Enumerated[TimeAccountingCategory].all.foldMap(f)
  def add(p: TimeAccountingCategory, t: Time): TimeAccountingCategoryTime = this + TimeAccountingCategoryTime.single(p, t)
}

object TimeAccountingCategoryTime {
  val empty: TimeAccountingCategoryTime = constant(Time.Zero)
  def constant(t: Time) = new TimeAccountingCategoryTime(_ => t)
  def single(p: TimeAccountingCategory, t: Time): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(a => if (p == a) t else Time.Zero)
  def fromMap(map: Map[TimeAccountingCategory, Time]): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(map.withDefaultValue(Time.Zero))
  def fromFunction(f: TimeAccountingCategory => Time): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(f)
}

