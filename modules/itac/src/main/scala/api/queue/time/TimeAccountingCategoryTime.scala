// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.queue.time

import cats.syntax.all.*
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.IntCentiPercent
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

/** A total mapping from TimeAccountingCategory to Time. */
final class TimeAccountingCategoryTime private (private val f: TimeAccountingCategory => TimeSpan) extends (TimeAccountingCategory => TimeSpan) {
  def map(op: TimeSpan => TimeSpan): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(f map op)
  def filter(g: TimeAccountingCategory => Boolean): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(p => if (g(p)) f(p) else TimeSpan.Zero)
  def apply(p: TimeAccountingCategory): TimeSpan = f(p)
  def zipWith(p: TimeAccountingCategoryTime)(g: (TimeSpan, TimeSpan) => TimeSpan): TimeAccountingCategoryTime = new TimeAccountingCategoryTime((f, p.f).mapN(g))
  def +(p: TimeAccountingCategoryTime): TimeAccountingCategoryTime = zipWith(p)(_ +| _)
  def -(p: TimeAccountingCategoryTime): TimeAccountingCategoryTime = zipWith(p)(_ -| _)
  def *(p: IntCentiPercent): TimeAccountingCategoryTime = map(_ *| p)
  def total: TimeSpan = Enumerated[TimeAccountingCategory].all.foldMap(f)
  def add(p: TimeAccountingCategory, t: TimeSpan): TimeAccountingCategoryTime = this + TimeAccountingCategoryTime.single(p, t)
}

object TimeAccountingCategoryTime {
  val empty: TimeAccountingCategoryTime = constant(TimeSpan.Zero)
  def constant(t: TimeSpan) = new TimeAccountingCategoryTime(_ => t)
  def single(p: TimeAccountingCategory, t: TimeSpan): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(a => if (p == a) t else TimeSpan.Zero)
  def fromMap(map: Map[TimeAccountingCategory, TimeSpan]): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(map.withDefaultValue(TimeSpan.Zero))
  def fromFunction(f: TimeAccountingCategory => TimeSpan): TimeAccountingCategoryTime = new TimeAccountingCategoryTime(f)
}

