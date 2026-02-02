// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.queue.time

import cats.syntax.all.*
import edu.gemini.tac.qengine.util.{Percent, Time}
import edu.gemini.tac.qengine.ctx.Partner
import lucuma.core.util.Enumerated

/** A total mapping from Partner to Time. */
final class PartnerTime private (private val f: Partner => Time) extends (Partner => Time) {
  def map(op: Time => Time): PartnerTime = new PartnerTime(f map op)
  def filter(g: Partner => Boolean): PartnerTime = new PartnerTime(p => if (g(p)) f(p) else Time.Zero)
  def apply(p: Partner): Time = f(p)
  def zipWith(p: PartnerTime)(g: (Time, Time) => Time): PartnerTime = new PartnerTime((f, p.f).mapN(g))
  def +(p: PartnerTime): PartnerTime = zipWith(p)(_ + _)
  def -(p: PartnerTime): PartnerTime = zipWith(p)(_ - _)
  def *(p: Percent): PartnerTime = map(_ * p)
  def total: Time = Enumerated[Partner].all.foldMap(f)
  def add(p: Partner, t: Time): PartnerTime = this + PartnerTime.single(p, t)
}

object PartnerTime {
  val empty: PartnerTime = constant(Time.Zero)
  def constant(t: Time) = new PartnerTime(_ => t)
  def single(p: Partner, t: Time): PartnerTime = new PartnerTime(a => if (p == a) t else Time.Zero)
  def fromMap(map: Map[Partner, Time]): PartnerTime = new PartnerTime(map.withDefaultValue(Time.Zero))
  def fromFunction(f: Partner => Time): PartnerTime = new PartnerTime(f)
}

