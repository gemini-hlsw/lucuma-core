// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import lucuma.core.model.IntCentiPercent

/** A conditions specification paired with an arbitrary value. */
final case class ConditionsBin[A](cat: ConditionsCategory, binValue: A) {

  def map[B](f: A => B): ConditionsBin[B] =
    copy(binValue = f(binValue))

  def updated(newValue: A): ConditionsBin[A] =
    map(_ => newValue)

}

object ConditionsBin {

  def of[A](bins: (ConditionsCategory, A)*): List[ConditionsBin[A]] =
    bins.map(tup => ConditionsBin(tup._1, tup._2)).toList

  def ofPercent(bins: (ConditionsCategory, Double)*): List[ConditionsBin[IntCentiPercent]] =
    bins.map(tup => ConditionsBin(tup._1, IntCentiPercent.unsafeFromPercent(tup._2))).toList

}

