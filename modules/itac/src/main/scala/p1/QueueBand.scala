// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

abstract sealed class QueueBand(val number: Int) extends Ordered[QueueBand] with Product with Serializable {
  def compare(that: QueueBand): Int = number - that.number
}

object QueueBand {

  case object QBand1 extends QueueBand(1)
  case object QBand2 extends QueueBand(2)
  case object QBand3 extends QueueBand(3)
  case object QBand4 extends QueueBand(4)

  val values = List(QBand1, QBand2, QBand3, QBand4)
}