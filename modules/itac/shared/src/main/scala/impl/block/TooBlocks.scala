// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.block

import edu.gemini.tac.qengine.util.Time

/**
 * An object used to split a Block into a number of smaller blocks, one for each
 * of a collection of remaining time values in group.  The goal is to evenly
 * distribute the time across all elements, but the lack of remaining time in
 * any element cannot prevent the scheduling of a ToO observation.
 *
 * <p>We try to split the time evenly, but any element that cannot hold
 * its share of the ToO observation is simply filled to the max and the left
 * over time is spread across the remaining bins.
 */
object TooBlocks {
  private val indexOrdering: Ordering[(_, Int)] = new Ordering[(_, Int)] {
    def compare(x: (_, Int), y: (_, Int)) = x._2 - y._2
  }

  private def remainingTimeOrdering[T](remTime: T => Time): Ordering[(T, Int)] = new Ordering[(T, Int)] {
    def compare(x: (T, Int), y: (T, Int)): Int = remTime(x._1).compare(remTime(y._1))
  }

  // Here we concern ourselves with the limit for the provided obs conditions.
  // The target is ignored since the target is not known.  We will be spreading
  // the time over the dec ranges.
//  def apply(block: Block, raGroup: RaBinGroup[RaReservation]): Option[Seq[Block]] =
//    apply[RaReservation](block, raGroup.bins, _.remaining(block.obs.conditions))

//  def apply(block: Block, decGroup: DecBinGroup[BoundedTime]): Option[Seq[Block]] =
//    apply[DecBin[BoundedTime]](block, decGroup.bins, _.binValue.remaining)

  def apply[T](block: Block, bins: Seq[T], remTime: T => Time): Option[Seq[Block]] =
    new TooBlocks(block, bins, remTime).seq


  private class TooBlocks[T](val block: Block, val bins: Seq[T], val remTime: T => Time) {

    /**
     * Returns a sequence of smaller blocks (one for each member of the
     * RaBinGroup) wrapped in a Some.  Each block is adjusted to contain the
     * amount of time to reserve in the corresponding RA bin.  If the original
     * block cannot be distributed over the RA bins, None is returned.
     */
    val seq: Option[Seq[Block]] = distributeTime(block.time, bins) match {
        case Nil => None
        case s   => Some(s.map(block.updated(_)))
      }

    // Distribute time across the bins, returning a sequence of (Int, Time) which
    // identifies the amount of time to reserve in each bin by index.  If there
    // isn't enough time remaining in all the bins, then Nil is returned.
    private [this] def distributeTime(t: Time, bins: Seq[T]): List[Time] =
      distributeTime(t, zipAndSort(bins)).sorted(TooBlocks.indexOrdering).unzip._1

    // Zips with index and sorts according to remaining time in each RA bin.
    private[this] def zipAndSort(bins: Seq[T]): List[(T, Int)] =
      bins.zipWithIndex.sorted(TooBlocks.remainingTimeOrdering(remTime)).toList

    // Considering the bins in sorted order according to remaining time, build
    // the time distribution sequence.  Bins that can't hold an even distribution
    // of time are filled to their capacity.
    private[this] def distributeTime(t: Time, bins: List[(T, Int)]): List[(Time, Int)] =
      bins match {
        case Nil => Nil
        case head :: tail => {
          val rem   = remTime(head._1) // Time remaining in the current RA bin
          val index = head._2          // Index of the current (head) RA bin
          // Time in an even distribution across the remaining bins
          val evenDist = Time.hours(t.toHours.value / bins.size)

          if (rem < evenDist)
            distributeTime(t - rem, tail) match {
              case Nil => Nil
              case distTail => (rem, index) :: distTail
            }
          else
            bins.map(tup => (evenDist, tup._2))
        }
      }
  }
}