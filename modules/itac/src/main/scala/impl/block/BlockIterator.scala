// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.block

import cats.syntax.all.*
import edu.gemini.tac.qengine.api.queue.time.TimeAccountingCategoryTime
import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.Enumerated
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import BlockIterator.IMap

/**
 * An immutable iterator that can be used to generate time blocks across all
 * TimeAccountingCategorys and proposals.  It combines single TimeAccountingCategory iterators, slicing the
 * time across TimeAccountingCategorys according to a provide sequence and map of TimeAccountingCategory
 * time quanta.
 */
trait BlockIterator {
  private val LOGGER : Logger = LoggerFactory.getLogger("edu.gemini.itac")

  /**
   * Maps a TimeAccountingCategory to the length of its time quantum.
   */
  val quantaMap: TimeAccountingCategoryTime

  /**
   * The remaining sequence of TimeAccountingCategory time quanta.  As the iterator progresses,
   * the sequence advances.
   */
  val seq: Seq[TimeAccountingCategory]

  /**
   * Map of TimeAccountingCategory to TimeAccountingCategoryTimeBlockIterator.  As the iterator progresses,
   * the TimeAccountingCategoryTimeBlockIterators are advanced.
   */
  val iterMap: IMap

  /**
   * The time remaining in the current time quantum.
   */
  val remTime: Time

  /**
   * Computes the list of remaining proposals in the iterator.
   */
  def remPropList: List[Proposal] =
    Enumerated[TimeAccountingCategory].all.flatMap(p => iterMap(p).remainingProposals)

  /**
   *  The TimeAccountingCategory that occupies the current time quantum.
   */
  def currentTimeAccountingCategory: TimeAccountingCategory = seq.head

  def isStartOf(prop: Proposal): Boolean =
    (currentTimeAccountingCategory == prop.ntac.TimeAccountingCategory) &&
    iterMap(currentTimeAccountingCategory).isStartOf(prop)

  /**
   * Whether this iterator will produce any time blocks.  This method should
   * be called before calling any other methods.  If it returns
   * <code>false</code>, the result of using the other methods is not
   * specified.
   */
  def hasNext: Boolean = iterMap(currentTimeAccountingCategory).hasNext

  /**
   * Generates the next TimeBlock and a new iterator configured to produce the
   * remaining blocks.
   */
  def next(activeList : Proposal=>List[ItacObservation]) : (Block, BlockIterator) = {
    // Advance the TimeAccountingCategory time block iterator.  Use at most remTime time.
    // This may cause the iterator to generate a block for part of an
    // observation, which is fine.
    val (block, iter) = iterMap(currentTimeAccountingCategory).next(remTime, activeList)

    // Return the block and advance this iterator.  This may move to another
    // observation in the same time quantum or, if we've reached the end of the
    // quantum, it will advance to the next TimeAccountingCategory in the sequence.  Use the
    // updated map of TimeAccountingCategory time block iterators.
    (block, advance(block.time, iterMap.updated(currentTimeAccountingCategory, iter)))
  }

  /**
   * Extracts all the TimeBlocks from this iterator into a single list.
   * This method is mostly intended for testing support since it is not
   * tail recursive and could be expensive for lengthy sequences.
   */
  def toList(activeList : Proposal=>List[ItacObservation]) : List[Block] =
    if (!hasNext) Nil else next(activeList) match { case (b, it) => b :: it.toList(activeList) }

  /**
   * Skips the proposal that would be generated in the next TimeBlock.
   */
  def skip(activeList : Proposal=>List[ItacObservation]): BlockIterator = {
    val TimeAccountingCategoryIter = iterMap(currentTimeAccountingCategory).skip(activeList)
    val m = iterMap.updated(currentTimeAccountingCategory, TimeAccountingCategoryIter)
    if (TimeAccountingCategoryIter.hasNext) mkIterator(seq, remTime, m) else advanceTimeAccountingCategory(m)
  }

  private def advance(t: Time, m: IMap): BlockIterator =
    if ((remTime > t) && m(currentTimeAccountingCategory).hasNext) mkIterator(seq, remTime - t, m)
    else advanceTimeAccountingCategory(m)

  private def advanceTimeAccountingCategory(m: IMap): BlockIterator =
    advanceTimeAccountingCategory(seq.tail, m)

  private def advanceTimeAccountingCategory(s: Seq[TimeAccountingCategory], blockIteratorByTimeAccountingCategory: IMap, remaining: Set[TimeAccountingCategory] = BlockIterator.validTimeAccountingCategories(quantaMap)): BlockIterator = {
    if (remaining.isEmpty || s.isEmpty){
      //QueueCalculationLog.logger.log(Level.trace, "BlockIterator.empty()")
      LOGGER.debug("""<Event source="BlockIterator" event="Empty"/>""".toString())
      BlockIterator.Empty
    } else {
      val hasNext1 = blockIteratorByTimeAccountingCategory(s.head).hasNext
      val hasQuantaTime = !quantaMap(s.head).isZero
      if (hasNext1 && hasQuantaTime) {
        mkIterator(s, quantaMap(s.head), blockIteratorByTimeAccountingCategory)
      } else {
        val moreSeq = s.tail
        moreSeq.isEmpty match {
          case true => LOGGER.debug("End of sequence")
          case false => {
            // val nextTimeAccountingCategory = moreSeq.head
            // LOGGER.debug(<Event source="BlockIterator" event="advanceTimeAccountingCategory">
            //   {nextTimeAccountingCategory.fullName}
            // </Event>.toString)
          }
        }
        //QueueCalculationLog.logger.log(Level.trace, (<Event source="BlockIterator" event="advanceTimeAccountingCategory">{s.head.fullName}</Event>).toString)
        advanceTimeAccountingCategory(moreSeq, blockIteratorByTimeAccountingCategory, remaining - s.head)
      }
    }
  }

  protected def mkIterator(TimeAccountingCategorySeq: Seq[TimeAccountingCategory], t: Time, iterMap: IMap): BlockIterator
}

object BlockIterator {
  type IMap = Map[TimeAccountingCategory, TimeAccountingCategoryBlockIterator]

  object Empty extends BlockIterator {
    val quantaMap: TimeAccountingCategoryTime = TimeAccountingCategoryTime.empty
    val seq: Seq[TimeAccountingCategory] = Seq.empty
    val remTime: Time = Time.Zero
    val iterMap: IMap = Map.empty

    override def isStartOf(prop: Proposal): Boolean = false
    override def remPropList: List[Proposal] = Nil
    override def hasNext: Boolean = false
    def mkIterator(s: Seq[TimeAccountingCategory], t: Time, m: IMap) = this
  }

  private class BlockIteratorImpl(
          val quantaMap: TimeAccountingCategoryTime,
          val seq: Seq[TimeAccountingCategory],
          val remTime: Time,
          val iterMap: IMap) extends BlockIterator {

    def mkIterator(s: Seq[TimeAccountingCategory], t: Time, m: IMap) = {
      LoggerFactory.getLogger("edu.gemini.itac").debug("BlockIterator: " + seq.head + " remTime " + remTime)
      //QueueCalculationLog.logger.log(Level.trace, (<Event source="BlockIterator" event="mkIterator">{s.head.fullName}</Event>).toString)
      new BlockIteratorImpl(quantaMap, s, t, m)
    }
  }

  private def genIterMap(m: Map[TimeAccountingCategory, List[Proposal]], activeList : Proposal=>List[ItacObservation]): IMap =
    Enumerated[TimeAccountingCategory].all.map(p => p -> m.get(p).orEmpty).toMap.map { case (k, v) => (k, TimeAccountingCategoryBlockIterator.apply(v, activeList)) }

  // Finds the first TimeAccountingCategory that has a non-zero time quantum and a proposal
  // list and returns the sequence advanced to that TimeAccountingCategory and the time in its
  // time quantum.
  private def init(qMap: TimeAccountingCategoryTime, iMap: IMap, TimeAccountingCategorySeq: Seq[TimeAccountingCategory], remaining: Set[TimeAccountingCategory]): (Seq[TimeAccountingCategory], Time) =
    if (remaining.isEmpty || TimeAccountingCategorySeq.isEmpty)
      (Seq.empty, Time.Zero)
    else if (!qMap(TimeAccountingCategorySeq.head).isZero && iMap(TimeAccountingCategorySeq.head).hasNext)
      (TimeAccountingCategorySeq, qMap(TimeAccountingCategorySeq.head))
    else
      init(qMap, iMap, TimeAccountingCategorySeq.tail, remaining - TimeAccountingCategorySeq.head)

  // Calculates an initial set of valid TimeAccountingCategorys.  It trims any TimeAccountingCategorys
  // without a time quanta.  These TimeAccountingCategorys should not appear in the sequence.
  private def validTimeAccountingCategories(quantaMap: TimeAccountingCategoryTime): Set[TimeAccountingCategory] =
    Enumerated[TimeAccountingCategory].all.filter(!quantaMap(_).isZero).toSet

  /**
   * Constructs the TimeBlockIterator for the appropriate queue band category,
   * using the time quanta indicated in the quantaMap, the sorted proposals in
   * the propLists, and the provided sequence of TimeAccountingCategorys.
   *
   * <p>The TimeAccountingCategory sequence can be finite but an infinite sequence is expected
   * in order to be able to generate time blocks for all the proposals.
   */
  def apply(quantaMap: TimeAccountingCategoryTime, seq: Seq[TimeAccountingCategory], propLists: Map[TimeAccountingCategory, List[Proposal]], activeList : Proposal=>List[ItacObservation]): BlockIterator = {

    // Filter `quantaMap` to retain entries only for relevant TimeAccountingCategorys; i.e., those who have
    // proposals. Failure to do this can lead to nontermination in `advanceTimeAccountingCategory`.
    val relevant   = propLists.toList.collect { case (p, _ :: _) => p } .toSet
    val quantaMapʹ = quantaMap.filter(relevant)

    val iterMap = genIterMap(propLists, activeList)

    init(quantaMapʹ, iterMap, seq, validTimeAccountingCategories(quantaMapʹ)) match {
      case (s, _) if s.isEmpty => Empty
      case (timeAccountingCategorySeq, remainingTime) => {
        new BlockIteratorImpl(quantaMapʹ, timeAccountingCategorySeq, remainingTime, iterMap)
      }
    }
  }
}
