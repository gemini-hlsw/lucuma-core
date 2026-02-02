// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.block

import cats.syntax.all.*
import edu.gemini.tac.qengine.api.queue.time.PartnerTime
import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.p1.Observation
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.Time
import lucuma.core.util.Enumerated
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import BlockIterator.IMap

/**
 * An immutable iterator that can be used to generate time blocks across all
 * partners and proposals.  It combines single partner iterators, slicing the
 * time across partners according to a provide sequence and map of partner
 * time quanta.
 */
trait BlockIterator {
  private val LOGGER : Logger = LoggerFactory.getLogger("edu.gemini.itac")

  /**
   * Maps a Partner to the length of its time quantum.
   */
  val quantaMap: PartnerTime

  /**
   * The remaining sequence of Partner time quanta.  As the iterator progresses,
   * the sequence advances.
   */
  val seq: Seq[Partner]

  /**
   * Map of Partner to PartnerTimeBlockIterator.  As the iterator progresses,
   * the PartnerTimeBlockIterators are advanced.
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
    Enumerated[Partner].all.flatMap(p => iterMap(p).remainingProposals)

  /**
   *  The partner that occupies the current time quantum.
   */
  def currentPartner: Partner = seq.head

  def isStartOf(prop: Proposal): Boolean =
    (currentPartner == prop.ntac.partner) &&
    iterMap(currentPartner).isStartOf(prop)

  /**
   * Whether this iterator will produce any time blocks.  This method should
   * be called before calling any other methods.  If it returns
   * <code>false</code>, the result of using the other methods is not
   * specified.
   */
  def hasNext: Boolean = iterMap(currentPartner).hasNext

  /**
   * Generates the next TimeBlock and a new iterator configured to produce the
   * remaining blocks.
   */
  def next(activeList : Proposal=>List[Observation]) : (Block, BlockIterator) = {
    // Advance the partner time block iterator.  Use at most remTime time.
    // This may cause the iterator to generate a block for part of an
    // observation, which is fine.
    val (block, iter) = iterMap(currentPartner).next(remTime, activeList)

    // Return the block and advance this iterator.  This may move to another
    // observation in the same time quantum or, if we've reached the end of the
    // quantum, it will advance to the next partner in the sequence.  Use the
    // updated map of partner time block iterators.
    (block, advance(block.time, iterMap.updated(currentPartner, iter)))
  }

  /**
   * Extracts all the TimeBlocks from this iterator into a single list.
   * This method is mostly intended for testing support since it is not
   * tail recursive and could be expensive for lengthy sequences.
   */
  def toList(activeList : Proposal=>List[Observation]) : List[Block] =
    if (!hasNext) Nil else next(activeList) match { case (b, it) => b :: it.toList(activeList) }

  /**
   * Skips the proposal that would be generated in the next TimeBlock.
   */
  def skip(activeList : Proposal=>List[Observation]): BlockIterator = {
    val partnerIter = iterMap(currentPartner).skip(activeList)
    val m = iterMap.updated(currentPartner, partnerIter)
    if (partnerIter.hasNext) mkIterator(seq, remTime, m) else advancePartner(m)
  }

  private def advance(t: Time, m: IMap): BlockIterator =
    if ((remTime > t) && m(currentPartner).hasNext) mkIterator(seq, remTime - t, m)
    else advancePartner(m)

  private def advancePartner(m: IMap): BlockIterator =
    advancePartner(seq.tail, m)

  private def advancePartner(s: Seq[Partner], blockIteratorByPartner: IMap, remaining: Set[Partner] = BlockIterator.validpartners(quantaMap)): BlockIterator = {
    if (remaining.isEmpty || s.isEmpty){
      //QueueCalculationLog.logger.log(Level.trace, "BlockIterator.empty()")
      LOGGER.debug("""<Event source="BlockIterator" event="Empty"/>""".toString())
      BlockIterator.Empty
    } else {
      val hasNext1 = blockIteratorByPartner(s.head).hasNext
      val hasQuantaTime = !quantaMap(s.head).isZero
      if (hasNext1 && hasQuantaTime) {
        mkIterator(s, quantaMap(s.head), blockIteratorByPartner)
      } else {
        val moreSeq = s.tail
        moreSeq.isEmpty match {
          case true => LOGGER.debug("End of sequence")
          case false => {
            // val nextPartner = moreSeq.head
            // LOGGER.debug(<Event source="BlockIterator" event="advancePartner">
            //   {nextPartner.fullName}
            // </Event>.toString)
          }
        }
        //QueueCalculationLog.logger.log(Level.trace, (<Event source="BlockIterator" event="advancePartner">{s.head.fullName}</Event>).toString)
        advancePartner(moreSeq, blockIteratorByPartner, remaining - s.head)
      }
    }
  }

  protected def mkIterator(partnerSeq: Seq[Partner], t: Time, iterMap: IMap): BlockIterator
}

object BlockIterator {
  type IMap = Map[Partner, PartnerBlockIterator]

  object Empty extends BlockIterator {
    val quantaMap: PartnerTime = PartnerTime.empty
    val seq: Seq[Partner] = Seq.empty
    val remTime: Time = Time.Zero
    val iterMap: IMap = Map.empty

    override def isStartOf(prop: Proposal): Boolean = false
    override def remPropList: List[Proposal] = Nil
    override def hasNext: Boolean = false
    def mkIterator(s: Seq[Partner], t: Time, m: IMap) = this
  }

  private class BlockIteratorImpl(
          val quantaMap: PartnerTime,
          val seq: Seq[Partner],
          val remTime: Time,
          val iterMap: IMap) extends BlockIterator {

    def mkIterator(s: Seq[Partner], t: Time, m: IMap) = {
      LoggerFactory.getLogger("edu.gemini.itac").debug("BlockIterator: " + seq.head + " remTime " + remTime)
      //QueueCalculationLog.logger.log(Level.trace, (<Event source="BlockIterator" event="mkIterator">{s.head.fullName}</Event>).toString)
      new BlockIteratorImpl(quantaMap, s, t, m)
    }
  }

  private def genIterMap(m: Map[Partner, List[Proposal]], activeList : Proposal=>List[Observation]): IMap =
    Enumerated[Partner].all.map(p => p -> m.get(p).orEmpty).toMap.map { case (k, v) => (k, PartnerBlockIterator.apply(v, activeList)) }

  // Finds the first partner that has a non-zero time quantum and a proposal
  // list and returns the sequence advanced to that partner and the time in its
  // time quantum.
  private def init(qMap: PartnerTime, iMap: IMap, partnerSeq: Seq[Partner], remaining: Set[Partner]): (Seq[Partner], Time) =
    if (remaining.isEmpty || partnerSeq.isEmpty)
      (Seq.empty, Time.Zero)
    else if (!qMap(partnerSeq.head).isZero && iMap(partnerSeq.head).hasNext)
      (partnerSeq, qMap(partnerSeq.head))
    else
      init(qMap, iMap, partnerSeq.tail, remaining - partnerSeq.head)

  // Calculates an initial set of valid partners.  It trims any partners
  // without a time quanta.  These partners should not appear in the sequence.
  private def validpartners(quantaMap: PartnerTime): Set[Partner] =
    Enumerated[Partner].all.filter(!quantaMap(_).isZero).toSet

  /**
   * Constructs the TimeBlockIterator for the appropriate queue band category,
   * using the time quanta indicated in the quantaMap, the sorted proposals in
   * the propLists, and the provided sequence of partners.
   *
   * <p>The partner sequence can be finite but an infinite sequence is expected
   * in order to be able to generate time blocks for all the proposals.
   */
  def apply(quantaMap: PartnerTime, seq: Seq[Partner], propLists: Map[Partner, List[Proposal]], activeList : Proposal=>List[Observation]): BlockIterator = {

    // Filter `quantaMap` to retain entries only for relevant partners; i.e., those who have
    // proposals. Failure to do this can lead to nontermination in `advancePartner`.
    val relevant   = propLists.toList.collect { case (p, _ :: _) => p } .toSet
    val quantaMapʹ = quantaMap.filter(relevant)

    val iterMap = genIterMap(propLists, activeList)

    init(quantaMapʹ, iterMap, seq, validpartners(quantaMapʹ)) match {
      case (s, _) if s.isEmpty => Empty
      case (partnerSeq, remainingTime) => {
        new BlockIteratorImpl(quantaMapʹ, partnerSeq, remainingTime, iterMap)
      }
    }
  }
}
