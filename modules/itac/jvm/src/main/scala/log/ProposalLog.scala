// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.log.ProposalLog.Key
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.p1.QueueBand

import scala.collection.immutable.SortedSet

/**
 * A collection of log messages keyed by a combination of proposal id and
 * queue band time category.  The idea is that there may be multiple log
 * messages for a particular proposal, one for each stage of the queue
 * calculation.
 *
 * <p>This class wraps a SortedMap in order to simplify/hide the
 * (Proposal.Id, QueueBand.TimeCategory) keys and add a few features specific
 * to proposal logging.
 */
trait ProposalLog {
  import ProposalLog.Entry
  import ProposalLog.removeDuplicateKeys

  protected val log: List[Entry]

  def isDefinedAt(key: Key): Boolean = log.exists(_.key == key)
  def isDefinedAt(id: Proposal.Id, band: QueueBand): Boolean =
    isDefinedAt(Key(id, band))

  /**
   * Obtains the LogMessage associated with the given Key, or throws an
   * exception if not defined.  Use the get method if the key is possibly not
   * defined.
   */
  def apply(key: Key): LogMessage = get(key).get

  /**
   * Obtains the LogMessage associated with the given proposal id and queue
   * band time category, or throws an exception if not defined.  This is
   * a convenience method to hide the creation of the Key from the proposal
   * id and time category.
   */
  def apply(id: Proposal.Id, band: QueueBand): LogMessage = get(Key(id, band)).get

  def get(key: Key): Option[LogMessage] =
    log.find(_.key == key).map(_.msg)

  def get(id: Proposal.Id, band: QueueBand): Option[LogMessage] = get(Key(id, band))

  def getOrElse(key: Key, default: LogMessage): LogMessage =
    get(key).getOrElse(default)

  def getOrElse(id: Proposal.Id, band: QueueBand, default: LogMessage): LogMessage =
    getOrElse(Key(id, band), default)

  /**
   * Converts the log into a sorted list of Key and LogMessage where the Key
   * contains the proposal id and the time category.
   */
  def toMap: Map[Key, LogMessage] = log.reverse.map(_.toPair).toMap

  /**
   * Converts the log into a sorted list of entries that is ordered
   * by insertion order of the log messages.  Intermediate log messages are
   * included in the results, so a given (proposal id, time category) pair
   * may appear multiple times.
   */
  def toDetailList: List[Entry] = log.reverse

  /**
   * Gets a list of ProposalLog Entry, ordered by insertion, in which only the
   * final log message for a particular (proposal id, time category) key is
   * kept.  This is useful for displaying results to the user since it shows
   * the final order of events and hides any backtracking done by the algorithm.
   * In the full list, a given proposal may be accepted, backtracked over, and
   * later rejected.  This is likely to confuse the user.  Instead, with the
   * filtered list, they will see only the final rejection.
   */
  def toList: List[Entry] = removeDuplicateKeys(log)

  /**
   * Gets a list of (QueueBand.TimeCategory, LogMessage) pairs for all entries
   * related to the given proposal.
   */
  def toList(id: Proposal.Id): List[Entry] =
    removeDuplicateKeys(log.filter(_.key.id == id))

  /**
   * Gets all the proposal ids for proposals that have one or more log messages
   * in the ProposalLog.
   */
  def proposalIds: SortedSet[Proposal.Id] =
    log.foldLeft(SortedSet.empty[Proposal.Id]) {
      (s,e) => s + e.key.id
    }

  /**
   * Creates a new ProposalLog with the LogMessage associated with the given
   * key added or replaced with the provided message.
   */
  def updated(key: Key, msg: LogMessage): ProposalLog = mkProposalLog(Entry(key, msg) :: log)

  /**
   * Creates a new ProposalLog with the LogMessage associated with the given
   * proposal id and time category added or replaced with the provided
   * message.  This is a convenience method to obviate the need to explicitly
   * create a Key object from the proposal id and category.
   */
  def updated(id: Proposal.Id, band: QueueBand, msg: LogMessage): ProposalLog =
    updated(Key(id, band), msg)

  /**
   * Creates a new ProposalLog with entries for all the given proposals at the
   * specified time category.  The LogMessages are determined by the provided
   * function.
   */
  def updated(propList: List[Proposal], band: QueueBand, f: Proposal => LogMessage): ProposalLog =
    mkProposalLog(propList.foldLeft(log) {
      (lst, prop) => Entry(Key(prop.id, band), f(prop)) :: lst
    })

  /**
   * Creates a proposal log from a sorted map.
   */
  protected def mkProposalLog(l: List[Entry]): ProposalLog

  def |+|(other: ProposalLog): ProposalLog =
    mkProposalLog(log ++ other.log)

}

object ProposalLog {
  /**
   * A combination of proposal id and queue band time category that serves as
   * a key for looking up proposal log messages.
   */
  case class Key(id: Proposal.Id, band: QueueBand)
  object Key {
    implicit val OrderingKey: Ordering[Key] =
      Ordering.by(k => (k.id, k.band))
  }

  /**
   * ProposalLog entry, which groups a key (proposal id, time category) and
   * LogMessage.
   */
  case class Entry(key: Key, msg: LogMessage) {
    def toPair: Tuple2[Key, LogMessage] = (key, msg)
  }

  // Takes a reversed list of entries, filters it to remove duplicate keys
  // that represent intermediate log entries, reversing the list to insertion
  // order in the process.
  private def removeDuplicateKeys(reverseLst: List[Entry]): List[Entry] = {
    case class Res(lst: List[Entry], keys: Set[Key])
    val empty = Res(Nil, Set.empty)

    // Note: reverses the log entries as it goes leaving them in insertion
    // order and selecting only the final entry per key (which is the first
    // one to show up in the reversed list).
    reverseLst.foldLeft(empty) {
      (res, entry) =>
        if (res.keys.contains(entry.key))
          res // skip this one
        else

          Res(entry :: res.lst, res.keys + entry.key)
    }.lst
  }

  val Empty: ProposalLog = new ProposalLogImpl(List.empty)

  def apply(tups: (Proposal.Id, QueueBand, LogMessage)*): ProposalLog =
    new ProposalLogImpl(List(tups.map {
      case (id, band, msg) => Entry(Key(id, band), msg)
    }*).reverse)

  private class ProposalLogImpl(val log: List[Entry]) extends ProposalLog {
    protected def mkProposalLog(l: List[Entry]): ProposalLog = new ProposalLogImpl(l)
  }


}
