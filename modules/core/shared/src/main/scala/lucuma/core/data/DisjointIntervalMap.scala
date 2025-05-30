// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data

import cats.collections.Diet
import cats.collections.Discrete
import cats.collections.Range
import cats.kernel.CommutativeSemigroup
import cats.kernel.Eq
import cats.kernel.LowerBounded
import cats.kernel.Order
import cats.kernel.UpperBounded

/** A map from keys to interval sets, where the interval sets are disjoint and non-empty. */
sealed trait DisjointIntervalMap[K, V: Discrete: Order]:

  /** The underlying map. */
  def toMap: Map[K, Diet[V]]

  /** The key whose interval set includes `v`, if any. */
  def getKeyForValue(value: V)(using Order[V]): Option[K] =
    getKeyForRange(Range(value, value))

  /** The key whose interval set includes all of `range`, if any. */
  def getKeyForRange(range: Range[V])(using Order[V]): Option[K] =
    toMap.find(_._2.containsRange(range)).map(_._1)

  /** The sub-map that intersects the specified `range`. */
  def intersect(range: Range[V]): DisjointIntervalMap[K, V] =
    DisjointIntervalMap.unsafeFromMap:
      toMap
        .view
        .mapValues(_ & range)
        .filterNot(_._2.isEmpty)
        .toMap

  /** This map intersected pairwise with `other`. */
  infix def intersect(other: DisjointIntervalMap[K, V]): DisjointIntervalMap[K, V] =
    DisjointIntervalMap.unsafeFromMap:
      toMap
        .view
        .map: (k, d) =>
          k -> other.get(k).map(d & _).getOrElse(Diet.empty)
        .filterNot(_._2.isEmpty)
        .toMap

  /** The interval set for `key`, if any. */
  def get(key: K): Option[Diet[V]] =
    toMap.get(key)

  /**
   * A new `DisjointIntervalMap` where `value` is added to `k`'s interval set and
   * removed from its current interval set (if any).
   */
  def add(key: K, value: V): DisjointIntervalMap[K, V] =
    addRange(key, Range(value, value))

  /**
   * A new `DisjointIntervalMap` where `range` is added to `k`'s interval set and
   * removed from any other interval sets where it overlaps.
   */
  def addRange(key: K, range: Range[V]): DisjointIntervalMap[K, V] =
    DisjointIntervalMap.unsafeFromMap:
      toMap
        .view
        .mapValues(_.removeRange(range)) // remove range from existing sets
        .filterNot(_._2.isEmpty)         // remove entries with empty sets
        .toMap
        .updatedWith(key):
          case None    => Some(Diet.fromRange(range)) // new set if there wasn't one
          case Some(d) => Some(d.addRange(range))     // otherwise update

  /**
   * A new `DisjointIntervalMap` where `value` is removed from `k`'s interval set.
   */
  def remove(key: K, value: V): DisjointIntervalMap[K, V] =
    removeRange(key, Range(value, value))

  /**
   * A new `DisjointIntervalMap` where `range` is removed from `k`'s interval set.
   */
  def removeRange(key: K, range: Range[V]): DisjointIntervalMap[K, V] =
    DisjointIntervalMap.unsafeFromMap:
      toMap.updatedWith(key):
        case None => None
        case Some(d) => Some(d.removeRange(range)).filterNot(_.isEmpty)

  override def toString =
    import Order.catsKernelOrderingForOrder
    def tsd(d: Diet[V]): String =
       d.toIterator
        .toList
        .sortBy(_.start)
        .map: r =>
          val (a, b) = (r.start, r.end)
          if (a == b) then a.toString
          else s"$a..$b"
        .mkString("[", ",", "]")
    s"DisjointIntervalMap(${toMap.view.mapValues(tsd).toMap.toString})"

  override def equals(that: Any): Boolean =
    sys.error("Universal equality is not defined for DisjointIntervalMap.")

  override def hashCode(): Int =
    sys.error("Universal hashing is not defined for DisjointIntervalMap.")

object DisjointIntervalMap:

  // This is O(n^2) but we expect n to be pretty small ... :-\
  private def disjoint[V: Order](rs: List[Range[V]]): Boolean =
    rs match
      case Nil => true
      case h :: t => !t.exists(_.overlaps(h)) && disjoint(t)

  /** Construct a DisjointIntervalMap with no entries. */
  def empty[K, V: Discrete: Order]: DisjointIntervalMap[K, V] =
    unsafeFromMap(Map.empty)

  /** Construct a DisjointIntervalMap with a single entry whose interval set covers the entire range of `V`. */
  def full[K, V: LowerBounded: UpperBounded: Discrete: Order](key: K): DisjointIntervalMap[K, V] =
    one(key, Range(LowerBounded[V].minBound, UpperBounded[V].maxBound))

  /** Construct a DisjointIntervalMap with a single entry. */
  def one[K, V: Discrete: Order](key: K, range: Range[V]): DisjointIntervalMap[K, V] =
    unsafeFromMap(Map(key -> Diet.fromRange(range)))

  /** Construct a DisjointIntervalMap from the given map, throwing an error unless the interval sets are disjoint. */
  def unsafeFromMap[K, V: Discrete: Order](map: Map[K, Diet[V]]): DisjointIntervalMap[K, V] =
    fromMap(map).getOrElse:
      throw new AssertionError(s"DisjointIntervalMap: overlapping ranges found in $map")

  /** Construct a DisjointIntervalMap from the given map, if the interval sets are disjoint. */
  def fromMap[K, V: Discrete: Order](map: Map[K, Diet[V]]): Option[DisjointIntervalMap[K, V]] =
    Option.when(disjoint(map.values.flatMap(_.toIterator).toList)):
      new DisjointIntervalMap[K, V]:
        val toMap = map

  /** DisjointIntervalMap forms a commutative semigroup, with intersection as the combining operation. */
  given [K,V]: CommutativeSemigroup[DisjointIntervalMap[K,V]] =
    CommutativeSemigroup.instance(_ intersect _)

  /** DisjointIntervalMap is Eq if its key and values are Eq. */
  given [K, V: Eq]: Eq[DisjointIntervalMap[K, V]] =
    Eq.by(_.toMap)

