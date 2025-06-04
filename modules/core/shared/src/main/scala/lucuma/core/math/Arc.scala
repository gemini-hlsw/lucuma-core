// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.syntax.all.*
import lucuma.core.math.Arc.Empty
import lucuma.core.math.Arc.Full
import lucuma.core.math.Arc.Partial
import monocle.Lens
import monocle.Optional
import monocle.Prism

import scala.util.NotGiven

/** An arc, either empty (0°) or full (360°) with no endpoints, or partial and clockwise with starting and ending angles (inclusive). */
sealed trait Arc[A] {

  /** Does the given angle lie on this arc? */
  def contains(a: Angle): Boolean

  /** Does point `a` lie on this arc? */
  def contains(a: A): Boolean

  /** Do all points on `other` lie on this arc? */
  def containsAll(other: Arc[A]): Boolean

  /** Do there exist points that lie on both arcs? */
  def existsOverlap(other: Arc[A]): Boolean

  /** Does this arc contain no points? */
  def isEmpty: Boolean  = Arc.empty.getOption(this).isDefined

  /** Does this arc contain at least one point? */
  def nonEmpty: Boolean = !isEmpty

  /** Does this arc contain all points? */
  def isFull: Boolean = Arc.full.getOption(this).isDefined

  /** Do there existe points not on this arc? */
  def nonFull: Boolean = !isFull

  /** Does this arc contain a single point? */
  def isSingular: Boolean 

  /** Does this arc contain anything other than a single point? */
  def nonSingular: Boolean = !isSingular

}
    
object Arc extends ArcOptics {

  final case class Empty[A]() extends Arc[A]:
    def contains(a: Angle): Boolean = false
    def contains(a: A): Boolean = false
    def containsAll(other: Arc[A]): Boolean = other == this
    def existsOverlap(other: Arc[A]): Boolean = false
    def isSingular: Boolean = false

  object Empty:
    given [A]: Eq[Empty[A]] = Eq.fromUniversalEquals

  final case class Full[A]() extends Arc[A]:
    def contains(a: Angle): Boolean = true
    def contains(a: A): Boolean = true
    def containsAll(other: Arc[A]): Boolean = true
    def isSingular: Boolean = false
    def existsOverlap(other: Arc[A]): Boolean =
      other match
        case Empty() => false
        case Full() => true
        case Partial(start, end) => true
      
  object Full:
    given [A]: Eq[Full[A]] = Eq.fromUniversalEquals

  final case class Partial[A: Angular](start: A, end: A) extends Arc[A] {
    // In this scope we're comparing angles in [0..360)
    private given Order[Angle] = Angle.AngleOrder

    private lazy val startAngle = start.toAngle
    private lazy val endAngle = end.toAngle

    /** The clockwise angular separation between `start` and `end`. */
    def size: Angle =
      endAngle - startAngle

    def contains(a: Angle): Boolean =
      (a - startAngle) <= size

    def contains(a: A): Boolean =
      contains(a.toAngle)

    def containsAll(other: Arc[A]): Boolean =
      other match
        case Empty() => true
        case Full() => false
        case other @ Partial(_, _) =>      
          contains(other.start) &&
          contains(other.end) &&
          other.startAngle - startAngle <= other.endAngle - startAngle

    def existsOverlap(other: Arc[A]): Boolean =
      other match
        case Empty() => false
        case Full() => true
        case other @ Partial(_, _) =>    
          contains(other.start) || contains(other.end) ||
          other.contains(start) || other.contains(end)

    def isSingular: Boolean =
      start.toAngle === end.toAngle // or, equivalently, size === Angle0

  }
  
  object Partial:
    given [A: Eq]: Eq[Partial[A]] =
      Eq.by(a => (a.start, a.end))

    def start[A: Angular]: Lens[Arc.Partial[A], A] =
      Lens[Arc.Partial[A], A](_.start)(a => s => s.copy(start = a))
  
    def end[A: Angular]: Lens[Arc.Partial[A], A] =
      Lens[Arc.Partial[A], A](_.end)(a => s => s.copy(end = a))
    
  given [A: Eq](using NotGiven[Order[A]]): Eq[Arc[A]] =
    Eq.instance:
      case (Empty(), Empty()) => true
      case (Full(), Full()) => true
      case (a @ Partial(a1, b1), b @ Partial(a2, b2)) => a === b
      case _ => false

  given [A: Order]: Order[Arc[A]] =
    case (Empty(), Empty()) => 0
    case (Full(), Full()) => 0
    case (Arc.Empty(), _) => -1
    case (_, Arc.Empty()) =>  1
    case (Arc.Full(), _)  => 1
    case (_, Arc.Full())  => -1
    case (Arc.Partial(a1, b1), Arc.Partial(a2, b2)) => 
      (a1, b1) compare (a2, b2)

}

trait ArcOptics {

  def empty[A]: Prism[Arc[A], Arc.Empty[A]] =
    Prism.partial[Arc[A], Arc.Empty[A]] { case e @ Arc.Empty() => e }(a => a)

  def full[A]: Prism[Arc[A], Arc.Full[A]] =
    Prism.partial[Arc[A], Arc.Full[A]] { case e @ Arc.Full() => e }(a => a)

  def partial[A]: Prism[Arc[A], Arc.Partial[A]] =
    Prism.partial[Arc[A], Arc.Partial[A]] { case e @ Arc.Partial(_, _) => e }(a => a)
  
  def start[A: Angular]: Optional[Arc[A], A] =
    partial[A].andThen(Arc.Partial.start[A])

  def end[A: Angular]: Optional[Arc[A], A] =
    partial[A].andThen(Arc.Partial.end[A])

}