// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.syntax.all.*
import lucuma.core.math.Arc.Empty
import lucuma.core.math.Arc.Full
import lucuma.core.math.Arc.Partial
import monocle.Prism
import monocle.Lens
import monocle.Optional

/** An arc, either empty (0°) or full (360°) with no endpoints, or partial and clockwise with starting and ending angles. */
sealed trait Arc[A]:
  
  def contains(a: Angle): Boolean
  def contains(a: A): Boolean
  def containsAll(other: Arc[A]): Boolean
  def existsOverlap(other: Arc[A]): Boolean

  def isEmpty: Boolean =
    this match
      case Empty() => true
      case Full() => false
      case Partial(start, end) => false

  def nonEmpty: Boolean = 
    !isEmpty

  def isFull: Boolean =
    this match
      case Empty() => false
      case Full() => true
      case Partial(start, end) => false
    
  def nonFull: Boolean = 
    !isFull

  def isSingular: Boolean =
    this match
      case Empty() => false
      case Full() => false
      case Partial(start, end) => start == end
  
  def nonSingular: Boolean =
    !isSingular

    
object Arc extends ArcOptics:

  final case class Empty[A]() extends Arc[A]:
    def contains(a: Angle): Boolean = false
    def contains(a: A): Boolean = false
    def containsAll(other: Arc[A]): Boolean = other == this
    def existsOverlap(other: Arc[A]): Boolean = false

  object Empty:
    given [A]: Eq[Empty[A]] = Eq.fromUniversalEquals

  final case class Full[A]() extends Arc[A]:
    def contains(a: Angle): Boolean = true
    def contains(a: A): Boolean = true
    def containsAll(other: Arc[A]): Boolean = true
    def existsOverlap(other: Arc[A]): Boolean =
      other match
        case Empty() => false
        case Full() => true
        case Partial(start, end) => true
      
  object Full:
    given [A]: Eq[Full[A]] = Eq.fromUniversalEquals

  final case class Partial[A: Angular](start: A, end: A) extends Arc[A]:
    // In this scope we're comparing angles in [0..360)
    private given Order[Angle] = Angle.AngleOrder

    private lazy val startAngle = start.toAngle
    private lazy val endAngle = end.toAngle

    /** The clockwise angular separation between `start` and `end`. */
    def size: Angle =
      endAngle - startAngle

    /** Does `a` lie on this arc? */
    def contains(a: Angle): Boolean =
      (a - startAngle) <= size

    def contains(a: A): Boolean =
      contains(a.toAngle)

    /** Do all points on `other` lie on this arc? */
    def containsAll(other: Arc[A]): Boolean =
      other match
        case Empty() => true
        case Full() => false
        case other @ Partial(_, _) =>      
          contains(other.start) &&
          contains(other.end) &&
          other.startAngle - startAngle <= other.endAngle - startAngle

    /** Do any points on `other` lie on this arc? */
    def existsOverlap(other: Arc[A]): Boolean =
      other match
        case Empty() => false
        case Full() => true
        case other @ Partial(_, _) =>    
          contains(other.start) || contains(other.end) ||
          other.contains(start) || other.contains(end)

  object Partial extends PartialOptics:
    given [A: Eq]: Eq[Partial[A]] =
      Eq.by(a => (a.start, a.end))

  given [A: Eq]: Eq[Arc[A]] =
    Eq.instance:
      case (Empty(), Empty()) => true
      case (Full(), Full()) => true
      case (a @ Partial(a1, b1), b @ Partial(a2, b2)) => a === b
      case _ => false


trait PartialOptics:

  def start[A: Angular]: Lens[Arc.Partial[A], A] =
    Lens[Arc.Partial[A], A](_.start)(a => s => s.copy(start = a))

  def end[A: Angular]: Lens[Arc.Partial[A], A] =
    Lens[Arc.Partial[A], A](_.end)(a => s => s.copy(end = a))
  
trait ArcOptics:

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
