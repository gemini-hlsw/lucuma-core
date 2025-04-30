// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data

import cats.Functor
import cats.kernel.Eq
import cats.kernel.Monoid
import cats.syntax.monoid.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Site

/** A total map from Site to A. */
final case class PerSite[+A](gn: A, gs: A) extends (Site => A):

  /** PerSite is a total function from Site to A. Alias for `forSite`. */
  def apply(site: Site): A =
    forSite(site)

  /** Lower to a pair. */
  def toPair: (A, A) =
    (gn, gs)

  /** Select the value for the given site. Alias for `apply`. */
  def forSite(site: Site): A =
    site match
      case Site.GN => gn
      case Site.GS => gs    

  /** PerSite is a functor. */
  def map[B](f: A => B): PerSite[B] =
    PerSite(f(gn), f(gs))

  /** Collapse values into monoid B. */
  def fold[B: Monoid](fgn: A => B, fgs: A => B): B =
    fgn(gn) |+| fgs(gs)

  /** Examine each (Site, A) pair and fold into monoid B. */
  def foldWithSite[B: Monoid](f: (Site, A) => B): B =
    f(Site.GN, gn) |+| f(Site.GS, gs)      

  /** Select sites where the given predicate holds. */
  def filter(f: A => Boolean): Set[Site] =
    foldWithSite: (s, a) =>
      if f(a) then Set(s) else Set.empty

object PerSite:

  /** Construct a PerSite filled with the zeros of monoid A. */
  def empty[A](using M: Monoid[A]): PerSite[A] =
    PerSite(M.empty, M.empty)

  /** Construct a PerSite with a constant value. */
  def const[A](a: A): PerSite[A] =
    unfold(_ => a)
  
  /** Construct a PerSite from a function. */
  def unfold[A](f: Site => A): PerSite[A] =
    PerSite(f(Site.GN), f(Site.GS))

  /** PerSite is a covariant functor. */
  given Functor[PerSite] with
    def map[A,B](fa: PerSite[A])(f: A => B): PerSite[B] = fa.map(f)

  /** PerSite is a monoid if A is a monoid. */
  given [A](using M: Monoid[A]): Monoid[PerSite[A]] with
    def empty: PerSite[A] = PerSite(M.empty, M.empty)
    def combine(x: PerSite[A], y: PerSite[A]): PerSite[A] =
      PerSite(x.gn |+| y.gn, x.gs |+| y.gs)

  /** PerSite is Eq if A is Eq. */
  given [A: Eq]: Eq[PerSite[A]] =
    Eq.by(_.toPair)

  /** PerSite is encodable to Json if A is encodable. */      
  given [A: Encoder]: Encoder[PerSite[A]] = ps =>
    Json.obj(
      "gn" -> ps.gn.asJson,
      "gs" -> ps.gs.asJson,
    )

  /** PerSite is decodable from Json if A is decodable. */      
  given [A: Decoder]: Decoder[PerSite[A]] = hc =>
    for
      gn <- hc.downField("gn").as[A]
      gs <- hc.downField("gs").as[A]
    yield PerSite(gn, gs)



case class X()

class Y extends X