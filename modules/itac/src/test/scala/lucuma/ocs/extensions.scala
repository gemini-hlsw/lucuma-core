// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import java.io.File
import java.io.FileReader
import scala.xml.XML
import scala.xml.Elem
import lucuma.core.util.Enumerated
import lucuma.core.model.CloudExtinction
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import cats.mtl.Raise
import cats.syntax.all.*
import cats.Applicative
import lucuma.core.model.ImageQuality
import cats.mtl.Handle

extension (xml: XML.type) def load(f: File): Elem =
    val r = new FileReader(f)
    try XML.load(r) finally r.close()

extension (self: CloudExtinction.Preset.type)(using DummyImplicit)
  def ofPercentile(p: Double): CloudExtinction.Preset =
    Enumerated[CloudExtinction.Preset].all.minBy: ce =>
      math.abs(ce.percentile.toPercent.toDouble - p)

extension [F[_]: Applicative](self: CloudExtinction.Preset.type)(using R: Raise[F, String])
  def fromOcs(of: String): F[CloudExtinction.Preset] =
    def closestToPercentile(p: Double): CloudExtinction.Preset =
      Enumerated[CloudExtinction.Preset].all.minBy: ce =>
        math.abs(ce.percentile.toPercent.toDouble - p)
    of match
      case "70%/Cirrus" => closestToPercentile(70).pure
      case _ => R.raise(s"can't parse CloudExtinction.Preset: $of")

extension [F[_]: Applicative](self: SkyBackground.type)(using R: Raise[F, String])
  def fromOcs(of: String): F[SkyBackground] =
    of match
      case "Any/Bright" => SkyBackground.Bright.pure
      case "80%/Grey" => SkyBackground.Gray.pure
      case _ => R.raise(s"can't parse SkyBackground: $of")

extension [F[_]: Applicative](self: WaterVapor.type)(using R: Raise[F, String])
  def fromOcs(of: String): F[WaterVapor] =
    of match
      case "Any" => WaterVapor.Wet.pure
      case _ => R.raise(s"can't parse WaterVapor: $of")

extension [F[_]: Applicative](self: ImageQuality.Preset.type)(using R: Raise[F, String])
  def fromOcs(of: String): F[ImageQuality.Preset] =
    of match
      case "85%/Poor" => ImageQuality.Preset.TwoPointZero.pure // ???
      case "70%/Good" => ImageQuality.Preset.OnePointFive.pure // ???
      case _ => R.raise(s"can't parse ImageQuality: $of")

extension [F[_]: Applicative, K, V](self: Map[K, V])(using R: Raise[F, String])
  def getF(k: K): F[V] =
    self.get(k) match
      case Some(v) => v.pure
      case None => R.raise(s"Can't find value for key $k")
        
extension [F[_], A](self: F[A])(using H: Handle[F, String])
  infix def orElse(other: F[A]): F[A] =
    H.handleWith(self): 
      (_: String) => other