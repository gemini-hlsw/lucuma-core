// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import cats.Applicative
import cats.mtl.Handle
import cats.mtl.Raise
import cats.syntax.all.*
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.TooActivation
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.util.Enumerated

import java.io.File
import java.io.FileReader
import scala.xml.Elem
import scala.xml.XML

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
      case "Any" => closestToPercentile(100).pure
      case "80%/Cloudy" => closestToPercentile(80).pure
      case "70%/Cirrus" => closestToPercentile(70).pure
      case "50%/Clear" => closestToPercentile(50).pure
      case _ => R.raise(s"can't parse CloudExtinction.Preset: $of")

extension [F[_]: Applicative](self: SkyBackground.type)(using R: Raise[F, String])
  def fromOcs(of: String): F[SkyBackground] =
    of match
      case "Any/Bright" => SkyBackground.Bright.pure
      case "80%/Grey" => SkyBackground.Gray.pure
      case "50%/Dark" => SkyBackground.Dark.pure
      case "20%/Darkest" => SkyBackground.Darkest.pure
      case _ => R.raise(s"can't parse SkyBackground: $of")

extension [F[_]: Applicative](self: WaterVapor.type)(using R: Raise[F, String])
  def fromOcs(of: String): F[WaterVapor] =
    of match
      case "Any" => WaterVapor.Wet.pure
      case "80%/High" => WaterVapor.Median.pure
      case "50%/Median" => WaterVapor.Dry.pure
      case _ => R.raise(s"can't parse WaterVapor: $of")

extension [F[_]: Applicative](self: ImageQuality.Preset.type)(using R: Raise[F, String])
  def fromOcs(of: String): F[ImageQuality.Preset] =
    of match
      case "Any" => ImageQuality.Preset.TwoPointZero.pure // ???
      case "85%/Poor" => ImageQuality.Preset.OnePointFive.pure // ???
      case "70%/Good" => ImageQuality.Preset.OnePointTwo.pure // ???
      case "20%/Best" => ImageQuality.Preset.PointOne.pure // ???      
      case _ => R.raise(s"can't parse ImageQuality: $of")

extension [F[_]: Applicative](self: TooActivation.type)(using R: Raise[F, String])
  def fromOcs(of: String): F[TooActivation] =
    of match
      case "None" => TooActivation.None.pure
      case "Standard" => TooActivation.Standard.pure
      case "Rapid" => TooActivation.Rapid.pure
      case _ => R.raise(s"can't parse TooActivation: $of")

extension [F[_]: Applicative, K, V](self: Map[K, V])(using R: Raise[F, String])
  def getF(k: K): F[V] =
    self.get(k) match
      case Some(v) => v.pure
      case None => R.raise(s"Can't find value for key $k")
        
extension [F[_], A](self: F[A])(using H: Handle[F, String])
  infix def orElse(other: F[A]): F[A] =
    H.handleWith(self): 
      (_: String) => other