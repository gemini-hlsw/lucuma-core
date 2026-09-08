// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.typedarray.Float64Array

/**
 * Facade over the `lucuma-geo-wasm` npm package (Rust `geo` compiled with wasm-bindgen, target
 * `web`). Geometries live in a wasm-side arena addressed by integer handles; coordinates are µas
 * with x = -p, y = q, the same convention as `lucuma.core.geom.jts`.
 */
@js.native
@JSImport("lucuma-geo-wasm", JSImport.Namespace)
private[wasm] object LucumaGeoWasm extends js.Object {

  /** wasm-bindgen loader. `opts.module_or_path` may hold bytes, a URL or a compiled module. */
  @JSName("default")
  def init(opts: js.UndefOr[js.Object] = js.native): js.Promise[js.Any] = js.native

  def version(): String = js.native

  def empty_new(): Int                                                                     = js.native
  def poly_new(coords: Float64Array): Int                                                  = js.native
  def rect_new(x0: Double, y0: Double, x1: Double, y1: Double): Int                        = js.native
  def ellipse_new(x0: Double, y0: Double, x1: Double, y1: Double, npts: Int): Int          = js.native
  def arc_new(x0: Double, y0: Double, x1: Double, y1: Double, start: Double, extent: Double, npts: Int): Int =
    js.native

  /** `kind`: 0 intersection, 1 union, 2 difference. */
  def op(kind: Int, a: Int, b: Int): Int = js.native
  def affine(h: Int, m00: Double, m01: Double, m02: Double, m10: Double, m11: Double, m12: Double): Int =
    js.native

  def area(h: Int): Double = js.native

  /** `[minx, miny, maxx, maxy]`, all NaN when empty. */
  def bbox(h: Int): Float64Array = js.native

  /** Flat x,y coordinates of every exterior ring. */
  def coords(h: Int): Float64Array = js.native

  def contains_point(h: Int, x: Double, y: Double): Boolean = js.native
  def intersects(a: Int, b: Int): Boolean                   = js.native

  def free(h: Int): Unit = js.native
  def live(): Int        = js.native
}
