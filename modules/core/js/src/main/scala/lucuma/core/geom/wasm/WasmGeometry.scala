// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.geom.ShapeInterpreter

import scala.scalajs.js

/**
 * Entry point for the wasm geometry kernel. `load` initialises the `lucuma-geo-wasm` npm module,
 * checks its version against `CompatibleRange`, installs `WasmShapeInterpreter` as
 * `ShapeInterpreter.default` and returns it. Failures surface as the underlying error; callers
 * decide whether to stay on JTS.
 */
object WasmGeometry {

  /** npm semver range of `lucuma-geo-wasm` this facade was written against. */
  val CompatibleRange: String = "^0.1"

  private val CompatibleMajor = 0
  private val CompatibleMinor = 1

  /**
   * Loads the kernel letting the npm module locate its own `.wasm` file (browser bundlers rewrite
   * the `import.meta.url` lookup). Node cannot fetch `file:` URLs: use `loadFrom` there.
   */
  def load: IO[ShapeInterpreter] = loadFrom(js.undefined)

  /**
   * Loads the kernel from `moduleOrPath`: wasm bytes, a `URL`/`Response`, or a compiled
   * `WebAssembly.Module`, as accepted by the wasm-bindgen loader.
   */
  def loadFrom(moduleOrPath: js.UndefOr[js.Any]): IO[ShapeInterpreter] = {
    val opts: js.UndefOr[js.Object] =
      moduleOrPath.map(m => js.Dynamic.literal(module_or_path = m))
    for {
      x <- IO.fromPromise(IO(LucumaGeoWasm.init(opts)))
      v <- IO(LucumaGeoWasm.version())
      _ <- IO.raiseUnless(isCompatible(v))(
             new IllegalStateException(
               s"lucuma-geo-wasm $v is not compatible with this lucuma-core facade (requires $CompatibleRange)"
             )
           )
      _ <- IO {
             WasmShapeInterpreter.markLoaded(x)
             ShapeInterpreter.default = WasmShapeInterpreter
           }
    } yield WasmShapeInterpreter
  }

  /** Caret-range check: same major, and for major 0 the same minor too. */
  private[wasm] def isCompatible(version: String): Boolean =
    version.split("[.\\-+]").toList match {
      case maj :: min :: _ =>
        (maj.toIntOption, min.toIntOption).tupled.exists { (a, b) =>
          a === CompatibleMajor && (if (a === 0) b === CompatibleMinor else b >= CompatibleMinor)
        }
      case _               => false
    }
}
