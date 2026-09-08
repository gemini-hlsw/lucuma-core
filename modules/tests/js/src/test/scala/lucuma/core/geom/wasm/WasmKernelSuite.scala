// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import lucuma.core.geom.ShapeInterpreter
import munit.AnyFixture
import munit.FunSuite

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:fs", JSImport.Namespace)
private[wasm] object NodeFs extends js.Object {
  def readFileSync(path: js.Any): js.typedarray.Uint8Array = js.native
}

/** Loads the kernel under Node, which cannot fetch the package's own `file:` URL. */
object WasmKernel {

  def wasmBytes: IO[js.typedarray.Uint8Array] = IO {
    val url = js.`import`.meta.asInstanceOf[js.Dynamic]
      .resolve("lucuma-geo-wasm/lucuma_geo_wasm_bg.wasm")
    NodeFs.readFileSync(js.Dynamic.newInstance(js.Dynamic.global.URL)(url))
  }

  def load: IO[ShapeInterpreter] = wasmBytes.flatMap(WasmGeometry.loadFrom(_))
}

/**
 * Mixin for suites that drive `WasmShapeInterpreter` explicitly: loads the kernel before the
 * suite, runs each (synchronous) test inside `scoped` and fails it if kernel handles leaked.
 */
trait WasmKernelSuite extends FunSuite {

  private val kernel = new AnyFixture[Unit]("lucuma-geo-wasm") {
    def apply(): Unit                = ()
    override def beforeAll(): Any    = WasmKernel.load.unsafeToFuture()
  }

  override def munitFixtures: Seq[AnyFixture[?]] = super.munitFixtures :+ kernel

  override def munitTestTransforms: List[TestTransform] =
    super.munitTestTransforms :+ new TestTransform(
      "wasm arena",
      test =>
        test.withBody { () =>
          val before = WasmShapeInterpreter.liveHandles
          val result = WasmShapeInterpreter.scoped(test.body())
          val after  = WasmShapeInterpreter.liveHandles
          assert(after <= before, s"${test.name}: leaked ${after - before} kernel handles")
          result
        }
    )
}
