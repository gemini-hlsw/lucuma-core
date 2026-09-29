// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.wasm

import lucuma.core.geom.ShapeExpressionTests

// The shape expression laws, run against the kernel instead of JTS.
class WasmShapeExpressionSuite
    extends ShapeExpressionTests(using WasmShapeInterpreter)
    with WasmKernelFixture {
  // Overlay identities hold to the parity budget (1e-7 relative), not to JTS's 700 µas².
  override protected def overlayAreaTolerance(nominal: Long): Double =
    math.max(700.0, nominal.toDouble.abs * 1e-7)
}
