// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import lucuma.core.math.Offset

/**
  * Defines a box of offsets that sorround a shape
  *
  * The vertices are in the order indicated below:
  *
  * p1           p2
  * +------------+
  * |            |
  * |            |                      ^
  * |            |                      |   q
  * |            |                      |
  * |            |                      |
  * |            |                      |
  * +------------+              <-------+
  * p4           p3               p
  */
final case class BoundingOffsets(p1: Offset, p2: Offset, p3: Offset, p4: Offset)
