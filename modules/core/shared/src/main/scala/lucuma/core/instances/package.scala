// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

/**
  * Implicit instances for types defined outside of Gen. Each set of instances is provided as a
  * trait that can be extended and as a module whose members can be imported (preferred).
  */
package object instances {

  object all extends TreeMapInstances with TreeSetInstances

}
