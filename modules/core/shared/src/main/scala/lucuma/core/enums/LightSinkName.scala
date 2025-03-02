// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for SF Sink names.
 * @group Enumerations (Generated)
 */
enum LightSinkName(val tag: String, val name: String) derives Enumerated:
  /** @group Constructors */ case Gmos extends LightSinkName("Gmos", "gmos")
  /** @group Constructors */ case Niri_f6 extends LightSinkName("Niri_f6", "nirif6p")
  /** @group Constructors */ case Niri_f14 extends LightSinkName("Niri_f14", "nirif14p")
  /** @group Constructors */ case Niri_f32 extends LightSinkName("Niri_f32", "nirif32p")
  /** @group Constructors */ case Ac extends LightSinkName("Ac", "ac")
  /** @group Constructors */ case Hr extends LightSinkName("Hr", "hr")
  /** @group Constructors */ case Nifs extends LightSinkName("Nifs", "nifs")
  /** @group Constructors */ case Gmos_Ifu extends LightSinkName("Gmos_Ifu", "gmosifu")
  /** @group Constructors */ case Gnirs extends LightSinkName("Gnirs", "gnirs")
  /** @group Constructors */ case Visitor extends LightSinkName("Visitor", "visitor")
  /** @group Constructors */ case F2 extends LightSinkName("F2", "f2")
  /** @group Constructors */ case Gsaoi extends LightSinkName("Gsaoi", "gsaoi")
  /** @group Constructors */ case Phoenix extends LightSinkName("Phoenix", "phoenix")
  /** @group Constructors */ case Gpi extends LightSinkName("Gpi", "gpi")
  /** @group Constructors */ case Ghost extends LightSinkName("Ghost", "ghost")
  /** @group Constructors */ case Igrins2 extends LightSinkName("Igrins2", "igrins2")
