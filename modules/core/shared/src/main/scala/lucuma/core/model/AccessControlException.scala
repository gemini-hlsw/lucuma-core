// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

final case class AccessControlException(displayName: String, id: User.Id, role: Role, access: Access) extends
  Exception(s"$displayName (User ${id.value}, $role) does not have required access $access.")
