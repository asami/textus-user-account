package org.simplemodeling.textus.useraccount

import org.goldenport.cncf.CncfVersion
import org.goldenport.cncf.component.Component

/*
 * @since   May. 22, 2025
 *  version Mar. 23, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
object UserAccountComponentMain:
  def runtimeVersion: String = CncfVersion.current

  def createComponent(): Component =
    GeneratedDomainComponentLoader.createStandalone()
