package org.textus.useraccount

import org.goldenport.cncf.CncfVersion
import domain.DomainComponent

/*
 * @since   May. 22, 2025
 * @version Mar. 22, 2026
 * @author  ASAMI, Tomoharu
 */
object UserAccountComponentMain:
  def runtimeVersion: String = CncfVersion.current

  def createComponent(): DomainComponent =
    DomainComponent()
