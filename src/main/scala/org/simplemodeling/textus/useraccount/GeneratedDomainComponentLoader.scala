package org.simplemodeling.textus.useraccount

import org.goldenport.cncf.component.{Component, ComponentCreate}

/*
 * @since   May. 23, 2025
 * @version Mar. 23, 2026
 * @author  ASAMI, Tomoharu
 */
object GeneratedDomainComponentLoader:
  def create(componentCreate: ComponentCreate): Seq[Component] =
    ComponentFactory.create(componentCreate)

  def createStandalone(): Component =
    ComponentFactory.createStandalone()
