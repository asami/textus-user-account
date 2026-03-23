package org.textus.useraccount

import scala.util.Try
import org.goldenport.cncf.component.{Component, ComponentCreate}

/*
 * @since   May. 23, 2025
 * @version Mar. 23, 2026
 * @author  ASAMI, Tomoharu
 */
object GeneratedDomainComponentLoader:
  private val DefaultCandidates = Vector(
    "textus.user.account.longpackagenametest.alpha.beta.gamma.delta.epsilon.DomainComponent",
    "domain.DomainComponent"
  )

  private def candidateClassNames: Vector[String] =
    val explicit =
      sys.env
        .get("TEXTUS_DOMAIN_COMPONENT_CLASS")
        .orElse(sys.props.get("textus.domain.component.class"))
        .toVector
    (explicit ++ DefaultCandidates).distinct

  def create(componentCreate: ComponentCreate): Seq[Component] =
    val attempts = candidateClassNames.map(_tryCreateWithFactory(_, componentCreate))
    attempts.collectFirst { case Right(components) => components }.getOrElse {
      val detail = attempts.collect { case Left(message) => message }.mkString("; ")
      throw new IllegalStateException(s"DomainComponent factory is not available: $detail")
    }

  def createStandalone(): Component =
    val attempts = candidateClassNames.map(_tryCreateStandalone)
    attempts.collectFirst { case Right(component) => component }.getOrElse {
      val detail = attempts.collect { case Left(message) => message }.mkString("; ")
      throw new IllegalStateException(s"DomainComponent class is not available: $detail")
    }

  private def _tryCreateWithFactory(
    className: String,
    componentCreate: ComponentCreate
  ): Either[String, Seq[Component]] =
    Try {
      val companionClass = Class.forName(s"${className}$$")
      val companion = companionClass.getField("MODULE$").get(null)
      val factoryClass = Class.forName(s"${className}$$Factory")
      val factory =
        factoryClass.getConstructors.find(_.getParameterCount == 0).map(_.newInstance()).orElse {
          factoryClass.getConstructors.find { c =>
            c.getParameterCount == 1 &&
            c.getParameterTypes.head.isAssignableFrom(companionClass)
          }.map(_.newInstance(companion))
        }.getOrElse {
          throw new NoSuchMethodException(s"Factory constructor is missing in ${factoryClass.getName}")
        }
      val createMethod = factory.getClass.getMethods.find { m =>
        m.getName == "create" && m.getParameterCount == 1
      }.getOrElse {
        throw new NoSuchMethodException(s"create(ComponentCreate) is missing in $className.Factory")
      }
      createMethod.invoke(factory, componentCreate) match {
        case xs: Seq[?] => xs.asInstanceOf[Seq[Component]]
        case c: Component => Seq(c)
        case other =>
          throw new IllegalStateException(
            s"unexpected return type from $className.Factory.create: ${other.getClass.getName}"
          )
      }
    }.toEither.left.map(e => _toErrorMessage(className, e))

  private def _tryCreateStandalone(className: String): Either[String, Component] =
    Try {
      Class.forName(className).getConstructor().newInstance().asInstanceOf[Component]
    }.toEither.left.map(e => _toErrorMessage(className, e))

  private def _toErrorMessage(className: String, e: Throwable): String =
    val message = Option(e.getMessage).getOrElse("")
    s"$className (${e.getClass.getName}: $message)"
