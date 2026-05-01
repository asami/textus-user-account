package org.simplemodeling.textus.useraccount.cli

import org.goldenport.Consequence
import org.goldenport.cncf.action.Action
import org.goldenport.cncf.cli.{CncfRuntime, RunMode}
import org.goldenport.cncf.cli.help.CommandProtocolHelp
import org.goldenport.cncf.component.{ComponentCreate, ComponentOrigin}
import org.goldenport.protocol.Request
import org.simplemodeling.textus.useraccount.GeneratedDomainComponentLoader

/*
 * @since   May. 23, 2025
 *  version Mar. 23, 2026
 *  version Mar. 25, 2026
 * @version May.  1, 2026
 * @author  ASAMI, Tomoharu
 */
object TextusUserAccountSyncCommandMain {
  private def _splitRuntimeArgs(
    args: Array[String]
  ): (Array[String], Array[String]) = {
    val runtime = Vector.newBuilder[String]
    val command = Vector.newBuilder[String]
    var i = 0
    while (i < args.length) {
      val current = args(i)
      if (current.startsWith("--cncf.") || current.startsWith("-cncf.")) {
        runtime += current
        if (!current.contains("=") && i + 1 < args.length && !args(i + 1).startsWith("-")) {
          runtime += args(i + 1)
          i = i + 1
        }
      } else {
        command += current
      }
      i = i + 1
    }
    (runtime.result().toArray, command.result().toArray)
  }

  private def _parseCommandArgs(
    runtime: CncfRuntime,
    subsystem: org.goldenport.cncf.subsystem.Subsystem,
    args: Array[String]
  ): Consequence[Request] = {
    val method = runtime.getClass.getMethods.find { m =>
      m.getName == "parseCommandArgs" && m.getParameterCount == 3
    }.getOrElse {
      throw new NoSuchMethodException("CncfRuntime.parseCommandArgs is not available")
    }
    method
      .invoke(runtime, subsystem, args, RunMode.Command)
      .asInstanceOf[Consequence[Request]]
  }

  def main(args: Array[String]): Unit = {
    val (runtimeArgs, commandArgs) = _splitRuntimeArgs(args)
    val normalized = CommandProtocolHelp.normalizeArgs(commandArgs) match {
      case Left(code) =>
        sys.exit(code)
      case Right(xs) =>
        xs
    }

    val runtime = new CncfRuntime()
    val result = runtime
      .initializeForEmbedding(
        args = runtimeArgs,
        modeHint = Some(RunMode.Command),
        extraComponents = subsystem =>
          GeneratedDomainComponentLoader.create(
            ComponentCreate(subsystem, ComponentOrigin.Main)
          )
      )
      .flatMap { subsystem =>
        _parseCommandArgs(runtime, subsystem, normalized).flatMap { req =>
          req.component.flatMap(name => subsystem.components.find(_.name == name)) match {
            case None =>
              Consequence.componentNotFound(req.component.getOrElse(""))
            case Some(component) =>
              component.logic.makeOperationRequest(req).flatMap {
                case action: Action =>
                  val call = component.logic.createActionCall(action)
                  component.actionEngine.execute(call)
                case _ =>
                  Consequence.operationInvalid("OperationRequest", "OperationRequest must be Action")
              }
          }
        }
      }

    try {
      result match {
        case Consequence.Success(res) =>
          println(res.show)
          sys.exit(0)
        case Consequence.Failure(conclusion) =>
          println(conclusion.toRecord.toYamlString)
          sys.exit(1)
      }
    } finally {
      runtime.closeEmbedding()
    }
  }
}
