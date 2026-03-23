package org.goldenport.cncf.cli

import org.goldenport.Consequence
import org.goldenport.cncf.action.Action
import org.goldenport.cncf.cli.help.CommandProtocolHelp
import org.goldenport.cncf.component.{ComponentCreate, ComponentOrigin}
import org.textus.useraccount.GeneratedDomainComponentLoader

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
        runtime
          .parseCommandArgs(subsystem, normalized, RunMode.Command)
          .flatMap { req =>
            req.component.flatMap(name => subsystem.components.find(_.name == name)) match {
              case None =>
                Consequence.failure(s"component not found: ${req.component.getOrElse("")}")
              case Some(component) =>
                component.logic.makeOperationRequest(req).flatMap {
                  case action: Action =>
                    val call = component.logic.createActionCall(action)
                    component.actionEngine.execute(call)
                  case _ =>
                    Consequence.failure("OperationRequest must be Action")
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
