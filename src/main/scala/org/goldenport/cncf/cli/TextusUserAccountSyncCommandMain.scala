package org.goldenport.cncf.cli

import domain.DomainComponent
import org.goldenport.Consequence
import org.goldenport.cncf.action.Action
import org.goldenport.cncf.cli.help.CommandProtocolHelp
import org.goldenport.cncf.component.{ComponentCreate, ComponentOrigin}

object TextusUserAccountSyncCommandMain {
  def main(args: Array[String]): Unit = {
    val normalized = CommandProtocolHelp.normalizeArgs(args) match {
      case Left(code) =>
        sys.exit(code)
      case Right(xs) =>
        xs
    }

    val runtime = new CncfRuntime()
    val result = runtime
      .initializeForEmbedding(
        modeHint = Some(RunMode.Command),
        extraComponents = subsystem =>
          DomainComponent.Factory().create(ComponentCreate(subsystem, ComponentOrigin.Main))
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
