package org.simplemodeling.textus.useraccount.cli

import org.goldenport.Consequence
import org.goldenport.cncf.action.Action
import org.goldenport.cncf.cli.{CncfRuntime, RunMode}
import org.goldenport.cncf.cli.help.CommandProtocolHelp
import org.goldenport.cncf.component.{ComponentCreate, ComponentOrigin}
import org.goldenport.cncf.job.JobId
import org.goldenport.protocol.Request
import org.goldenport.protocol.operation.OperationResponse
import org.simplemodeling.textus.useraccount.GeneratedDomainComponentLoader

/*
 * @since   May. 23, 2025
 *  version Mar. 23, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
object TextusUserAccountAwaitCommandMain {
  private val DefaultAwaitTimeoutMillis = 60000L

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

  private def _awaitTimeoutMillis: Long =
    sys.env
      .get("TEXTUS_AWAIT_TIMEOUT_MILLIS")
      .flatMap(s => scala.util.Try(s.toLong).toOption)
      .filter(_ > 0L)
      .getOrElse(DefaultAwaitTimeoutMillis)

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
              Consequence.failure(s"component not found: ${req.component.getOrElse("")}")
            case Some(component) =>
              component.logic.makeOperationRequest(req).flatMap {
                case action: Action =>
                  val call = component.logic.createActionCall(action)
                  component.actionEngine.execute(call).flatMap {
                    case scalar @ OperationResponse.Scalar(v) =>
                      JobId.parse(v.toString).toOption match {
                        case Some(jobid) =>
                          component.logic.awaitJobResult(jobid, timeoutMillis = _awaitTimeoutMillis)
                        case None => Consequence.success(scalar)
                      }
                    case other =>
                      Consequence.success(other)
                  }
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
