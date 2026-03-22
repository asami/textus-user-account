package org.textus.useraccount

import domain.DomainComponent
import org.goldenport.cncf.cli.CncfRuntime
import org.goldenport.cncf.component.{ComponentCreate, ComponentOrigin}

/*
 * @since   May. 23, 2025
 * @version Mar. 23, 2026
 * @author  ASAMI, Tomoharu
 */
object UserAccountCommandMain:
  def main(args: Array[String]): Unit =
    val code = CncfRuntime.runWithExtraComponents(
      args,
      subsystem => {
        DomainComponent.Factory().create(
          ComponentCreate(subsystem, ComponentOrigin.Main)
        )
      }
    )
    val waitMillis =
      sys.env.get("TEXTUS_COMMAND_POST_WAIT_MILLIS").flatMap(_.toLongOption).getOrElse(2000L)
    if (args.headOption.contains("command") && waitMillis > 0L) {
      Thread.sleep(waitMillis)
    }
    if (code != 0) {
      sys.exit(code)
    }
