package xyz.hyperreal

import scala.util.parsing.input.Position

package object sysl {

  def perror(msg: String) = problem(null, msg)

  def problem(pos: Position, error: String) = {
    if (pos eq null)
      println(error)
    else
      println(s"${pos.line}: $error\n${pos.longString}")

    sys.exit(1)
  }

}
