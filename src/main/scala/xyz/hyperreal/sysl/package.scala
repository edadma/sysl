package xyz.hyperreal

import scala.util.parsing.input.Position

package object sysl {

  def perror(msg: String): Nothing = problem(null, msg)

  def problem(pos: Position, error: String): Nothing = {
    if (pos eq null)
      println(error)
    else
      println(s"${pos.line}: $error\n${pos.longString}")

    sys.exit(1)
  }

  case class Const(value: Any, typ: Type)

}
