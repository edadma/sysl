package xyz.hyperreal.sysl

object CodeGenerator {

  def apply(as: List[SourceAST]) =
    compile( as.head )

  def compile( src: SourceAST ) =
    src.stmts map compile

  def compile( stmt: StatementAST ) =
    stmt match {
      case DeclarationBlockAST
    }

}
