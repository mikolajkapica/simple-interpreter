import scala.annotation.tailrec

@main
def main(): Unit = {
  def repl: Unit = {
    val source = scala.io.Source.fromFile("src/program.txt")
    val code = try source.mkString finally source.close()
    code.split("\n") foreach { line =>
      if (line != "") {
        print(line + " = ")
        val tokens = Lexer(line).getTokens()
        val ast = Parser().getAst(tokens)
        Evaluator().eval(ast)
      }
    }
  }
  repl
}


def prettyPrintAst(ast: List[Node]) = {
  def prettyPrintAstNode(node: Node, indent: Int): Unit = {
    println("  " * indent + node.nodeType + " " + node.literal)
    node.children.foreach(prettyPrintAstNode(_, indent + 1))
  }
  ast.foreach(prettyPrintAstNode(_, 0))

}
