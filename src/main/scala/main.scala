import scala.annotation.tailrec

@main
def main(): Unit = {
  val source = scala.io.Source.fromFile("src/program.txt")
  val code = try source.mkString finally source.close()

  val l = Lexer(code)
  val tokens = l.getTokens(l)
}


