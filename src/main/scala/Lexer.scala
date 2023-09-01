import scala.annotation.tailrec

class Lexer(input: String, position: Int, line: Int, column: Int) {

  def this(input: String) = this(input, 1, 1, 1)

  private def hasNext: Boolean = {
    this.input.length >= this.position
  }

  private def getRest: String = {
    input.substring(position-1)
  }

  private def prettyPrintPosition(): Unit = {
    println(Console.BLUE +
      "position: " + this.position +
      " line: " + this.line +
      " column: " + this.column + Console.RESET)
  }

  private def advance(posBy: Int): Option[Lexer] = {
    def canAdvance: Boolean = this.input.length >= this.position + posBy - 1
    def lineBy: Int = this.input.substring(position, position + posBy-1).count(_ == '\n')
    def newColumn: Int = {
      val lastNewline = this.input.substring(0, position + posBy-1).lastIndexOf('\n')
      if (lastNewline == -1) this.column + posBy
      else this.position + posBy - lastNewline
    }
    if (canAdvance) Some(Lexer(input, position + posBy, line + lineBy, newColumn))
    else None
  }

  private def getNextToken: (Lexer, Token) = {
    if (this.hasNext) {
      this.getRest match {
        case str if str.startsWith("niech") => (this.advance("niech".length).get, Token(TokenType.Let, "niech"))
        case str if str.startsWith("#") =>
          val comment = this.getRest.takeWhile(_ != '\n')
          (this.advance(comment.length).get, Token(TokenType.Comment, comment))
        // comparison
        case str if str.startsWith("==") => (this.advance("==".length).get, Token(TokenType.Equal, "=="))
        case str if str.startsWith("!=") => (this.advance("!=".length).get, Token(TokenType.NotEqual, "!="))
        case str if str.startsWith("<=") => (this.advance("<=".length).get, Token(TokenType.LessThanOrEqual, "<="))
        case str if str.startsWith(">=") => (this.advance(">=".length).get, Token(TokenType.GreaterThanOrEqual, ">="))
        case str if str.startsWith("<") => (this.advance("<".length).get, Token(TokenType.LessThan, "<"))
        case str if str.startsWith(">") => (this.advance(">".length).get, Token(TokenType.GreaterThan, ">"))

        case str if str.startsWith("=") => (this.advance("=".length).get, Token(TokenType.Assign, "="))

        // logic
        case str if str.startsWith("prawda") => (this.advance("prawda".length).get, Token(TokenType.True, "prawda"))
        case str if str.startsWith("falsz") => (this.advance("falsz".length).get, Token(TokenType.False, "falsz"))
        case str if str.startsWith("fałsz") => (this.advance("fałsz".length).get, Token(TokenType.False, "fałsz"))
        case str if str.startsWith("i ") => (this.advance("i".length).get, Token(TokenType.And, "i"))
        case str if str.startsWith("lub ") => (this.advance("lub".length).get, Token(TokenType.Or, "lub"))
        case str if str.startsWith("!") => (this.advance("!".length).get, Token(TokenType.Not, "!"))

        case str if str.charAt(0).isWhitespace =>
          val spaces = this.getRest.takeWhile(_.isWhitespace)
          (this.advance(spaces.length).get, Token(TokenType.Whitespace, spaces))

        case str if str.startsWith(",") => (this.advance(",".length).get, Token(TokenType.Comma, ","))
        case str if str.startsWith(";") => (this.advance(";".length).get, Token(TokenType.Semicolon, ";"))

        // math
        case str if str.startsWith("+") => (this.advance("+".length).get, Token(TokenType.Plus, "+"))
        case str if str.startsWith("-") => (this.advance("-".length).get, Token(TokenType.Minus, "-"))
        case str if str.startsWith("*") => (this.advance("*".length).get, Token(TokenType.Star, "*"))
        case str if str.startsWith("/") => (this.advance("/".length).get, Token(TokenType.Slash, "/"))
        case str if str.startsWith("^") => (this.advance("/".length).get, Token(TokenType.Caret, "/"))

        // parentheses
        case str if str.startsWith("(") => (this.advance("(".length).get, Token(TokenType.LParen, "("))
        case str if str.startsWith(")") => (this.advance(")".length).get, Token(TokenType.RParen, ")"))
        case str if str.startsWith("{") => (this.advance("{".length).get, Token(TokenType.LBrace, "{"))
        case str if str.startsWith("}") => (this.advance("}".length).get, Token(TokenType.RBrace, "}"))

        // keywords
        case str if str.startsWith("jeżeli ") => (this.advance("jeżeli".length).get, Token(TokenType.If, "jeżeli"))
        case str if str.startsWith("jezeli ") => (this.advance("jezeli".length).get, Token(TokenType.If, "jezeli"))
        case str if str.startsWith("inaczej ") => (this.advance("inaczej".length).get, Token(TokenType.Else, "inaczej"))
        case str if str.startsWith("zwróć ") => (this.advance("zwróć".length).get, Token(TokenType.Return, "zwróć"))
        case str if str.startsWith("zwróc ") => (this.advance("zwróc".length).get, Token(TokenType.Return, "zwróc"))
        case str if str.startsWith("zwroć ") => (this.advance("zwroć".length).get, Token(TokenType.Return, "zwroć"))
        case str if str.startsWith("zwroc ") => (this.advance("zwroc".length).get, Token(TokenType.Return, "zwróc"))
        case str if str.startsWith("funkcja ") => (this.advance("funkcja".length).get, Token(TokenType.Function, "funkcja"))

        // literals
        case str if str.charAt(0).isLetter =>
          val ident = this.getRest.takeWhile(_.isLetterOrDigit)
          (this.advance(ident.length).get, Token(TokenType.Ident, ident))
        case str if str.charAt(0).isDigit => 
          val int = this.getRest.takeWhile(_.isDigit)
          (this.advance(int.length).get, Token(TokenType.Int, int))
        case _ =>
          val badline =
            this.input.substring(
              math.max(this.input.lastIndexOf('\n', this.position - 1) + 1, 0),
              this.input.indexOf('\n', this.position - 1)
            )
          val message =
            badline + "\n" +
            String.format("%" + this.column + "s", "^") +
            " Unexpected character: " + this.input.charAt(position) +
            " (line: " + this.line +
            " column: " + this.column + ")"
          (this.advance(1).get, Token(TokenType.Error, message))
      }
    }
    else (this, Token(TokenType.EOF, ""))
  }

  def getTokens(): List[Token] =
    @tailrec
    def aux(l: Lexer, acc: List[Token]): List[Token] =
      val nextToken = l.getNextToken
      nextToken match
        case (_, Token(TokenType.EOF, _)) => Token(TokenType.EOF, "") :: acc
        case (l, Token(TokenType.Error, msg)) => aux(l, acc)
        case (l, Token(TokenType.Whitespace, _)) => aux(l, acc)
        case (l, Token(TokenType.Newline, _)) => aux(l, acc)
        case (l, Token(TokenType.Comment, _)) => aux(l, acc)
        case (l, t) => aux(l, t :: acc)
    aux(this, Nil).reverse
}


