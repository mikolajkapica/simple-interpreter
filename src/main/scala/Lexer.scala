class Lexer(input: String, position: Int, line: Int, column: Int) {

  def this(input: String) = this(input, 0, 0, 0)

  def hasNext: Boolean = {
    this.input.length - 1 >= this.position
  }

  def prettyPrintPosition(): Unit = {
    println(Console.BLUE +
      "position: " + this.position +
      " line: " + this.line +
      " column: " + this.column + Console.RESET)
  }

  def advance(posBy: Int): Option[Lexer] = {
    def canAdvance: Boolean = this.input.length - 1 >= this.position + posBy - 1
    def lineBy: Int = this.input.substring(position, position + posBy).count(_ == '\n')
    //TODO: calculate newColumn
    def newColumn: Int = 0
    if (canAdvance) Some(Lexer(input, position + posBy, line + lineBy, newColumn))
    else None
  }

  def getRest: String = {
    input.substring(position)
  }

  // can i return a tuple? A: yes Q: how do i do that?
  // A: return a tuple
  def getNextToken: (Lexer, Token) = {
    if (this.hasNext) {
      input.substring(position) match {
        case str if str.startsWith("niech") => (this.advance("niech".length).get, Token(TokenType.Let, "niech"))

        // comparison
        case str if str.startsWith("==") => (this.advance("==".length).get, Token(TokenType.Equal, "=="))
        case str if str.startsWith("!=") => (this.advance("!=".length).get, Token(TokenType.NotEqual, "!="))
        case str if str.startsWith("<=") => (this.advance("<=".length).get, Token(TokenType.LessThanOrEqual, "<="))
        case str if str.startsWith(">=") => (this.advance(">=".length).get, Token(TokenType.GreaterThanOrEqual, ">="))
        case str if str.startsWith("<") => (this.advance("<".length).get, Token(TokenType.LessThan, "<"))
        case str if str.startsWith(">") => (this.advance(">".length).get, Token(TokenType.GreaterThan, ">"))

        case str if str.startsWith("=") => (this.advance("=".length).get, Token(TokenType.Assign, "="))

        // logic
        case str if str.startsWith("prawda ") => (this.advance("prawda".length).get, Token(TokenType.True, "prawda"))
        case str if str.startsWith("falsz ") => (this.advance("falsz".length).get, Token(TokenType.False, "falsz"))
        case str if str.startsWith("fałsz ") => (this.advance("fałsz".length).get, Token(TokenType.False, "fałsz"))
        case str if str.startsWith("i ") => (this.advance("i".length).get, Token(TokenType.And, "i"))
        case str if str.startsWith("lub ") => (this.advance("lub".length).get, Token(TokenType.Or, "lub"))
        case str if str.startsWith("!") => (this.advance("!".length).get, Token(TokenType.Not, "!"))

        // helpers
        case str if str.startsWith("\n") => (this.advance("\n".length).get, Token(TokenType.Newline, "\\n"))
        case str if str.startsWith(" ") =>
          val spaces = this.getRest.takeWhile(_ == ' ')
          (this.advance(spaces.length).get, Token(TokenType.Whitespace, spaces))
        case str if str.startsWith(",") => (this.advance(",".length).get, Token(TokenType.Comma, ","))
        case str if str.startsWith(";") => (this.advance(";".length).get, Token(TokenType.Semicolon, ";"))

        // math
        case str if str.startsWith("+") => (this.advance("+".length).get, Token(TokenType.Plus, "+"))
        case str if str.startsWith("-") => (this.advance("-".length).get, Token(TokenType.Minus, "-"))
        case str if str.startsWith("*") => (this.advance("*".length).get, Token(TokenType.Star, "*"))
        case str if str.startsWith("/") => (this.advance("/".length).get, Token(TokenType.Slash, "/"))

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
          val message =
          "Unexpected character: " + this.input.charAt(position) +
          " in line: " + this.line +
          " at column: " + this.column
          (this.advance(1).get, Token(TokenType.Error, message))
      }
    }
    else (this, Token(TokenType.EOF, ""))
  }
}


