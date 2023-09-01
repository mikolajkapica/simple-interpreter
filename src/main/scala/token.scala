enum TokenType:
  case Let

  case Error
  case Comment

  // comparison
  case Equal
  case NotEqual
  case GreaterThanOrEqual
  case LessThanOrEqual
  case GreaterThan
  case LessThan

  case Assign

  // logic
  case True
  case False
  case And
  case Or
  case Not


  // helpers
  case Whitespace
  case Newline
  case Comma
  case Semicolon

  // math
  case Plus
  case Minus
  case Star
  case Slash
  case Caret

  // parentheses
  case LParen
  case RParen
  case LBrace
  case RBrace

  // keywords
  case If
  case Else
  case While
  case Return
  case Function
  case Print

  // literals
  case Ident
  case Int

// EOF
  case EOF


case class Token(tokenType: TokenType, lexeme: String)
