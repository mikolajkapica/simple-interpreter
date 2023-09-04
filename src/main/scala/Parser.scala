import TokenType._

import scala.annotation.tailrec

enum NodeType {
  case LetStatement
  case ReturnStatement
  case IfStatement
  case ExpressionStatement
  case Identifier
  case IntegerLiteral
  case Add, Sub, Mul, Div, Pow
  case Positive, Negative
}

case class Node(nodeType: NodeType, literal: String, children: List[Node])

enum Precedence {
  case Precedence_Min
  case Precedence_Term
  case Precedence_Factor
  case Precedence_Power
}

val precedences: Map[TokenType, Precedence] = Map(
  Plus  -> Precedence.Precedence_Term,
  Minus -> Precedence.Precedence_Term,
  Star  -> Precedence.Precedence_Factor,
  Slash -> Precedence.Precedence_Factor,
  Caret -> Precedence.Precedence_Power,
  RParen -> Precedence.Precedence_Min,
  LParen -> Precedence.Precedence_Min,
)



class Parser(val currentToken: Int) {
  def this() = this(0)

  type Tokens = List[Token]
  type Statements = List[Node]

  def getAst(tokens: Tokens): Statements = 
    parseExpression(this, tokens, Precedence.Precedence_Min)._1 :: Nil

  def parseNumber(tokens: Tokens): (Node, Parser) =
    val token = tokens(currentToken)
    (Node(NodeType.IntegerLiteral, token.lexeme, Nil), Parser(currentToken + 1))
  
  def parseTerminalExpr(parser: Parser, tokens: Tokens) : (Node, Parser) = {
    tokens(parser.currentToken).tokenType match {
      case Int => parser.parseNumber(tokens)
      case LParen =>
        val (ret, newParser) = parseExpression(parser_advance(parser), tokens, Precedence.Precedence_Min)
        if (tokens(newParser.currentToken).tokenType != RParen) throw new Exception("Expected )")
        (ret, Parser(newParser.currentToken + 1))
      case Plus => 
        val (expr, newParser) = parseTerminalExpr(parser_advance(parser), tokens)
        (Node(NodeType.Positive, "", List(expr)), newParser)
      case Minus => 
        val (expr, newParser) = parseTerminalExpr(parser_advance(parser), tokens)
        (Node(NodeType.Negative, "", List(expr)), newParser)
      case x => throw new Exception("bad terminal expression - got: " + x)
    }

  }

  def parser_advance(parser: Parser): Parser = Parser(parser.currentToken + 1)

  def parse_infix_expr(parser: Parser, tokens: Tokens, operator: Token, left: Node): (Node, Parser) = {
    val nodeType = operator.tokenType match {
      case Plus  => NodeType.Add
      case Minus => NodeType.Sub
      case Star  => NodeType.Mul
      case Slash => NodeType.Div
      case Caret => NodeType.Pow
      case _ => throw new Exception("bad infix expression - got: " + operator)
    }
    val (right, new_parser) = parser.parseExpression(parser, tokens, precedences(operator.tokenType))
    (Node(nodeType, operator.lexeme, List(left, right)), new_parser)
  }

  def parseExpression(old_parser: Parser, tokens: Tokens, previous_precedence: Precedence) : (Node, Parser) = 
    val (left, parser) = parseTerminalExpr(old_parser, tokens)
    if (parser.currentToken + 2 > tokens.length) return (left, parser)
    val current_operator = tokens(parser.currentToken)
    @tailrec
    def aux(parser: Parser, left: Node, current_operator: Token): (Node, Parser) =
      val current_precedence = precedences(current_operator.tokenType)
      if (current_precedence.ordinal <= previous_precedence.ordinal) (left, parser) 
      else {
        val new_parser = parser_advance(parser)
        val (new_left, new_new_parser) = parse_infix_expr(new_parser, tokens, current_operator, left)
        val new_current_operator = tokens(new_new_parser.currentToken)
        if (new_current_operator.tokenType == EOF) (new_left, new_new_parser) else
        aux(new_new_parser, new_left, new_current_operator)
      }
    aux(parser, left, current_operator)
}

