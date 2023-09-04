import NodeType.{ExpressionStatement, Positive}

class Evaluator() {
  def eval(statements: List[Node]): Unit = {
    def evalExpression(expression: Node): Int = {
      expression.nodeType match
      case NodeType.Positive       => evalExpression(expression.children.head).toInt
      case NodeType.Negative       => - evalExpression(expression.children.head).toInt
      case NodeType.Add            => evalExpression(expression.children.head) + evalExpression(expression.children(1)).toInt
      case NodeType.Sub            => evalExpression(expression.children.head) - evalExpression(expression.children(1)).toInt
      case NodeType.Mul            => evalExpression(expression.children.head) * evalExpression(expression.children(1)).toInt
      case NodeType.Div            => evalExpression(expression.children.head) / evalExpression(expression.children(1)).toInt
      case NodeType.Pow            => Math.pow(evalExpression(expression.children.head), evalExpression(expression.children(1))).toInt
      case NodeType.IntegerLiteral => expression.literal.toInt
      case _                       => throw new Exception("Unknown node type: " + expression.nodeType)
    }
    statements foreach { statement => println(evalExpression(statement)) }
  }
}
