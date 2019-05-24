package ru.fpure.parboiledtest

trait AST

case class ConstantAST(value: Long) extends AST

case class FieldAST(field: Symbol) extends AST

case class FunctionCallAST(function: Symbol, arguments: Seq[AST]) extends AST

case class AndThenAST(first: AST, second: AST) extends AST

case class ForAST(cond: AST, window: Long, duration: (Long, Long)) extends AST