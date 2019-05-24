package ru.fpure.parboiledtest

import org.parboiled2.{CharPredicate, Parser, ParserInput, Rule1}

class SimpleParser(val input: ParserInput) extends Parser {
  def start: Rule1[AST] = rule {
    (andThen | booleanExpr) ~ EOI
  }

  def booleanExpr = rule {
    forExpr | basicBooleanExpr
  }

  def andThen: Rule1[AndThenAST] = rule {
    booleanExpr ~ ws ~ ignoreCase("andthen") ~ ws ~ booleanExpr ~> ((v1: AST, v2: AST) => AndThenAST(v1, v2))
  }

  def forExpr: Rule1[ForAST] = rule {
    (basicBooleanExpr ~ ignoreCase("for") ~ ws ~ time ~ ws ~ optional(range)) ~> (
      (cond: AST,
       window: Long,
       range: Option[(Long, Long)]) =>
        ForAST(cond, window, range.getOrElse((window, window)))
      )
  }

  def basicBooleanExpr: Rule1[AST] = rule {
    booleanTerm ~ zeroOrMore(
      ignoreCase("or") ~ ws ~ booleanTerm ~>
        ((e: AST, f: AST) => FunctionCallAST('or, Seq(e, f)))
        | ignoreCase("xor") ~ ws ~ booleanTerm ~>
        ((e: AST, f: AST) => FunctionCallAST('xor, Seq(e, f)))
    )
  }

  def booleanTerm: Rule1[AST] = rule {
    booleanFactor ~ zeroOrMore(
      ignoreCase("and") ~ !ignoreCase("then") ~ ws ~ booleanFactor ~>
        ((e: AST, f: AST) => FunctionCallAST('and, Seq(e, f)))
    )
  }

  def booleanFactor: Rule1[AST] = rule {
    comparison |
      "(" ~ basicBooleanExpr ~ ")" ~ ws | "not" ~ basicBooleanExpr ~> ((b: AST) => FunctionCallAST('not, Seq(b)))
  }

  def logical: Rule1[AST] = rule {
    (comparison ~ ws ~ booleanOperator ~ ws ~ comparison ~ ws) ~> (
        (v1: AST,
         op: String,
         v2: AST) => FunctionCallAST(Symbol(op), List(v1, v2)))
  }

  def range: Rule1[(Long, Long)] = rule {
    ("<" ~ ws ~ time ~> ((t: Long) => (0L, t - 1))
      | "<=" ~ ws ~ time ~> ((t: Long) => (0L, t))
      | ">" ~ ws ~ time ~> ((t: Long) => (t + 1, Long.MaxValue))
      | ">=" ~ ws ~ time ~> ((t: Long) => (t, Long.MaxValue))
      )
  }

  def comparison: Rule1[AST] = rule {
    (value ~ ws ~ operator ~ ws ~ value ~ ws) ~> (
        (v1: AST,
         op: String,
         v2: AST) => FunctionCallAST(Symbol(op), List(v1, v2)))
  }

  def operator: Rule1[String] = rule {
    ((str("=") ~> (() => "eq"))
      | (str("!=") ~> (() => "ne")))
  }

  def booleanOperator: Rule1[String] = rule {
    (ignoreCase("and") ~ !ignoreCase("then")) ~> (() => "and") | ignoreCase("or")~> (() => "or")
  }

  def value: Rule1[AST] = rule {
    number | functionCall | (anyWord ~> ((w: String) => FieldAST(Symbol(w))))
  }

  def anyWord: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | '_')) ~ ws
  }

  def timeUnit: Rule1[Int] = rule {
    (ignoreCase("seconds") ~> (() => 1000)
      | ignoreCase("sec") ~> (() => 1000)
      | ignoreCase("minutes") ~> (() => 60000)
      | ignoreCase("min") ~> (() => 60000)
      | ignoreCase("milliseconds") ~> (() => 1)
      | ignoreCase("ms") ~> (() => 1)
      | ignoreCase("hours") ~> (() => 3600000)
      | ignoreCase("hr") ~> (() => 3600000))
  }

  def number: Rule1[ConstantAST] = rule {
    // sign of a number: positive (or empty) = 1, negative = -1
    ((str("+") ~> (() => 1) | str("-") ~> (() => -1) | str("") ~> (() => 1))
      ~ capture(oneOrMore(CharPredicate.Digit)) ~ ws
      ~> ((s: Int, i: String) => ConstantAST(s * i.toLong)))
  }

  def time: Rule1[Long] = rule {
    number ~ timeUnit ~ ws ~>
      ((i: ConstantAST, u: Int) => i.value * u)
  }

  def functionCall = rule {
    (anyWord ~ ws ~ "(" ~ ws ~ value.*(ws ~ "," ~ ws) ~ ws ~ ")" ~ ws ~>
      ((function: String, arguments: Seq[AST]) =>
        FunctionCallAST(Symbol(function), arguments.toList)
        )
      )
  }

  def ws = rule {
    quiet(zeroOrMore(anyOf(" \t \n \r")))
  }
}
