package ru.fpure.parboiledtest

import org.parboiled2.ParseError

import scala.util.{Failure, Success}

object MainApp extends App {
  val rule = "lag(Speed) != 0 and Speed = 0 andThen OilPump != 0 for 90 sec < 60 sec"
  val p = new SimpleParser(rule)
  val z = p.start.run()
  z match {
    case Failure(exception) => println(p.formatError(exception.asInstanceOf[ParseError]))
    case Success(value) => println(value)
  }
}
