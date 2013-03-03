import java.io.{File, OutputStreamWriter}
import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.history.FileHistory

sealed trait Form
class UnitForm extends Form
case class CharForm(c: Char) extends Form {
  override def toString = "'" + c.toString + "'"
}
case class RealForm(d: Double) extends Form {
  override def toString = d.toString
}
case class SymbolForm(s: String) extends Form {
  override def toString = s
}
case class ListForm(l: List[Form]) extends Form {
  override def toString = l.mkString("[", " ", "]")
}
case class CallForm(l: List[Form]) extends Form

case class Function(args: List[SymbolForm], body: CallForm)

case class SyntaxError(msg: String) extends Exception(msg)
case class TypeError(msg: String) extends RuntimeException(msg)
case class NameError(msg: String) extends RuntimeException(msg)
case class ArityError(msg: String) extends RuntimeException(msg)

object Parser extends JavaTokenParsers {

  lazy val program: Parser[List[Form]] = rep(form)
  lazy val form: Parser[Form] = char | real | symbol | string | list | call
  lazy val char: Parser[CharForm] = "'" ~> ".".r <~ "'" ^^ { s => CharForm(s.head) }
  lazy val real: Parser[RealForm] = floatingPointNumber ^^ { s => RealForm(s.toDouble) }
  lazy val symbol: Parser[SymbolForm] = """[a-zA-Z_@~%!=#<>\-\+\*\?\^\&\/]+""".r ^^ { s => SymbolForm(s.toString) }
  lazy val string: Parser[ListForm] = stringLiteral ^^ { s =>
    val chars = s.substring(1, s.size - 1).toList.map("'" + _ + "'")
    parse(list, "[" + chars.mkString(" ") + "]").get
  }
  lazy val list: Parser[ListForm] = "[" ~> rep(form) <~ "]" ^^ { ListForm(_) }
  lazy val call: Parser[CallForm] = "(" ~> rep(form) <~ ")" ^^ { CallForm(_) }

  def parse(s: String) = parseAll(program, s) match {
    case Success(r, _) => r
    case Failure(msg, _) => throw SyntaxError(msg)
    case Error(msg, _) => throw new RuntimeException(msg)
  }

}

object Evaluator {

  type Env = Map[SymbolForm, Any]

  val EmptyEnv = Map.empty[SymbolForm, Any]
  val EmptyResult = ListForm(Nil)

  implicit def double2Form(d: Double): RealForm = RealForm(d)
  implicit def form2Double(f: RealForm): Double = f.d

  implicit def forms2Symbols(l: List[Form]): List[SymbolForm] = l.map {
    case s: SymbolForm => s
    case _ => throw new RuntimeException("Not a symbol.")
  }

  def eval(form: Form, env: Env = EmptyEnv): (Form, Env) = form match {
    case s: SymbolForm => env(s) match {
      case f: Form => (f, env)
      case _ => throw NameError("Nope")
    }
    case c: CallForm => call(c, env)
    case ListForm(l) => (ListForm(l.map(eval(_, env)).unzip._1), env)
    case _ => (form, env)
  }

  private def mathOp(env: Env, l: List[Form], f: (RealForm, RealForm) => RealForm): (Form, Env) = {
    (l.map(eval(_, env)._1)) match {
      case l: List[Form] => {
        val r = l.tail.foldLeft(l.headOption.getOrElse(RealForm(0L))) {
          case (RealForm(a), RealForm(b)) => f(a, b)
          case _ => throw TypeError("Cannot perform math operation on non-numerical type.")
        }
        (r, env)
      }
      case _ => throw TypeError("Incorrect arguments.")
    }
  }

  def call(form: CallForm, env: Env): (Form, Env) = (form: @unchecked) match {
    case CallForm(head :: tail) => (head: @unchecked) match {
      case SymbolForm("+") => mathOp(env, tail, _ + _)
      case SymbolForm("-") => mathOp(env, tail, _ - _)
      case SymbolForm("*") => mathOp(env, tail, _ * _)
      case SymbolForm("/") => mathOp(env, tail, _ / _)
      case SymbolForm("print") => {
        println(tail.map(eval(_, env)._1.toString).mkString(" "))
        (new UnitForm, env)
      }
      case SymbolForm("defn") => tail match {
        case List(f: SymbolForm, ListForm(args), body: CallForm) => (new UnitForm, env + ((f, Function(args, body))))
        case _ => throw TypeError("Incorrect arguments.")
      }
      case s: SymbolForm => env.get(s) match {
        case Some(Function(args, body)) => {
          if (tail.size == args.size) {
            val vars = tail.map {
              case s: SymbolForm => env(s)
              case f: Form => f
            }
            eval(body, env ++ args.zip(vars).toMap)
          } else {
            throw ArityError("Function \"" + s + "\" takes exactly " + args.size + " arguments (" + tail.size + " given)")
          }
        }
        case Some(f: Form) => (form, env)
        case _ => throw NameError("Name \"" + s + "\" is not defined.")
      }
    }
  }

}

object Main {

  val in = {
    val reader = new ConsoleReader(System.in, new OutputStreamWriter(System.out))
    reader.setPrompt(">> ")
    reader.setHistory(new FileHistory(new File("~/.scala-lisp")))
    reader.setHistoryEnabled(true)
    reader
  }

  def repl {
    var env = Evaluator.EmptyEnv
    while (true) {
      in.readLine match {
        case line: String =>
          try {
            val (r, e) = interpret(line, env)
            env = e
            r match {
              case f: UnitForm =>
              case _ => println(r)
            }
          } catch {
            case e: Throwable => println(e.getClass.getSimpleName + ": " + e.getMessage)
          }
        case _ => return
      }
    }
  }

  def interpret(line: String, initialEnv: Evaluator.Env): (Form, Evaluator.Env) = {
    var res: Form = Evaluator.EmptyResult
    var env = initialEnv
    for (form <- Parser.parse(line)) {
      val (r, e) = Evaluator.eval(form, env)
      res = r
      env = e
    }
    (res, env)
  }

  def main(args: Array[String]) {
    repl
  }

}
