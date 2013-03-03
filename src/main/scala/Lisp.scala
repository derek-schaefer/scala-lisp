import scala.language.implicitConversions
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.history.FileHistory
import scala.util.parsing.combinator.JavaTokenParsers

import java.io.File

sealed trait Form

class UnitForm extends Form

case class BooleanForm(b: Boolean) extends Form {
  override def toString = b.toString
}

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

class Error(msg: String) extends Exception(msg)
case class SyntaxError(msg: String) extends Error(msg)
case class TypeError(msg: String) extends Error(msg)
case class NameError(msg: String) extends Error(msg)
case class ArityError(msg: String) extends Error(msg)

object Parser extends JavaTokenParsers {

  lazy val program: Parser[List[Form]] = rep(form)
  lazy val form: Parser[Form] = bool | char | real | symbol | string | list | call
  lazy val bool: Parser[BooleanForm] = """true|false""".r ^^ { s => BooleanForm(s == "true") }
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
  type Result = (Form, Env)

  val EmptyEnv = Map.empty[SymbolForm, Any]
  val EmptyResult = ListForm(Nil)

  implicit def double2Form(d: Double): RealForm = RealForm(d)
  implicit def form2Double(f: RealForm): Double = f.d

  implicit def forms2Symbols(l: List[Form]): List[SymbolForm] = l.map {
    case s: SymbolForm => s
    case _ => throw TypeError("Not a symbol.")
  }

  def eval(form: Form, env: Env = EmptyEnv): Result = form match {
    case s: SymbolForm => env(s) match {
      case f: Form => (f, env)
      case _ => throw NameError("Nope")
    }
    case c: CallForm => call(c, env)
    case ListForm(l) => (ListForm(l.map(eval(_, env)).unzip._1), env)
    case _ => (form, env)
  }

  private def mathOp(env: Env, l: List[Form], f: (RealForm, RealForm) => RealForm): Result = {
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

  def call(form: CallForm, env: Env): Result = (form: @unchecked) match {
    case CallForm(head :: tail) => (head: @unchecked) match {
      case SymbolForm("+") => mathOp(env, tail, _ + _)
      case SymbolForm("-") => mathOp(env, tail, _ - _)
      case SymbolForm("*") => mathOp(env, tail, _ * _)
      case SymbolForm("/") => mathOp(env, tail, _ / _)
      case SymbolForm("defn") => tail match {
        case List(f: SymbolForm, ListForm(args), body: CallForm) => (new UnitForm, env + ((f, Function(args, body))))
        case _ => throw TypeError("Incorrect arguments.")
      }
      case SymbolForm("print") => {
        println(tail.map(eval(_, env)._1.toString).mkString(" "))
        (new UnitForm, env)
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

  val history = {
    val home = System.getProperty("user.home")
    new FileHistory(new File(home + "/.scala-lisp"))
  }

  val in = {
    val reader = new ConsoleReader(System.in, System.out, null, null)
    reader.setPrompt(">> ")
    reader.setHistory(history)
    reader.setHistoryEnabled(true)
    reader
  }

  def repl(env: Evaluator.Env = Evaluator.EmptyEnv) {
    in.readLine match {
      case line: String =>
        try {
          interpret(line, env) match {
            case (f: UnitForm, e: Evaluator.Env) => repl(e)
            case (f: Form, e: Evaluator.Env) => {
              println(f)
              repl(e)
            }
          }
        } catch {
          case e: Error => {
            println("%s: %s".format(e.getClass.getSimpleName, e.getMessage))
            repl(env)
          }
        }
      case _ => return
    }
  }

  def interpret(line: String, env: Evaluator.Env): Evaluator.Result = {
    def loop(forms: List[Form], res: Evaluator.Result): Evaluator.Result = forms match {
      case f :: Nil => Evaluator.eval(f, env)
      case f :: fs => loop(fs, Evaluator.eval(f, env))
    }
    loop(Parser.parse(line), (new UnitForm, env))
  }

  def main(args: Array[String]) { repl() }

}
