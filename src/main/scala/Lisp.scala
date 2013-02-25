import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

sealed trait Form
class UnitForm extends Form
case class RealForm(d: Double) extends Form
case class SymbolForm(s: String) extends Form
case class ListForm(l: List[Form]) extends Form
case class CallForm(l: List[Form]) extends Form

case class Function(args: List[SymbolForm], body: CallForm)

case class SyntaxError(msg: String) extends Exception(msg)
case class TypeError(msg: String) extends RuntimeException(msg)
case class NameError(msg: String) extends RuntimeException(msg)
case class ArityError(msg: String) extends RuntimeException(msg)

object Parser extends JavaTokenParsers {

  lazy val program: Parser[List[Form]] = rep(form)
  lazy val form: Parser[Form] = real | symbol | list | call
  lazy val real: Parser[RealForm] = floatingPointNumber ^^ { s => RealForm(s.toDouble) }
  lazy val symbol: Parser[SymbolForm] = """[a-zA-Z_@~%!=#<>\-\+\*\?\^\&\/]+""".r ^^ { s => SymbolForm(s.toString) }
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
        val r = l.tail.foldLeft(l.headOption.getOrElse(RealForm(0))) {
          case (RealForm(a), RealForm(b)) => f(a, b)
          case _ => throw TypeError("NO")
        }
        (r, env)
      }
      case _ => throw TypeError("NO")
    }
  }

  def call(form: CallForm, env: Env): (Form, Env) = (form: @unchecked) match {
    case CallForm(head :: tail) => (head: @unchecked) match {
      case SymbolForm("+") => mathOp(env, tail, _ + _)
      case SymbolForm("-") => mathOp(env, tail, _ - _)
      case SymbolForm("*") => mathOp(env, tail, _ * _)
      case SymbolForm("/") => mathOp(env, tail, _ / _)
      case SymbolForm("defn") => tail match {
        case List(f: SymbolForm, ListForm(args), body: CallForm) => (new UnitForm, env + ((f, Function(args, body))))
        case _ => throw TypeError("Incorrect arguments.")
      }
      case s: SymbolForm => env.get(s) match {
        case Some(Function(args, body)) => {
          if (tail.size == args.size) {
            eval(body, env ++ args.zip(tail).toMap) // TODO: eval symbols
          } else {
            throw ArityError("Function \"" + s + "\" takes exactly " + args.size + " arguments (" + tail.size + " given)")
          }
        }
        case Some(f: Form) => (form, env)
        case _ => throw NameError("Name \"" + s + "\" is not defined.")
      }
    }
  }

  def main(args: Array[String]) {
    for (s <- args) {
      var res: Form = Evaluator.EmptyResult
      var env = Evaluator.EmptyEnv
      for (form <- Parser.parse(s)) {
        val (r, e) = eval(form, env)
        res = r
        env = e
      }
      println(res)
    }
  }

}
