import scala.util.parsing.combinator._
import _root_.scala.language.postfixOps
import Hodor._

abstract class TheEssenceOfHodor
case class HodorProgram(codeBlock: HodorCodeBlock)
case class HodorStatementSeq(states: List[HodorStatement])

abstract class HodorStatement extends TheEssenceOfHodor
case class HodorFuncDecl(name: String, vars: List[String], code: HodorCodeBlock) extends HodorStatement
case class HodorVarDecl(name: String) extends HodorStatement
case class HodorAssign(name: String, hodorExpr: HodorExpr) extends HodorStatement
case class HodorPrint(expr: HodorExpr) extends HodorStatement
case class HodorCodeBlock(statementSequence: List[HodorStatement]) extends HodorStatement

abstract class HodorExpr extends HodorStatement
case class HodorFuncCall(name: String, params: List[HodorExpr]) extends HodorExpr
case class HodorVarExpr(name: String) extends HodorExpr
case class HodorNumber(n: Float) extends HodorExpr
case class HodorAdd(operands: List[HodorExpr]) extends HodorExpr
case class HodorSubtract(operands: List[HodorExpr]) extends HodorExpr
case class HodorMultiply(operands: List[HodorExpr]) extends HodorExpr
case class HodorDivide(operands: List[HodorExpr]) extends HodorExpr
case class HodorTrue() extends HodorExpr
case class HodorAnd(operands: List[HodorExpr]) extends HodorExpr
case class HodorOr(operands: List[HodorExpr]) extends HodorExpr
case class HodorNot(expr: HodorExpr) extends HodorExpr
case class HodorGT(left: HodorExpr, right: HodorExpr) extends HodorExpr
case class HodorLT(left: HodorExpr, right: HodorExpr) extends HodorExpr
case class HodorEQ(left: HodorExpr, right: HodorExpr) extends HodorExpr
case class HodorStr(str: String) extends HodorExpr

abstract class HodorConditional extends HodorStatement
case class HodorIf(expr: HodorExpr, thn: HodorCodeBlock) extends HodorConditional
case class HodorIfElse(expr: HodorExpr, thn: HodorCodeBlock, els: HodorCodeBlock) extends HodorConditional
case class HodorLoop(expr: HodorExpr, thn: HodorCodeBlock) extends HodorConditional

// Need functions

object HodorParser extends RegexParsers {
    override def skipWhitespace = true

    def parseProgram: Parser[HodorProgram] = block ^^ {  // returns code block change later
	    case b => HodorProgram(b)
    }

    def block: Parser[HodorCodeBlock] = "HODOR..." ~> statementSeq <~ "HODOR!" ^^ {
        case s => HodorCodeBlock(s)
    }

    def statementSeq = statement*

    def statement: Parser[HodorStatement] = (funcDecl | (expr <~ ":)") | varDecl | varAssign | ifElseState | ifState | whileLoop | printState | block) | comment ~> (funcDecl | (expr <~ ":)") | varDecl | varAssign | ifElseState | ifState | whileLoop | printState | block) ^^ {
        case s => s
    }

    def varName: Parser[String] = ("""([A-Za-z0-9]+)""").r ^^ {
        case v => v
    }

    def varDecl: Parser[HodorVarDecl] = "hodor" ~> varName <~ ":)" ^^ {
	    case v => HodorVarDecl(v)
    }

    def varAssign: Parser[HodorAssign] = (varName <~ "Hodor") ~ (expr <~ ":)") ^^ {
	    case v ~ e => HodorAssign(v, e)
    }

    def expr: Parser[HodorExpr] = ( funcCall | or | and | not | bool | gt | lt | eq | add | subr | mult | div | number | stringVal | varExpr) ^^ {
        case e => e
    }

    def comment = "<hodor>.*<hodor>".r

    def stringVal: Parser[HodorExpr] = "\"" ~> """([^"]*)""".r <~ "\"" ^^ {
        case s => HodorStr(s)
    }

    def number: Parser[HodorExpr] = """[-]?[0-9]+[\.]?[0-9]*""".r ^^ {
        case n => HodorNumber(n.toFloat)
    }

    def add: Parser[HodorExpr] = (("(" ~> ("HoDoR" ~> expr) ~ (expr+) <~ ")") | (("HoDoR" ~> expr) ~ (expr+))) ^^ {
        case left ~ right => HodorAdd(left :: right)
    }

    def subr: Parser[HodorExpr] = (("(" ~> ("hOdOr" ~> expr) ~ (expr+) <~ ")") | (("hOdOr" ~> expr) ~ (expr+))) ^^ {
        case left ~ right => HodorSubtract(left :: right)
    }

    def mult: Parser[HodorExpr] = (("(" ~> ("HODor" ~> expr) ~ (expr+) <~ ")") | (("HODor" ~> expr) ~ (expr+))) ^^ {
        case left ~ right => HodorMultiply(left :: right)
    }

    def div: Parser[HodorExpr] = (("(" ~> ("hoDOR" ~> expr) ~ (expr+) <~ ")") | (("hoDOR" ~> expr) ~ (expr+))) ^^ {
        case left ~ right => HodorDivide(left :: right)
    }

    def varExpr: Parser[HodorExpr] = varName ^^ {
        case v => HodorVarExpr(v)
    }

    def bool: Parser[HodorExpr] = """hodorHODORhodor""".r ^^ {
        case s => HodorTrue()
    }

    def and: Parser[HodorExpr] = (("(" ~> ("hodor.hodor" ~> expr) ~ (expr+) <~ ")") | (("hodor.hodor" ~> expr) ~ (expr+))) ^^ {
        case left ~ right => HodorAnd(left :: right)
    }

    def or: Parser[HodorExpr] = (("(" ~> ("HODOR.HODOR" ~> expr) ~ (expr+) <~ ")") | (("HODOR.HODOR" ~> expr) ~ (expr+))) ^^ {
        case left ~ right => HodorOr(left :: right)
    }

    def not: Parser[HodorExpr] = ((((("(" ~> "steve") ~> expr)) <~ ")") | ("steve" ~> expr)) ^^ {
        case e => HodorNot(e)
    }

    def gt: Parser[HodorExpr] = (("(" ~> ("HODORhodor" ~> expr) ~ (expr <~ ")")) | (("HODORhodor" ~> expr) ~ expr)) ^^ {
        case left ~ right => HodorGT(left, right)
    }

    def lt: Parser[HodorExpr] = (("(" ~> ("hodorHODOR" ~> expr) ~ (expr <~ ")")) | (("hodorHODOR" ~> expr) ~ expr)) ^^ {
        case left ~ right => HodorLT(left, right)
    }

    def eq: Parser[HodorExpr] = (("(" ~> ("hodor^hodor" ~> expr) ~ (expr <~ ")")) | (("hodor^hodor" ~> expr) ~ expr)) ^^ {
        case left ~ right => HodorEQ(left, right)
    }

    def ifElseState: Parser[HodorConditional] = "HODOR?" ~> expr ~ block ~ elseState ^^ {
        case e ~ b ~ els => HodorIfElse(e, b, els)
    }

    def ifState: Parser[HodorConditional] = "HODOR?" ~> expr ~ block ^^ {
        case e ~ b => HodorIf(e, b)
    }

    def elseState: Parser[HodorCodeBlock] = "HODOR/" ~> block ^^ {
        case b => b
    }

    def funcDecl: Parser[HodorFuncDecl] = ("_HODOR" ~> varName) ~ (varName*) ~ "_" ~ block ^^ {
        case v ~ vN ~ "_" ~ b => {
            HodorFuncDecl(v, vN, b)
        }
    }

    def funcCall: Parser[HodorFuncCall] = ("_hodor" ~> varName) ~ (expr*) <~ "_" ^^ {
        case f ~ vL => HodorFuncCall(f, vL)
    }

    def printState: Parser[HodorPrint] = "|HODOR|" ~> expr <~ ":)" ^^ {
        case e => HodorPrint(e)
    }

    def whileLoop: Parser[HodorLoop] = ("hoodddoooorrrrr" ~> expr) ~ block ^^ {
        case e ~ b => HodorLoop(e, b)
    }
}


