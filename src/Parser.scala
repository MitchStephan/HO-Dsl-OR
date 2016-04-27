import scala.util.parsing.combinator._
import scala.language.postfixOps
import Hodor._

abstract class TheEssenceOfHodor
case class HodorProgram(statementSequence: List[HodorStatement])
case class HodorStatementSeq(states: List[HodorStatement])
case class HodorCodeBlock(statementSequence: List[TheEssenceOfHodor]) extends TheEssenceOfHodor

abstract class HodorStatement extends TheEssenceOfHodor

case class HodorFuncDecl(name: String, vars: List[String], code: HodorCodeBlock) extends HodorStatement
case class HodorVarDecl(name: String) extends HodorStatement
case class HodorAssign(name: String, hodorVar: HodorExpr) extends HodorStatement

abstract class HodorExpr extends TheEssenceOfHodor
case class HodorFuncCall(name: String, params: List[String]) extends HodorExpr
case class HodorVarExpr(name: String) extends HodorExpr
case class HodorNumber(n: Int) extends HodorExpr
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
case class HodorEQ(operands: List[HodorExpr]) extends HodorExpr

abstract class HodorConditional extends HodorStatement
case class HodorIf(expr: HodorCodeBlock, thn: HodorCodeBlock) extends HodorConditional
case class HodorIfElse(expr: HodorCodeBlock, thn: HodorCodeBlock, els: HodorCodeBlock) extends HodorConditional
case class HodorCond() extends HodorStatement
case class HodorPrint(hodorVar: HodorVar) extends HodorStatement

// Need functions

object HodorParser extends RegexParsers {
  override def skipWhitespace = true

  def parseProgram: Parser[HodorProgram] = "HODOR..." ~> statementSeq <~ "HODOR!" ^^ {  // returns code block change later
	case v => HodorProgram(v)
  }

  def block: Parser[HodorCodeBlock] = "HODOR..." ~> statementSeq <~ "HODOR!" ^^ {
    case s => HodorCodeBlock(s)
  }

  def statementSeq = statement*

  def statement: Parser[HodorStatement] = (varDecl | varAssign | ifElseState | ifState | funcDecl) ^^ {
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

  def expr: Parser[HodorExpr] = (and | or | not | gt | lt | eq | bool | add | subr | mult | div | number | varExpr | funcCall) ^^ {
    case e => e
  }

  def number: Parser[HodorExpr] = """[0-9]+""".r ^^ {
    case n => HodorNumber(n.toInt)
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

  def eq: Parser[HodorExpr] = (("(" ~> ("hodorhodor || HODORHODOR" ~> expr) ~ (expr+) <~ ")") | (("hodorhodor || HODORHODOR" ~> expr) ~ (expr+))) ^^ {
    case left ~ right => HodorEQ(left :: right)
  }

  def ifElseState: Parser[HodorConditional] = "HODOR?" ~> block ~ block ~ elseState ^^ {
    case b1 ~ b2 ~ els => HodorIfElse(b1, b2, els)
  }

  def ifState: Parser[HodorConditional] = "HODOR?" ~> block ~ block ^^ {
    case b1 ~ b2 => HodorIf(b1, b2)
  }

  def elseState: Parser[HodorCodeBlock] = "HODOR/" ~> block ^^ {
    case b => b
  }


  def funcDecl: Parser[HodorFuncDecl] = ("_HODOR_" ~> varName) ~ ("(" ~> (varName*)) ~ (")" ~> block) ^^ { 
    case v ~ vN ~ b => HodorFuncDecl(v,vN,b)
  }

  def funcCall: Parser[HodorFuncCall] = ("_hodor_" ~> varName) ~ ("(" ~> (varName*)) <~ ")" ^^ {
    case f ~ vL => HodorFuncCall(f, vL)
  }
  /*| assign | codeBlock | ifStat | printStat*/

  //def statement: HodorVarDecl = """hodor""" ^^ { case v => HodorVarDecl(v) }
  //def statement = hodorAssign | hodorVarDecl | hodorCodeBlock | hodorIf | hodorPrint

  //def varName: Parser[String] = ("""([A-Za-z0-9]+)""") ^^ {
  //	case v => v
  //}

  /*def varVal = ("""([A-Za-z0-9]+)""") ^^ {
   case v => v
   }
   
   def hodorVarDecl: Parser[HodorVarDecl] = "hodor" ~> varName <~ ":)" ^^ { 
   case v => HodorVarDecl(v)
   }

   def hodorTrue: Parser[HodorTrue] = "hodorHODORhodor" ^^ { 
   case _ => HodorTrue() 
   }

   def hodorAssign: Parser[HodorAssign] = varName <~ "Hodor" ~> varVal ^^ {
   //case n <~ "Hodor" ~> v => HodorAssign(n, v)
   case _ => HodorAssign("HOdor" , HodorInt(5))
   }
   
   def hodorCodeBlock: Parser[HodorCodeBlock] = "HODOR..." ~> statementSeq <~ "HODOR!" ^^ { (s) => HodorCodeBlock(s) }

   def hodorIf: Parser[HodorIf] = "HODOR?" ~> hodorCodeBlock ~ hodorCodeBlock ~ hodorCodeBlock ^^ { 
   case cond ~ block1 ~ block2 => HodorIf(cond, block1, block2) 
   }*/

  // def varAssign: Parser[HodorAssign] = varName <~ "Hodor" ~> varVal <~ ":)" { }

  // def print: Parser[HodorPrint] = new Regex("|HODOR|\s+" + varRegex)


  /*def parse (code: String) = parseAll(program, code)


   def program: Parser[List[TheEssenceOfHodor]] = (decl | assignment | codeBlock) ~ program

   def decl: Parser[List[TheEssenceOfHodor]] = "hodor" ~> varName <~ ":)"

   def assignment: Parser[List[TheEssenceOfHodor]] = varName <~ "Hodor" ~> value <~ ":)"

   //def codeBlockParser: Parser[List[TheEssenceOfHodor]] = "HODOR..." ~> codeBlock <~ "HODOR!"
   //def codeBlock: Parser[List[TheEssenceOfHodor]] =
   */ 
}


