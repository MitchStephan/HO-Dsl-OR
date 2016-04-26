import scala.util.parsing.combinator._
import Hodor._

abstract class TheEssenceOfHodor
case class HodorVarDecl(name: String) extends TheEssenceOfHodor
case class HodorAssign(name: String, hodorVar: HodorVar)
case class HodorAssignment(name: String, value: HodorVar)
case class HodorCodeBlock(statementSequence: List[TheEssenceOfHodor]) extends TheEssenceOfHodor
case class HodorAdd(left: HodorVar, right: HodorVar) extends TheEssenceOfHodor
case class HodorSubtract(left: HodorVar, right: HodorVar) extends TheEssenceOfHodor
case class HodorMultiply() extends TheEssenceOfHodor
case class HodorDivide() extends TheEssenceOfHodor
case class HodorIf(expr: HodorCodeBlock) extends TheEssenceOfHodor
case class HodorElse() extends TheEssenceOfHodor
case class HodorCond() extends TheEssenceOfHodor
case class HodorGT(left: HodorVar, right: HodorVar) extends TheEssenceOfHodor
case class HodorPrint(hodorVar: HodorVar) extends TheEssenceOfHodor
case class HodorTrue() extends TheEssenceOfHodor

object HodorParser extends RegexParsers {

	def parseProgram: Parser[String] = "HODOR..." ~> varName <~ "HODOR!" ^^ {  // returns code block change later
		case v => v
	}

	def varName: Parser[String] = ("""([A-Za-z0-9]+)""") ^^ {
		case v => v
	}

	def varDecl: Parser[HodorVarDecl] = "hodor" ~> varName <~ ":)" ^^ { 
		case v => HodorVarDecl(v)
	}

	def hodorTrue: Parser[HodorTrue] = "hodorHODORhodor" ^^ { 
		case _ => HodorTrue() 
	}

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


