import scala.util.parsing.combinator._
import Hodor._

abstract class TheEssenceOfHodor
case class HodorDecl(name: String) extends TheEssenceOfHodor
case class HodorAssignment(name: String, value: HodorVar)
case class HodorCodeBlock(expressions: Array[TheEssenceOfHodor]) extends TheEssenceOfHodor
case class HodorAdd(left: HodorVar, right: HodorVar) extends TheEssenceOfHodor
case class HodorSubtract(left: HodorVar, right: HodorVar) extends TheEssenceOfHodor
case class HodorMultiply() extends TheEssenceOfHodor
case class HodorDivide() extends TheEssenceOfHodor
case class HodorIf(expr: HodorCodeBlock) extends TheEssenceOfHodor
case class HodorElse() extends TheEssenceOfHodor
case class HodorCond() extends TheEssenceOfHodor
case class HodorGT(left: HodorVar, right: HodorVar) extends TheEssenceOfHodor

class HodorParser extends RegexParsers {
	/*def parse (code: String) = parseAll(program, code)

	def program: Parser[List[TheEssenceOfHodor]] = (decl | assignment | codeBlock) ~ program

	def decl: Parser[List[TheEssenceOfHodor]] = "hodor" ~> varName <~ ":)"

	def assignment: Parser[List[TheEssenceOfHodor]] = varName <~ "Hodor" ~> value <~ ":)"

	//def codeBlockParser: Parser[List[TheEssenceOfHodor]] = "HODOR..." ~> codeBlock <~ "HODOR!"
	//def codeBlock: Parser[List[TheEssenceOfHodor]] =
	*/ 
}


