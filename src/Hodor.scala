import scala.collection.mutable.{ HashMap, Stack }
import scala.util.Random
import scala.math.{ min, max }
import scala.io.Source
import HodorParser._

object Hodor {
	var localScope: Scope = new Scope(None)

	class Scope (parentScope: Option[Scope]) {
		var funcs: HashMap[String, Function[Array[HodorVar], HodorVar]] = 
			new HashMap[String, Function[Array[HodorVar], HodorVar]]()
		var vars: HashMap[String, HodorVar] = new HashMap[String, HodorVar]
		var parent: Option[Scope] = parentScope

		def getFunc (name: String) {
			if (funcs.contains(name)) {
				funcs.get(name)
			} else {
				parent match {
					case Some(scope) => scope.getFunc(name)
					case None => throw new IllegalArgumentException("function was not defined")
				}
			}
		}

		def getVar (name: String) {
			if (vars.contains(name)) {
				vars.get(name)
			} else {
				parent match {
					case Some(scope) => scope.getVar(name)
					case None => throw new IllegalArgumentException("var is not defined")
				}
			}
		}

		def defineVar (name: String) {
			println("inside define var")
			vars += (name -> HodorNone())
		}

		def defineFunc (name: String, func: Function[Array[HodorVar], HodorVar]) {
			funcs += (name -> func)
		}

		def setVar (name: String, value: HodorVar) {
			if (vars.contains(name)) {
				vars(name) = value
			} else {
				parent match {
					case Some(scope) => scope.setVar(name, value)
					case None => throw new IllegalArgumentException("var is not defined")
				}
			}
		}
	}

	abstract sealed class HodorVar
    case class HodorInt(value: Int) extends HodorVar
	case class HodorBoolean(value: Boolean) extends HodorVar
	case class HodorString(value: String) extends HodorVar
	case class HodorNone() extends HodorVar

	abstract sealed class HodorLine
	case class Declare() extends HodorLine
	case class Assign() extends HodorLine
	case class Print() extends HodorLine

	def evaluateProgram(program: HodorProgram) {
		// will want a new scope, but using global for now
		for (statement <- program.statementSequence) {
		 statement match {
		 	case s: HodorFuncDecl => evaluateFunctionDeclare(s)
		 	case s: HodorVarDecl => evaluateVarDeclare(s)
		 	case s: HodorAssign => evaluateAssign(s)
		 	case s: HodorPrint => evaluatePrint(s)
		 } 
		}
	}

	def evaluateFunctionDeclare(funcDecl: HodorFuncDecl) {
		println(funcDecl)
	}

	def evaluateVarDeclare(varDecl: HodorVarDecl) {
		println(varDecl)
		localScope.defineVar(varDecl.name)
		printHashMap(localScope.vars)
	}

	def evaluateAssign(assign: HodorAssign) {
		println(assign)
		val hodorVar = HodorString("temp string") //evaluteExpression(assign.hodorExpr)
		localScope.setVar(assign.name, hodorVar)
		printHashMap(localScope.vars)
	}

	def evaluatePrint(print: HodorPrint) {
		println(print)
	}

	def printHashMap(map: HashMap[_, _]) {
		for ((k,v) <- map) {
			print("("+k+","+v+"), ")
		}
		println("")
	}

    def main(args: Array[String]): Unit = {
        for (file <- args) {
            val source = scala.io.Source.fromFile(file)
            val lines = try source.mkString finally source.close()
            // println(lines)
            val parseResult: HodorParser.ParseResult[HodorProgram] = HodorParser.parse(parseProgram, lines);
            println(parseResult)
            val hodorProgram: HodorProgram = parseResult.get
            // println(hodorProgram)
            evaluateProgram(hodorProgram)
        }
    }
}
