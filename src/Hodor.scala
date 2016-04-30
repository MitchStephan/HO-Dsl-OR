import scala.collection.mutable.{ HashMap, Stack }
import scala.util.Random
import scala.math.{ min, max }
import scala.io.Source
import HodorParser._

object Hodor {
	var localScope: Option[Scope] = None

	class Scope (parentScope: Option[Scope]) {
		var funcs: HashMap[String, Function[Array[HodorVar], HodorVar]] = 
			new HashMap[String, Function[Array[HodorVar], HodorVar]]()
		var vars: HashMap[String, HodorVar] = new HashMap[String, HodorVar]
		var parent: Option[Scope] = parentScope
		var returnVal: HodorVar = HodorNone()

		def getFunc (name: String) {
			if (funcs.contains(name)) {
				funcs.get(name)
			} else {
				parent match {
					case Some(s) => s.getFunc(name)
					case _ => throw new IllegalArgumentException("function was not defined")
				}
			}
		}

		def getVar (name: String): HodorVar = {
			if (vars.contains(name)) {
				var v = vars.get(name)
				v match{
					case Some(c) => c
					case _ => throw new IllegalArgumentException("var is not defined")
				}				
			} else {
				parent match {
					case Some(scope) => scope.getVar(name)
					case _ => throw new IllegalArgumentException("var is not defined")
				}
			}
		}

		def defineVar (name: String) {
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
					case _ => throw new IllegalArgumentException("var is not defined")
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

	def evaluateProgram(program: HodorProgram): Any = {
		evaluateCodeBlock(program.codeBlock) match {
			case i: HodorInt => i.value
			case s: HodorString => s.value
			case b: HodorBoolean => b.value
			case n: HodorNone => None
		}
	}

	def evaluateCodeBlock(program: HodorCodeBlock): HodorVar = {
		localScope = Some(new Scope(localScope))
		var scope = localScope match {
			case Some(s) => s
			case _ => throw new IllegalArgumentException()
		}
		for (statement <- program.statementSequence) {
			scope.returnVal = statement match {
		 		case s: HodorFuncDecl => evaluateFunctionDeclare(s)
		 		case s: HodorVarDecl => evaluateVarDeclare(s)
		 		case s: HodorAssign => evaluateAssign(s)
		 		case s: HodorPrint => evaluatePrint(s)
		 		case e: HodorExpr => evaluateExpression(e)
		 		case b: HodorCodeBlock => evaluateCodeBlock(b)
		 	}
		}
		localScope = scope.parent
		scope.returnVal
	}

	def evaluateFunctionDeclare(funcDecl: HodorFuncDecl): HodorNone = {
		println(funcDecl)
		HodorNone()
	}

	def evaluateVarDeclare(varDecl: HodorVarDecl): HodorNone = {
		//println(varDecl)
	 	localScope match {
			case Some(scope) => scope.defineVar(varDecl.name)
			case _ => throw new IllegalArgumentException()
		}
		//printHashMap(localScope.vars)
		HodorNone()
	}

	def evaluateAssign(assign: HodorAssign): HodorVar = {
		//println(assign)
		val hodorVar = evaluateExpression(assign.hodorExpr)
		localScope match {
			case Some(scope) => scope.setVar(assign.name, hodorVar)
			case _ => throw new IllegalArgumentException()
		}
		//printHashMap(localScope.vars)
		hodorVar
	}

	def evaluateExpression(expr: HodorExpr): HodorVar = {
		//print(expr)
		var returnVal = expr match {
			case e: HodorStr => HodorString(e.str)
			case e: HodorNumber => HodorInt(e.n)
			case e: HodorTrue => HodorBoolean(true)
			case e: HodorNot => evaluateNot(e)
			case e: HodorAnd => evaluateAnd(e)
			case e: HodorOr => evaluateOr(e)
			case e: HodorGT => evaluateGT(e)
			case e: HodorLT => evaluateLT(e)
			case e: HodorEQ => evaluateEQ(e)
			case e: HodorVarExpr => localScope match {
				case Some(s) => s.getVar(e.name)
				case _ => throw new IllegalArgumentException("We've done fucked up")
			}
			case e: HodorAdd => evaluateAdd(e)
			case e: HodorSubtract => evaluateSubtract(e)
			case e: HodorMultiply => evaluateMultiply(e)
			case e: HodorDivide => evaluateDivide(e)
			case _ => throw new IllegalArgumentException("YOu done fucked up")
		}
		returnVal
	}
	
	def evaluateAdd(input: HodorAdd): HodorInt = {
		var a = 0
		for (v <- input.operands){
			val v2 = evaluateExpression(v)
			v2 match{
				case c: HodorInt => { a += c.value }
				case _ => throw new IllegalArgumentException("YOu done fucked up")
			}
		}
		HodorInt(a)
	}

	def evaluateSubtract(input: HodorSubtract): HodorInt = {
		//a very lazy solution for a fencepost problem 
		var aa = evaluateExpression(input.operands(0))
		var a  = 0 
		aa match{
			case c: HodorInt => { a = 2*c.value }
			case _ => throw new IllegalArgumentException("YOu done fucked up")
		}
		for (v <- input.operands){
			val v2 = evaluateExpression(v)
			v2 match{
				case c: HodorInt => { a -= c.value }
				case _ => throw new IllegalArgumentException("YOu done fucked up")
			}
		}
		HodorInt(a)
	}

	def evaluateMultiply(input: HodorMultiply): HodorInt = {
		var a = 1
		for (v <- input.operands){
			val v2 = evaluateExpression(v)
			v2 match{
				case c: HodorInt => { a *= c.value }
				case _ => throw new IllegalArgumentException("YOu done fucked up")
			}
		}
		HodorInt(a)
	}

	def evaluateDivide(input: HodorDivide): HodorInt = {
		//a very lazy solution for a fencepost problem 
		var aa = evaluateExpression(input.operands(0))
		var a = 0
		aa match{
			case c: HodorInt => { a = c.value*c.value }
			case _ => throw new IllegalArgumentException("YOu done fucked up")
		}
		for (v <- input.operands){
			val v2 = evaluateExpression(v)
			v2 match{
				case c: HodorInt => { a /= c.value }
				case _ => throw new IllegalArgumentException("YOu done fucked up")
			}
		}
		HodorInt(a)
	}

	def evaluatePrint(print: HodorPrint): HodorNone = {
		//println(print)
		var input = evaluateExpression(print.expr)
		input match {
			case i: HodorInt => println(i.value)
			case i: HodorBoolean => println(i.value)
			case i: HodorString => println(i.value)
			case i: HodorNone => throw new IllegalArgumentException("Hodor? (expression does not have value)")
		}
		HodorNone()
	}

	def evaluateNot(not: HodorNot): HodorBoolean = {
		val b = evaluateExpression(not.expr)
		b match{
			case c: HodorBoolean => (HodorBoolean(!c.value))
			case _ => throw new IllegalArgumentException("YOu done fucked up")
		}
	}
	
	def evaluateAnd(and: HodorAnd): HodorBoolean = {
		var collect = HodorBoolean(true)
		for (op <- and.operands){
			val b = evaluateExpression(op)
			b match{
				case c: HodorBoolean => {collect = HodorBoolean(collect.value && c.value)}
				case _ => throw new IllegalArgumentException("YOu done fucked up")
			}
		}
		collect
	}

	def evaluateOr(or: HodorOr): HodorBoolean = {
		var collect = HodorBoolean(false)
		for (op <- or.operands){
			val b = evaluateExpression(op)
			b match{
				case c: HodorBoolean => {collect = HodorBoolean(collect.value || c.value)}
				case _ => throw new IllegalArgumentException("YOu done fucked up")
			}
		}
		collect
	}

	def evaluateGT(gt: HodorGT): HodorBoolean = {
		var left = evaluateExpression(gt.left)
		var right = evaluateExpression(gt.right)
		(left, right) match{
			case (l: HodorInt, r: HodorInt) => HodorBoolean(l.value > r.value)
			case _ => throw new IllegalArgumentException("Hodor? (Can only compare Ints)")
		}
	}

	def evaluateLT(lt: HodorLT): HodorBoolean = {
		var left = evaluateExpression(lt.left)
		var right = evaluateExpression(lt.right)
		(left, right) match{
			case (l: HodorInt, r: HodorInt) => HodorBoolean(l.value < r.value)
			case _ => throw new IllegalArgumentException("Hodor? (Can only compare Ints)")
		}
	}

	def evaluateEQ(eq: HodorEQ): HodorBoolean = {
		var left = evaluateExpression(eq.left)
		var right = evaluateExpression(eq.right)
		(left, right) match{
			case (l: HodorInt, r: HodorInt) => HodorBoolean(l.value == r.value)
			case (l: HodorBoolean, r: HodorBoolean) => HodorBoolean(l.value == r.value)
			case (l: HodorString, r: HodorString) => HodorBoolean(l.value == r.value)
			case _ => throw new IllegalArgumentException("Hodor? (Cannot compare two values of different types)")
		}
	}

    def main(args: Array[String]): Unit = {
        for (file <- args) {
            val source = scala.io.Source.fromFile(file)
            val lines = try source.mkString finally source.close()
            println(lines)
            val parseResult: HodorParser.ParseResult[HodorProgram] = HodorParser.parse(parseProgram, lines);
            println(parseResult)
            val hodorProgram: HodorProgram = parseResult.get
            println(hodorProgram)
            println("return: " + evaluateProgram(hodorProgram))
        }
    }
}
