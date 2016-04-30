import scala.collection.mutable.{ HashMap, Stack }
import scala.util.Random
import scala.math.{ min, max }
import scala.io.Source
import HodorParser._

object Hodor {
	var localScope: Option[Scope] = None

	class Scope (parentScope: Option[Scope]) {
		var funcs: HashMap[String, (List[String], HodorCodeBlock)] = 
			new HashMap[String, (List[String], HodorCodeBlock)]()
		var vars: HashMap[String, HodorVar] = new HashMap[String, HodorVar]
		var parent: Option[Scope] = parentScope
		var returnVal: HodorVar = HodorNone()

		def getFunc (name: String) : (List[String], HodorCodeBlock) = {
			if (funcs.contains(name)) {
				var v = funcs.get(name)
				v match{
					case Some(c) => c
					case _ => throw new IllegalArgumentException("function is not defined")
				}	
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

		def defineFunc (name: String, params: List[String], block: HodorCodeBlock) {
			funcs += (name -> (params, block))
		}

		def setVar (name: String, value: HodorVar): Unit = {
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
			case _ => throw new IllegalArgumentException("Hodor? (something wrong with scope)")
		}
		scope.returnVal = evaluateStatementSeq(program.statementSequence)
		localScope = scope.parent
		scope.returnVal
	}

	def evaluateFunctionDeclare(funcDecl: HodorFuncDecl): HodorNone = {
	 	localScope match {
			case Some(scope) => scope.defineFunc(funcDecl.name, funcDecl.vars, funcDecl.code)
			case _ => throw new IllegalArgumentException("Hodor? (something wrong with scope)")
		}
		HodorNone()
	}

	def evaluateFunctionCall(funcCall: HodorFuncCall): HodorVar = {
		localScope = Some(new Scope(localScope))
		var scope = localScope match {
			case Some(s) => s
			case _ => throw new IllegalArgumentException("Hodor? (something wrong with scope)")
		}
		var (params, block) = scope.getFunc(funcCall.name)
		if (params.size != funcCall.params.size) {
			throw new IllegalArgumentException("Hodor? (number of params does not match defintion)")
		}
		for (i <- 0 until params.size) {
			scope.defineVar(params(i))
			scope.setVar(params(i), evaluateExpression(funcCall.params(i)))
		}
		scope.returnVal = evaluateStatementSeq(block.statementSequence)
		localScope = scope.parent
		scope.returnVal
	}

	def evaluateStatementSeq(statementSeq: List[HodorStatement]): HodorVar = {
		var returnVal: HodorVar = HodorNone()
		for (statement <- statementSeq) {
			returnVal = statement match {
		 		case s: HodorFuncDecl => evaluateFunctionDeclare(s)
		 		case s: HodorVarDecl => evaluateVarDeclare(s)
		 		case s: HodorAssign => evaluateAssign(s)
		 		case s: HodorPrint => evaluatePrint(s)
		 		case e: HodorExpr => evaluateExpression(e)
		 		case b: HodorCodeBlock => evaluateCodeBlock(b)
		 		case c: HodorConditional => evaluateCond(c)
		 	}
		}
		returnVal
	}

	def evaluateVarDeclare(varDecl: HodorVarDecl): HodorNone = {
	 	localScope match {
			case Some(scope) => scope.defineVar(varDecl.name)
			case _ => throw new IllegalArgumentException()
		}
		HodorNone()
	}

	def evaluateAssign(assign: HodorAssign): HodorVar = {
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
		expr match {
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
			case e: HodorFuncCall => evaluateFunctionCall(e)
			case _ => throw new IllegalArgumentException("YOu done fucked up")
		}
	}

	def evaluateCond(cond: HodorConditional): HodorVar = {
		cond match {
			case i: HodorIf => evaluateIf(i.expr, i.thn)
			case ie: HodorIfElse => evaluateIf(ie.expr, ie.thn, ie.els)
			case wl: HodorLoop => evaluateLoop(wl.expr, wl.thn)
		}
	}

	def evaluateIf(expr: HodorExpr, thn: HodorCodeBlock): HodorVar = {
		isExpressionTrue(expr) match {
			case true => evaluateCodeBlock(thn)
			case false => HodorNone()
		}
	}

	def evaluateIf(expr: HodorExpr, thn: HodorCodeBlock, els: HodorCodeBlock): HodorVar = {
		isExpressionTrue(expr) match {
			case true => evaluateCodeBlock(thn)
			case false => evaluateCodeBlock(els)
		}
	}

	def isExpressionTrue(expr: HodorExpr): Boolean = {
		evaluateExpression(expr) match{
			case e: HodorBoolean => e.value
			case _ => throw new IllegalArgumentException("Hodor? (expr must evaluate to Boolean)")
		}
	}

	def evaluateLoop(expr: HodorExpr, thn: HodorCodeBlock): HodorVar = {
		while(isExpressionTrue(expr)){
			evaluateCodeBlock(thn)
		}
		HodorNone()
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
				case c: HodorBoolean => { collect = HodorBoolean(collect.value && c.value) }
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
				case c: HodorBoolean => { collect = HodorBoolean(collect.value || c.value) }
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
