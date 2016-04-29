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

	def evaluateExpression(expr: HodorExpr): HodorVar = {
		print(expr)
		expr match{
			case e: HodorStr => HodorString(e.str)
			case e: HodorNumber => HodorInt(e.n)
			case e: HodorTrue => HodorBoolean(true)
			case e: HodorNot => evaluateNot(e)
			case e: HodorAnd => evaluateAnd(e)
			case e: HodorOr => evaluateOr(e)
			case e: HodorGT => evaluateGT(e)
			case _ => throw new IllegalArgumentException("YOu done fucked up")
		}
	}

	def evaluatePrint(print: HodorPrint) {
		println(print)
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
		var test = true
		var l =0
		var r =0
		left match{
			case c: HodorInt => { test = true; l = c.value }
			case _ => { test = false }
		}
		right match{
			case c: HodorInt => { test = true; r = c.value }
			case _ => { test = false }
		}
		if(!test){
			throw new IllegalArgumentException("YOu done fucked up")
		}
		HodorBoolean(l > r)
	}

	def evaluateLT(lt: HodorLT): HodorBoolean = {
		var left = evaluateExpression(lt.left)
		var right = evaluateExpression(lt.right)
		var test = true
		var l =0
		var r =0
		left match{
			case c: HodorInt => { test = true;  l = c.value}
			case _ => { test = false }
		}
		right match{
			case c: HodorInt => { test = true; r = c.value}
			case _ => { test = false }
		}
		if(!test){
			throw new IllegalArgumentException("YOu done fucked up")
		}
		HodorBoolean(l < r)
	}

	/*def evaluateEQ(eq: HodorEQ): HodorBoolean = {
		var left = evaluateExpression(eq.left)
		var right = evaluateExpression(eq.right)
		var test = true
		left match{
			case c: HodorVar => { test = true }
			case _ => { test = false }
		}
		right match{
			case c: HodorVar => { test = true }
			case _ => { test = false }
		}
		if(!test){
			throw new IllegalArgumentException("YOu done fucked up")
		}
		HodorBoolean(left.value == right.value)
	}*/

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
