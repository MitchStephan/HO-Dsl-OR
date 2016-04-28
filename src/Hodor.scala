import scala.collection.mutable.{ HashMap, Stack }
import scala.util.Random
import scala.math.{ min, max }
import scala.io.Source
import HodorParser._

object Hodor {
	var localScope: Scope = new Scope(None);

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
			vars + (name -> Nil)
		}

		def defineFunc (name: String, func: Function[Array[HodorVar], HodorVar]) {
			funcs + (name -> func)
		}

		def updateVar (name: String, value: HodorVar) {
			if (vars.contains(name)) {
				vars(name) = value
			} else {
				parent match {
					case Some(scope) => scope.updateVar(name, value)
					case None => throw new IllegalArgumentException("var is not defined")
				}
			}
		}
	}

	abstract sealed class HodorVar
    case class HodorInt(value: Int) extends HodorVar
	case class HodorBoolean(value: Boolean) extends HodorVar
	case class HodorString(value: String) extends HodorVar

	abstract sealed class HodorLine
	case class Declare() extends HodorLine
	case class Assign() extends HodorLine
	case class Print() extends HodorLine

  def main(args: Array[String]): Unit = {
    for (file <- args) {
      val source = scala.io.Source.fromFile(file)
      val lines = try source.mkString finally source.close()
      println(HodorParser.parse(parseProgram, lines))
    }
  }
}
