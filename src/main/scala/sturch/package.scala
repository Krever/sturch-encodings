import scala.language.higherKinds
import scala.reflect.runtime.universe._


/**
  * Created by wpitula on 4/17/17.
  */
package object sturch {

  /**
    * Type Lambda
    */
  trait TL {
    type Apply[T <: TL] <: TL
  }

  type Identity = TL {
    type Apply[T <: TL] = T
  }
  type Const[A <: TL] = TL {
    type Apply[T <: TL] = A
  }

  sealed trait Expr

  case class Lambda(arg: Var, body: Expr) extends Expr

  case class Var(name: String) extends Expr

  case class Apply(fun: Expr, arg: Expr) extends Expr

  implicit class TLTypeOps(t: Type) {
    def getApply: Type = t.decl(TypeName("Apply")).asType.toType
  }

  def print[T: WeakTypeTag] = println(printExpr(parse(weakTypeOf[T])))

  def printExpr(expr: Expr): String = {
    expr match {
      case Var(v) => v
      case Lambda(Var(x), body) => s"λ$x.${printExpr(body)}"
      case Apply(fun, arg) => s"(${printExpr(fun)} ${printExpr(arg)})"
    }
  }

  def parse[T: WeakTypeTag]: Expr = parse(weakTypeOf[T])

  def parse(tp: Type): Expr = {
    val t = tp.dealias
    val apply = t.getApply
    if(apply.toString != apply.dealias.toString) {
      val arg = apply.typeArgs.head
      Lambda(Var(arg.toString), parse(apply))
    } else if (t.typeArgs.nonEmpty){
      val x = t.typeConstructor.toString
      val parts = x.split("#")
      require(parts(1) == "Apply")
      Apply(Var(parts(0)), parse(t.typeArgs.head))
    } else {
      Var(t.toString)
    }
  }
}
