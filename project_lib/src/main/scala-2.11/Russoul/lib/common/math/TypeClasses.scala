package Russoul.lib.common.math

import scala.language.implicitConversions

/**
  * Created by russoul on 18.05.17.
  */
object TypeClasses {


  trait Addable[A] {
    @inline def plus(x: A, y:A): A

    override def toString: String

    class AddableOps(lhs: A)(implicit ev: Addable[A]){
      def +(rhs: A): A = ev.plus(lhs, rhs)
    }

    //implicit def mkAddableOps(lhs: A): AddableOps = new AddableOps(lhs)
  }

  object Addable {
    object Implicits{
      implicit def infixAddableOps[A](x: A)(implicit num: Addable[A]): Addable[A]#AddableOps = new num.AddableOps(x)
    }
  }

  trait CommutativeGroupLike[A] extends Addable[A] with Ordering[A]{
    @inline def negate(x: A): A
    @inline def zero: A

    @inline def abs(x:A): A = {
      val neg = negate(x)

      if(lt(x,neg)){
        neg
      }else
        x
    }

    @inline def minus(x: A, y: A): A = plus(x, negate(y))

    class CommutativeGroupOps(lhs: A)(implicit ev: CommutativeGroupLike[A]) extends AddableOps(lhs){
      def -(rhs: A): A = ev.minus(lhs, rhs)
      def unary_-(): A = ev.negate(lhs)

    }

    //implicit def mkCommutativeGroupOps(lhs: A): CommutativeGroupOps = new CommutativeGroupOps(lhs)

  }

  object CommutativeGroupLike {
    object Implicits{
      implicit def infixCommutativeGroupLikeOps[A](x: A)(implicit num: CommutativeGroupLike[A]): CommutativeGroupLike[A]#CommutativeGroupOps = new num.CommutativeGroupOps(x)


    }
  }

  trait FieldLike[A] extends CommutativeGroupLike[A]{
    @inline def times(x: A, y: A): A
    @inline def inv(x: A): A
    @inline def one:A
    @inline def sqrt(x:A) : A
    @inline def atan2(x:A, y:A) : A
    @inline def pow(x:A, y:A) : A
    @inline def toRadians(x:A): A
    @inline def cos(x:A): A
    @inline def sin(x:A): A

    @inline def fromDouble(x: Double) : A

    @inline def div(x: A, y: A): A = times(x, inv(y))

    //def Pi: A


    class FieldOps(lhs: A)(implicit ev: FieldLike[A]) extends CommutativeGroupOps(lhs){
      def *(rhs: A) : A = ev.times(lhs, rhs)
      def /(rhs: A) : A = ev.div(lhs, rhs)



    }

    //implicit def mkFieldOps(lhs: A): FieldOps = new FieldOps(lhs)
  }

  object FieldLike {
    object Implicits{
      implicit def infixFieldLikeOps[A](x: A)(implicit num: FieldLike[A]): FieldLike[A]#FieldOps = new num.FieldOps(x)

      //TODO this results in a cast overhead !!!
      @inline implicit def toField[A](x:Double)(implicit ev: FieldLike[A]): A = ev.fromDouble(x)

      implicit class DoubleToField(x:Double){
        @inline def toField[A](implicit ev: FieldLike[A]): A = ev.fromDouble(x)
      }
    }
  }


  trait IntIsCommutativeGroup extends CommutativeGroupLike[Int]{
    override def negate(x: Int): Int = -x
    override def zero: Int = 0
    override def plus(x: Int, y: Int): Int = x + y

    override def toString: String = {
      "Int"
    }

  }

  trait FloatIsField extends FieldLike[Float]{
    override def times(x: Float, y: Float): Float = x * y
    override def inv(x: Float): Float = 1/x
    override def one: Float = 1F
    override def negate(x: Float): Float = -x
    override def zero: Float = 0F
    override def plus(x: Float, y: Float): Float = x + y
    override def sqrt(x: Float): Float = math.sqrt(x).toFloat
    override def atan2(x: Float, y:Float): Float = math.atan2(x,y).toFloat
    override def pow(x: Float, y:Float): Float = math.pow(x,y).toFloat



    //override val Pi = 3.14159265358979323846F //TODO bad idea ? BAD IDEA !

    override def cos(x: Float): Float = math.cos(x).toFloat
    override def sin(x: Float): Float = math.sin(x).toFloat

    override def toRadians(x: Float): Float = {
      math.toRadians(x).toFloat
    }

    override def fromDouble(x: Double): Float = {
      x.toFloat
    }

    override def toString: String = {
      "Float"
    }

  }

  trait DoubleIsField extends FieldLike[Double]{
    override def times(x: Double, y: Double): Double = x * y
    override def inv(x: Double): Double = 1/x
    override def one: Double = 1D
    override def negate(x: Double): Double = -x
    override def zero: Double = 0D
    override def plus(x: Double, y: Double): Double = x + y
    override def sqrt(x: Double): Double = math.sqrt(x)
    override def atan2(x: Double, y:Double): Double = math.atan2(x,y)
    override def pow(x: Double, y:Double): Double = math.pow(x,y)



    override def cos(x: Double): Double = math.cos(x)

    override def sin(x: Double): Double = math.sin(x)

    override def toRadians(x: Double): Double = math.toRadians(x)

    //override val Pi = 3.14159265358979323846D
    override def fromDouble(x: Double): Double = x
  }

  implicit object IntIsCommutativeGroup extends IntIsCommutativeGroup with Ordering.IntOrdering
  implicit object FloatIsField extends FloatIsField with Ordering.FloatOrdering
  implicit object DoubleIsField extends DoubleIsField with Ordering.DoubleOrdering

  override def toString: String = {
    "Double"
  }

}
