package syndeticlogic.argot.parser

object ATree {
  abstract class ATree
  case class Sum(l: ATree, r: ATree) extends ATree
  case class Var(n: String) extends ATree
  case class Const(v: Int) extends ATree
  type Environment = String => Int

  def eval(t: ATree, env: Environment): Int = t match {
    case Sum(l, r) => eval(l, env) + eval(r, env)
    case Var(n) => env(n)
    case Const(v) => v
  }

  def derive(t: ATree, v: String): ATree = t match {
    case Sum(l, r) => Sum(derive(l, v), derive(r, v))
    case Var(n) if (n == v) => Const(1)
    case _ => Const(0)
  }

  def main(args: scala.Array[String]): Unit = {
    val exp: ATree = Sum(Sum(Var("x"), Var("x")), Sum(Const(7), Var("y")))
    val env: Environment = { case "x" => 7 case "y" => 8 }
    println("Expression: " + exp)
    println("Eval with x=7 and y=8: " + eval(exp, env))
    println("Derive w.r.t. x:  " + derive(exp, "x"))
    println("Eval derive w.r.t. x:  " + eval(derive(exp, "x"), env))
    try {
      val date: Date = new Date(32, 13, 111)
    } catch {
      case e: Exception =>
        println("exception ")
        e.printStackTrace()
    }

    try {
      val date: Date = new Date(0, 0, 111)
    } catch {
      case e: Exception =>
        println("exception ")
        e.printStackTrace()
    }
  
    try {
      val date: Date = new Date(0, 0, 111)
    } catch {
      case e: Exception =>
        println("exception ")
        e.printStackTrace()
    }      
    val date: Date = new Date(2,29,2012)
    val date1: Date = new Date(2, 28, 2012)
    println("FINISHED CORECTLY"+(date < date1)+"; "+(date > date1))
  }
}

trait Ord {
  def <(that: Any): Boolean
  def <=(that: Any): Boolean = (this < that) || (this == that)
  def >(that: Any): Boolean = !(this <= that)
  def >=(that: Any): Boolean = !(this < that)
}

class Date(m: Int, d: Int, y: Int) extends Ord {
  require(d > 0 && y > 0)
  require(monthRequirements)

  def day = d
  def month = m
  def year = y

  def monthRequirements: Boolean = m match {
    case 1 => d <= 31
    case 2 => y % 4 match {
      case 0 => d <= 29
      case _ => d < 28
    }
    case 3 => d <= 31
    case 4 => d <= 30
    case 5 => d <= 31
    case 6 => d <= 30
    case 7 => d <= 31
    case 8 => d <= 31
    case 9 => d <= 30
    case 10 => d <= 31
    case 11 => d <= 30
    case 12 => d <= 31
    case _ => false
  }

  override def toString(): String = year + "-" + month + "-" + day
  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Date] && {
      val o = that.asInstanceOf[Date]
      o.day == day && o.month == month && o.year == year
    }
  }

  override def <(that: Any): Boolean = {
    if (!that.isInstanceOf[Date])
      error("Cannot compare " + that + " and a Data")
    val o = that.asInstanceOf[Date]
    (year < o.year) ||
      (year == o.year && (month < o.month || (month == o.month && day < o.day)))
  }

}