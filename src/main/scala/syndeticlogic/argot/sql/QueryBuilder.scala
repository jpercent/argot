package syndeticlogic.argot.sql

class QueryBuilder {

  def columnName(c: ColumnName): String = c.s
  def tableName(t: TableName): String = t.s
  def columnList(cl: ColumnList): String = {
    var ret = "("
    for (i <- 1 to cl.columns.length) {
      if (i != cl.columns.length) ret += cl.columns(i) + ","
      else ret += cl.columns(i) + ")"
    }
    ret
  }

  def nullValue(n: NullValue): String = "null"
  def argotBoolean(b: ArgotBoolean): String = b.b.toString()
  def integralNumber(i: IntegralNumber): String = i.i.toString()
  def realNumber(r: RealNumber): String = r.d.toString()
  def stringLiteral(s: StringLiteral): String = s.s
  def argotObject(ao: ArgotObject): String = {
    var ret = ""
    val obj: Iterator[(String, Value)] = ao.obj.elements
    for (i <- 1 to ao.obj.size) {
      val entry = obj.next()
      ret += entry._1+":"
      if(i != obj.size) ret += matchValue(entry._2)+","
      else ret += matchValue(entry._2)
    }
    "{" + ret + "}"    
  }
  def argotArray(aa: ArgotArray): String = {
    var ret = ""
    for (i <- 1 to aa.array.length) {
      if(i != aa.array.length) ret += matchValue(aa.array(i))+","
      else ret += matchValue(aa.array(i))
    }
    "[" + ret + "]"
  }

  def insert(i: InsertStmt): String = buildQuery(i.tableName) + buildQuery(i.insertOption) + buildQuery(i.columns)
  
  def matchInsertOption(io: InsertOption): String = io match {
    case _: Delayed => "delayed"
    case _: HighPriority => "highpriority"
    case _: LowPriority => "lowpriority"
    case _: None => ""
  }
  
  def matchValue(v: Value): String = v match {
    case x: NullValue => nullValue(x)
    case x: ArgotBoolean => argotBoolean(x)
    case x: IntegralNumber => integralNumber(x)
    case x: RealNumber => realNumber(x)
    case x: StringLiteral => stringLiteral(x)
    case x: ArgotObject => argotObject(x)
    case x: ArgotArray => argotArray(x)
  }

  def buildQuery(t: Tree): String = t match {
    case x: InsertStmt => insert(x)
    case x: Value => matchValue(x)
    case x: InsertOption => matchInsertOption(x)
    case x: ColumnName => columnName(x)
    case x: TableName => tableName(x)
    case x: ColumnList => columnList(x)
  }  
}