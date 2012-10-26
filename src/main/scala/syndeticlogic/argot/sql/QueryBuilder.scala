package syndeticlogic.argot.sql

class QueryBuilder {

  def columnName(c: ColumnName): String = c.s
  def tableName(t: TableName): String = t.s
  
  def columnList(cl: ColumnList): String = "("+columns(cl.columns, 1)+")"
  def columns(c: List[ColumnName], i: Int): String = {
    if(i == c.length) c(i).s
    else c(i).s + ","+ columns(c, i+1) 
  } 

  def nullValue(n: NullValue): String = "null"
  def argotBoolean(b: ArgotBoolean): String = b.b.toString()
  def integralNumber(i: IntegralNumber): String = i.i.toString()
  def realNumber(r: RealNumber): String = r.d.toString()
  def stringLiteral(s: StringLiteral): String = s.s
  
  def argotObject(ao: ArgotObject): String = "{"+members(ao.obj.elements, ao.obj.size, 1)+"}"
  def members(objiter: Iterator[(String, Value)], size: Int, i: Int): String = {
    val entry = objiter.next()
    if(size == i) entry._1 +":"+matchValue(entry._2) +","
    else entry._1 +":"+matchValue(entry._2)+members(objiter, size, i+1)
  }

  def argotArray(aa: ArgotArray): String = "["+elements(aa.array, 1)+"]"
  def elements(array: List[Value], i: Int): String = {
    if(i == array.length) matchValue(array(i))
    else matchValue(array(i)) + ","+elements(array, i+1)
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