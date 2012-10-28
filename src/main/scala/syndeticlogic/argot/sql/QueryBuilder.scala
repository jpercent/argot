package syndeticlogic.argot.sql

class QueryBuilder {

  def columnName(c: ColumnName): String = c.s
  def tableName(t: TableName): String = t.s
  def columnList(cl: ColumnList): String = {
    if(cl.columns.length == 0) ""
    else "("+columns(cl.columns, 0)+")"
  }
  def columns(c: List[ColumnName], i: Int): String = {
    if(i == c.length-1) c(i).s
    else c(i).s + ","+ columns(c, i+1) 
  } 

  def valueList(vl: ValueList): String = {
    println("values list = "+vl.values)
    "("+values(vl.values,0)+")"
  }
  def values(v: List[Value], i: Int): String = {
    if(i == v.length-1) matchValue(v(i))
    else matchValue(v(i)) + ","+ values(v, i+1) 
  }
  
  def nullValue(n: NullValue): String = "null"
  def argotBoolean(b: ArgotBooleanValue): String = b.b.toString()
  def integralNumber(i: IntegralNumber): String = i.i.toString()
  def realNumber(r: RealNumber): String = r.d.toString()
  def stringLiteral(s: StringLiteral): String = s.s
  
  def argotObject(ao: ArgotObjectValue): String = "{"+members(ao.obj.elements, ao.obj.size, 0)+"}"
  def members(objiter: Iterator[(String, Value)], size: Int, i: Int): String = {
    val entry = objiter.next()
    if(i == size-1) entry._1 +":"+matchValue(entry._2) +","
    else entry._1 +":"+matchValue(entry._2)+members(objiter, size, i+1)
  }

  def argotArray(aa: ArgotArray): String = "["+elements(aa.array, 0)+"]"
  def elements(array: List[Value], i: Int): String = {
    if(i == array.length-1) matchValue(array(i))
    else matchValue(array(i)) + ","+elements(array, i+1)
  }
 
  def insert(i: InsertStmt): String = {
    "insert into "+buildQuery(i.tableName)+buildQuery(i.insertOption)+buildQuery(i.columns)+" values "+buildQuery(i.values)
  }
 
  def matchInsertOption(io: InsertOption): String = io match {
    case _: Delayed => "delayed"
    case _: HighPriority => "highpriority"
    case _: LowPriority => "lowpriority"
    case _: None => ""
  }
  
  def matchValue(v: Value): String = v match {
    case x: NullValue => nullValue(x)
    case x: ArgotBooleanValue => argotBoolean(x)
    case x: IntegralNumber => integralNumber(x)
    case x: RealNumber => realNumber(x)
    case x: StringLiteral => stringLiteral(x)
    case x: ArgotObjectValue => argotObject(x)
    case x: ArgotArray => argotArray(x)
  }

  def buildQuery(t: Tree): String = t match {
    case x: InsertStmt => insert(x)
    case x: Value => matchValue(x)
    case x: InsertOption => matchInsertOption(x)
    case x: ColumnName => columnName(x)
    case x: TableName => tableName(x)
    case x: ColumnList => columnList(x)
    case x: ValueList => valueList(x)
  }  
}