package syndeticlogic.argot

class DDLBuilder extends ArgotBuilder with ValueBuilder {
  
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
 
  def insert(i: InsertStmt): String = {
    "insert into "+build(i.tableName)+build(i.insertOption)+build(i.columns)+" values "+build(i.values)
  }
 
  def matchInsertOption(io: InsertOption): String = io match {
    case _: Delayed => "delayed"
    case _: HighPriority => "highpriority"
    case _: LowPriority => "lowpriority"
    case _: None => ""
  }
  
  override def build(t: ArgotParseTree): String = t match {
    case x: InsertStmt => insert(x)
    case x: Value => matchValue(x)
    case x: InsertOption => matchInsertOption(x)
    case x: ColumnName => columnName(x)
    case x: TableName => tableName(x)
    case x: ColumnList => columnList(x)
    case x: ValueList => valueList(x)
  }  
}

