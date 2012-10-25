package syndeticlogic.argot.sql

import scala.util.parsing.combinator._
import java.io.FileReader;

class SQL extends JavaTokenParsers {
  
  abstract class Tree
  case class ColumnName(s: String) extends Tree
  case class TableName(s: String) extends Tree  
  case class InsertStmt(name: TableName, v: ColumnList) extends Tree
  case class ColumnList(columns: List[ColumnName]) extends Tree
    
  val INSERT: Parser[String] = """[iI][nN][sS][eE][rR][tT]""".r
  val INTO: Parser[String] = """[iI][nN][tT][oO]""".r
  val NAME: Parser[String] = ident
  val columnName: Parser[ColumnName] = ident ^^ (id => ColumnName(id))
  val tableName: Parser[TableName] = ident ^^ (id => TableName(id))
  
  def insertStmt: Parser[InsertStmt] = 
      INSERT~INTO~tableName~"("~columnList~");" ^^ { case insert~into~tname~"("~clist~");" => 
        InsertStmt(tname, ColumnList(clist))
      }
  
  def columnList: Parser[List[ColumnName]] = repsep(columnName, ",") ^^ (List() ++ _) |
      columnName ^^ (id => List[ColumnName](id)) 
}

object ParseSQL extends SQL {
  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(insertStmt, reader))
  }
}
