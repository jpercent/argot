package syndeticlogic.argot.sql

import scala.util.parsing.combinator._
import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

abstract class Tree
case class ColumnName(s: String) extends Tree
case class TableName(s: String) extends Tree
case class ColumnList(columns: List[ColumnName]) extends Tree
case class ValueList(values: List[Value]) extends Tree

case class Value(value: Any) extends Tree
case class NullValue() extends Value(null)
case class ArgotBoolean(b: Boolean) extends Value(b)
case class IntegralNumber(i: Long) extends Value(i)
case class RealNumber(d: Double) extends Value(d)
case class StringLiteral(s: String) extends Value(s)
case class ArgotObject(obj: Map[String, Value]) extends Value(obj)
case class ArgotArray(array: List[Value]) extends Value(array)

abstract class InsertOption extends Tree         
case class Delayed extends InsertOption
case class LowPriority extends InsertOption
case class HighPriority extends InsertOption
case class None extends InsertOption
case class InsertStmt(tableName: TableName, insertOption: InsertOption, columns: ColumnList, values: ValueList) extends Tree 

trait Commons extends JavaTokenParsers {
  val NAME: Parser[String] = ident
  val columnName: Parser[ColumnName] = ident ^^ (id => ColumnName(id))
  val tableName: Parser[TableName] = ident ^^ (id => TableName(id))

}

