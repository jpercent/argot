package syndeticlogic.argot

import scala.util.parsing.combinator._
import syndeticlogic.argot.(array;
import syndeticlogic.argot.(b:Bo;
import syndeticlogic.argot.(d:Do;
import syndeticlogic.argot.(i:Lo;
import syndeticlogic.argot.(obj:;
import syndeticlogic.argot.(s:St;
import syndeticlogic.argot.ArgotCompoundType;
import syndeticlogic.argot.ArgotType;
import syndeticlogic.argot.ColumnList;
import syndeticlogic.argot.ColumnName;
import syndeticlogic.argot.InsertOption;
import syndeticlogic.argot.Key;
import syndeticlogic.argot.List;
import syndeticlogic.argot.Map;
import syndeticlogic.argot.Parser;
import syndeticlogic.argot.TableName;
import syndeticlogic.argot.Tree;
import syndeticlogic.argot.Value;
import syndeticlogic.argot.ValueList;
import syndeticlogic.argot.extend;

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
case class NullValue extends Value(null)
case class ArgotBooleanValue(b: Boolean) extends Value(b)
case class IntegralNumber(i: Long) extends Value(i)
case class RealNumber(d: Double) extends Value(d)
case class StringLiteral(s: String) extends Value(s)
case class ArgotObjectValue(obj: Map[String, Value]) extends Value(obj)
case class ArgotArray(array: List[Value]) extends Value(array)

abstract class Key
case class PrimaryKey extends Key
case class ForeignKey extends Key
case class IndexKey extends Key

abstract class ArgotType
case class ArgotTypeType(id: String, key: Key) extends ArgotType
case class ArgotBoolean(id: String, key: Key) extends ArgotType
case class ArgotByte(id: String, key: Key) extends ArgotType
case class ArgotChar(id: String, key: Key) extends ArgotType
case class ArgotShort(id: String, key: Key) extends ArgotType
case class ArgotInteger(id: String, key: Key) extends ArgotType
case class ArgotLong(id: String, key: Key) extends ArgotType
case class ArgotFloat(id: String, key: Key) extends ArgotType
case class ArgotDouble(id: String, key: Key) extends ArgotType
case class ArgotString(id: String, key: Key) extends ArgotType
case class ArgotBinary(id: String, key: Key) extends ArgotType
case class CodeableRef(typeName: String, id: String, decomposed: String, key: Key) extends ArgotType

abstract class ArgotCompoundType
case class Codeable(typeName: String, typeList: List[ArgotType]) extends ArgotCompoundType
case class VectorDef(id: String, typeName: ArgotType) extends ArgotCompoundType
case class SingletonDef(typeName: String, typeList: List[ArgotType]) extends ArgotCompoundType
case class TableDef(typeName: String) extends ArgotCompoundType

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

