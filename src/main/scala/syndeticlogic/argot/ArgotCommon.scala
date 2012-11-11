package syndeticlogic.argot

import scala.util.parsing.combinator._

import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

abstract class ArgotParseTree
case class ColumnName(s: String) extends ArgotParseTree
case class TableName(s: String) extends ArgotParseTree
case class ColumnList(columns: List[ColumnName]) extends ArgotParseTree
case class ValueList(values: List[Value]) extends ArgotParseTree

case class Value(value: Any) extends Reference
case class NullValue extends Value(null)
case class ArgotBooleanValue(b: Boolean) extends Value(b)
case class IntegralNumber(i: Long) extends Value(i)
case class RealNumber(d: Double) extends Value(d)
case class StringLiteral(s: String) extends Value(s)
case class ArgotObjectValue(obj: Map[String, Value]) extends Value(obj)
case class ArgotArray(array: List[Value]) extends Value(array)

abstract class Key extends ArgotParseTree
case class PrimaryKey extends Key
case class ForeignKey extends Key
case class IndexKey extends Key

abstract class ArgotType extends ArgotParseTree
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

abstract class ArgotCompoundType extends ArgotParseTree
case class Codeable(typeName: String, superType: String, typeList: List[ArgotType]) extends ArgotCompoundType
case class VectorDef(id: String, typeName: ArgotType) extends ArgotCompoundType
case class SingletonDef(typeName: String, typeList: List[ArgotType]) extends ArgotCompoundType
case class TableDef(typeName: String) extends ArgotCompoundType

abstract class Method extends ArgotParseTree
case class EqualsMethod(functionBody: List[Statement]) extends Method
case class CompartorMethod extends Method

abstract class Statement extends Method
case class ReturnStatement(b: Boolean) extends Statement
case class IfThenElseStatement(clauses: List[SubStatement]) extends Statement

abstract class SubStatement extends Statement
case class IfStatement(c: Condition, b: Block) extends SubStatement
case class ElseIfStatement(c: Condition, b: Block) extends SubStatement
case class ElseStatement(b: Block) extends SubStatement

abstract class SubStatementComponent extends SubStatement
case class Condition(f: BooleanFunction) extends SubStatementComponent
case class Block(l: List[Statement]) extends SubStatementComponent

abstract class BooleanFunction
case class Negation(f: BooleanFunction) extends BooleanFunction
abstract class Comparator extends BooleanFunction
case class Less(lhs: Reference, rhs: Reference)  extends Comparator
case class LessOrEqual(lhs: Reference, rhs: Reference) extends Comparator
case class EqualEqual(lhs: Reference, rhs: Reference) extends Comparator
case class NotEqual(lhs: Reference, rhs: Reference) extends Comparator
case class GreaterOrEqual(lhs: Reference, rhs: Reference) extends Comparator
case class Greater(lhs: Reference, rhs: Reference) extends Comparator
abstract class Connector extends BooleanFunction
case class And(lhs: BooleanFunction, rhs: BooleanFunction) extends Connector
case class Or(lhs: BooleanFunction, rhs: BooleanFunction) extends Connector

abstract class Reference
case class MemberReference(member: String) extends Reference
case class ArrayReference(name: String, index: String) extends Reference
case class QualifiedMemberReference(obj: String, member: Reference) extends Reference

abstract class InsertOption extends ArgotParseTree         
case class Delayed extends InsertOption
case class LowPriority extends InsertOption
case class HighPriority extends InsertOption
case class None extends InsertOption
case class InsertStmt(tableName: TableName, insertOption: InsertOption, columns: ColumnList, values: ValueList) extends ArgotParseTree 

trait Commons extends JavaTokenParsers {
  val NAME: Parser[String] = ident
  val columnName: Parser[ColumnName] = ident ^^ (id => ColumnName(id))
  val tableName: Parser[TableName] = ident ^^ (id => TableName(id))
}

