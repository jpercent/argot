package syndeticlogic.argot.parser

import scala.util.parsing.combinator._

import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

sealed abstract class ArgotParseNode {
  // XXX - make this a property
  val debug = false
  val log = LogFactory.getLog("TransitionTracer")
  
  if(debug)
    trace
 
  def traceCallout = {}   
  def trace = 
    log.info(this.toString())
    traceCallout
}

case class ColumnName(s: String) extends ArgotParseNode
case class TableName(s: String) extends ArgotParseNode
case class ColumnList(columns: List[ColumnName]) extends ArgotParseNode
case class ValueList(values: List[Value]) extends ArgotParseNode

abstract class Reference extends ArgotParseNode
case class Value(value: Any) extends Reference
case class NullValue extends Value(null)
case class ArgotBooleanValue(b: Boolean) extends Value(b)
case class IntegralNumber(i: Long) extends Value(i)
case class RealNumber(d: Double) extends Value(d)
case class StringLiteral(s: String) extends Value(s)
case class ArgotObjectValue(obj: Map[String, Value]) extends Value(obj)
case class ArgotArray(array: List[Value]) extends Value(array)

abstract class Key extends ArgotParseNode
case class PrimaryKey extends Key
case class ForeignKey extends Key
case class IndexKey extends Key
case class NoKey extends Key

abstract class StorageStrategy extends ArgotParseNode
case class Compose extends StorageStrategy
case class Decompose extends StorageStrategy

abstract class ArgotType extends ArgotParseNode 
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

abstract class ArgotCompoundType extends ArgotType

abstract class ArgotSpecialType extends ArgotCompoundType
case class VectorDef(typeName: String, id: String, key: Key) extends ArgotSpecialType
case class MapDef(keyName: String, valueName: String, id: String, key: Key) extends ArgotSpecialType
case class CodeableRef(typeName: String, id: String, storageStrategy: StorageStrategy, key: Key) extends ArgotSpecialType
//case class TypeDefinition(typeName: String, superType: String, typeList: List[ArgotType], method: Method, method1: Method) extends ArgotCompoundType
case class Codeable(typeName: String, superType: String, typeList: List[ArgotType], method: Method, method1: Method) extends ArgotCompoundType
case class SingletonDef(typeName: String, typeList: List[ArgotType]) extends ArgotCompoundType
case class TableDef(id: String, typeList: List[ArgotType]) extends ArgotCompoundType

abstract class MethodType extends ArgotParseNode
case class BooleanReturnType extends MethodType
case class TernaryReturnType extends MethodType

abstract class Method extends ArgotParseNode
case class MethodUndefined extends Method
case class EqualsMethod(functionBody: List[Statement], paramName: String) extends Method
case class CompareMethod(functionBody: List[Statement], paramName: String) extends Method

abstract class Statement extends Method
case class BooleanReturnStatement(b: Boolean) extends Statement
case class TernaryReturnStatement(value: CompareValue) extends Statement
case class IfThenElseStatement(clauses: List[SubStatement]) extends Statement
case class Foreach(block: Block) extends Statement

abstract class SubStatement extends Statement
case class IfStatement(c: Condition, b: Block) extends SubStatement
case class ElseIfStatement(c: Condition, b: Block) extends SubStatement
case class ElseStatement(b: Block) extends SubStatement

abstract class SubStatementComponent extends SubStatement
case class Condition(f: BooleanFunction) extends SubStatementComponent
case class Block(l: List[Statement]) extends SubStatementComponent

abstract trait BooleanFunction extends ArgotParseNode
case class Negation(f: BooleanFunction) extends BooleanFunction
abstract class Comparator extends BooleanFunction
case class Less(lhs: Reference, rhs: Reference)  extends Comparator
case class LessOrEqual(lhs: Reference, rhs: Reference) extends Comparator
case class EqualEqual(lhs: Reference, rhs: Reference) extends Comparator
case class NotEqual(lhs: Reference, rhs: Reference) extends Comparator
case class GreaterOrEqual(lhs: Reference, rhs: Reference) extends Comparator
case class Greater(lhs: Reference, rhs: Reference) extends Comparator
case class EqualsObject(q: QualifiedMemberReference) extends Comparator

abstract class Connector extends BooleanFunction
case class And(lhs: BooleanFunction, rhs: BooleanFunction) extends Connector
case class Or(lhs: BooleanFunction, rhs: BooleanFunction) extends Connector

case class MemberReference(member: String) extends Reference
case class VectorReference(id: String, index: String) extends Reference
case class MapReference(id: String, key: Reference) extends Reference
case class QualifiedMemberReference(obj: String, member: Reference) extends Reference

abstract class FunctionReference extends Reference with BooleanFunction
case class EqualsReference(param: Reference) extends FunctionReference
case class CompareReference(param: Reference) extends FunctionReference

case class CompareValue extends Reference
case class LessValue extends CompareValue
case class EqualValue extends CompareValue
case class GreaterValue extends CompareValue 

abstract class InsertOption extends ArgotParseNode         
case class Delayed extends InsertOption
case class LowPriority extends InsertOption
case class HighPriority extends InsertOption
case class NoOption extends InsertOption
case class InsertStmt(tableName: TableName, insertOption: InsertOption, columns: ColumnList, values: ValueList) extends ArgotParseNode 

trait Commons extends JavaTokenParsers {
  val NAME: Parser[String] = ident
  val columnName: Parser[ColumnName] = ident ^^ (id => ColumnName(id))
  val tableName: Parser[TableName] = ident ^^ (id => TableName(id))
  val log: Log = LogFactory.getLog("ExpansionTracer")
  // XXX - make this a property
  val debug = false
  
  def traceCallout() = {}
  
  def trace(s: String) = {
    if(debug) log.info(s)
    traceCallout()
  }
}

abstract class ArgotBuilder {
  def build(t: ArgotParseNode): String;
} 

trait ValueBuilder {

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

  def matchValue(v: Value): String = v match {
    case x: NullValue => nullValue(x)
    case x: ArgotBooleanValue => argotBoolean(x)
    case x: IntegralNumber => integralNumber(x)
    case x: RealNumber => realNumber(x)
    case x: StringLiteral => stringLiteral(x)
    case x: ArgotObjectValue => argotObject(x)
    case x: ArgotArray => argotArray(x)
  }
}




