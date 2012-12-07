package syndeticlogic.argot.parser

import scala.util.parsing.combinator._

import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
// vector and map are special because they can be used in declarations of types and as definitions of storage objects.
trait SpecialTypes extends Commons {
    val VECTOR: Parser[String] = "vector"
    val MAP: Parser[String] = "map"
    def vector: Parser[VectorDef] = {
      VECTOR~"["~>NAME~"]"~NAME ^^ {case typeName~rbracket~id => VectorDef(typeName, id, NoKey()) }
    }
    def map: Parser[MapDef] = {
      MAP~"["~>NAME~","~NAME~"]"~NAME ^^ {case keyName~comma~valueName~rbracket~id => MapDef(keyName, valueName, id, NoKey())}
    }
}

trait Types extends Commons with SpecialTypes {
  val TYPE: Parser[String] = "type"
  val BOOLEAN: Parser[String]  = "boolean"
  val BYTE: Parser[String] = "byte"
  val CHAR: Parser[String] = "char"
  val SHORT: Parser[String] = "short"
  val INTEGER: Parser[String] = "integer"
  val LONG: Parser[String] = "long"
  val FLOAT: Parser[String] = "float"
  val DOUBLE: Parser[String] = "double"
  val STRING: Parser[String] = "string"
  val BINARY: Parser[String] = "binary"
  val CODEABLE: Parser[String] = "codeable"
  val DECOMPOSE: Parser[String] = "decompose"
  val PRIMARY: Parser[String] = "primary"
  val FOREIGN: Parser[String] = "foreign"
  val INDEX: Parser[String] = "index"
 
  def tableTypeList: Parser[List[ArgotType]] = repsep(argotTableType, ",") ^^ (List() ++ _) |
      argotTableType ^^ (id => List[ArgotType](id))
  
  def argotTableType: Parser[ArgotType] ={
    argotType~opt(key) ^^ {
      case atype~Some(key) => {atype match { 
        case x: ArgotTypeType => ArgotTypeType(x.id, key) 
        case x: ArgotBoolean => ArgotBoolean(x.id, key) 
        case x: ArgotByte => ArgotByte(x.id, key) 
        case x: ArgotChar => ArgotChar(x.id, key) 
        case x: ArgotShort => ArgotShort(x.id, key) 
        case x: ArgotInteger => ArgotInteger(x.id, key) 
        case x: ArgotLong => ArgotLong(x.id, key) 
        case x: ArgotFloat => ArgotFloat(x.id, key) 
        case x: ArgotDouble => ArgotDouble(x.id, key) 
        case x: ArgotString => ArgotString(x.id, key)         
        case x: ArgotBinary => ArgotBinary(x.id, key) 
        case x: CodeableRef => CodeableRef(x.typeName, x.id, x.storageStrategy, key)
        case x: VectorDef => VectorDef(x.typeName, x.id, key)
        case x: MapDef => MapDef(x.keyName, x.valueName, x.id, key)
        }
      }
      case atype~None => atype
    }
  }
      
  def typeList: Parser[List[ArgotType]] = repsep(argotType, ",") ^^ (List() ++ _) |
      argotType ^^ (id => List[ArgotType](id))

  def argotType: Parser[ArgotType] = {
    TYPE~NAME ^^ {case atype~name => ArgotTypeType(name, NoKey())} |
    BOOLEAN~NAME ^^ {case atype~name => ArgotBoolean(name, NoKey())} |
    BYTE~NAME ^^ {case atype~name => ArgotByte(name, NoKey())} |
    CHAR~NAME ^^ {case atype~name => ArgotChar(name, NoKey())} |
    SHORT~NAME ^^ {case atype~name => ArgotShort(name, NoKey())} |      
    INTEGER~NAME ^^ {case atype~name => ArgotInteger(name, NoKey())}|
    LONG~NAME ^^ {case atype~name => ArgotLong(name, NoKey())} |
    FLOAT~NAME ^^ {case atype~name => ArgotFloat(name, NoKey())} |      
    DOUBLE~NAME ^^ {case atype~name => ArgotDouble(name, NoKey())} |
    STRING~NAME ^^ {case atype~name => ArgotString(name, NoKey())} |
    BINARY~NAME ^^ {case atype~name => ArgotBinary(name,NoKey())} |
    CODEABLE~NAME~NAME~opt(DECOMPOSE) ^^ {
      case atype~name~name1~Some(decomposed) => CodeableRef(name, name1, Decompose(), NoKey())
      case atype~name~name1~None => CodeableRef(name, name1, Compose(), NoKey())      
    } |
    vector |
    map
  }
 
  def key: Parser[Key] = PRIMARY ^^ (x => PrimaryKey()) | FOREIGN ^^ (x => ForeignKey()) | INDEX ^^ (x => IndexKey())
}


trait CodeableObject extends Types with Values with SpecialTypes {
  val EXTENDS: Parser[String] = "extends"
  val AND: Parser[String] = "and"
  val OR: Parser[String] = "or"
  val EQUALS: Parser[String] = "equals"
  val COMPARE: Parser[String] = "compare"
  val FOREACH: Parser[String] = "foreach"
    // XXX - this is wrong
  val OTHER: Parser[String] = "(other)"
  val RETURN: Parser[String] = "return"
  val TRUE: Parser[String] = "true"
  val FALSE: Parser[String] = "false"
  val IF: Parser[String] = "if"
    // XXX - this seems wrong too
  val ELSEIF: Parser[String] = "elseif"
  val ELSE: Parser[String] = "else"
  
  def codeable: Parser[Codeable] = {
    CODEABLE~NAME~opt(optionalExtends)~"{"~typeList~opt(method)~opt(method)~"}" ^^ {
      case codeable~name~Some(supertype)~"{"~typelist~Some(optmethod)~Some(optmethod1)~"}" => Codeable(name, supertype, typelist, optmethod, optmethod1)
      case codeable~name~Some(supertype)~"{"~typelist~Some(optmethod)~None~"}" => Codeable(name, supertype, typelist, optmethod, MethodUndefined())
      case codeable~name~Some(supertype)~"{"~typelist~None~None~"}" => Codeable(name, supertype, typelist, MethodUndefined(), MethodUndefined())
      case codeable~name~None~"{"~typelist~Some(optmethod)~Some(optmethod1)~"}" => Codeable(name, "", typelist, optmethod, optmethod1)
      case codeable~name~None~"{"~typelist~Some(optmethod)~None~"}" => Codeable(name, "", typelist, optmethod, MethodUndefined())      
      case codeable~name~None~"{"~typelist~None~None~"}" => { Codeable(name, "", typelist, MethodUndefined(), MethodUndefined())}
    }
  }
  
  def method: Parser[Method] = { 
    equals | compare
  }
  
  def optionalExtends: Parser[String] = EXTENDS~NAME ^^ {case optextends~name => name}
  
  def equals: Parser[EqualsMethod] = {
    EQUALS~OTHER~"{"~>functionStmt<~"}" ^^ (functionstmt => EqualsMethod(functionstmt)) 
  }
  
  def compare: Parser[CompareMethod] = {
    COMPARE~OTHER~"{"~>functionStmt<~"}" ^^ (functionstmt => CompareMethod(functionstmt)) 
  }

  def functionStmt: Parser[List[Statement]] = {
    statement~opt(rep(statement)) ^^ {case stmt~optrepmultistmt => 
      if(optrepmultistmt.get != NoOption) List(stmt) ++ optrepmultistmt.get 
      else List(stmt)
    }
  }
  
  def statement: Parser[Statement] = {
    ifStatement | booleanReturnStatement | ternaryReturnStatement
  }
  
  def ifStatement: Parser[IfThenElseStatement] = 
    singleIf~opt(elseIfStmt)~opt(elseStmt) ^^ {case ifstmt~elseifstmt~elsestmt => {
      if(elseifstmt.get != NoOption && elsestmt.get != NoOption) {
        val clauses = ifstmt :: elseifstmt.get :: elsestmt.get :: List[SubStatement]()
        IfThenElseStatement(clauses)
      } else if(elseifstmt.get != NoOption) {
        val clauses = ifstmt :: elseifstmt.get :: List[SubStatement]()
        IfThenElseStatement(clauses)
      } else if(elsestmt.get != NoOption) {
        val clauses = ifstmt :: elsestmt.get :: List[SubStatement]()
        IfThenElseStatement(clauses)
      } else {
        val clauses = ifstmt :: List[SubStatement]()
        IfThenElseStatement(clauses)
      }
    }
  }
 
  def singleIf: Parser[IfStatement] = { IF~"("~>condition~")"~"{"~block<~"}" ^^ 
    {case conditional~")"~"{"~block => IfStatement(conditional, Block(block))}
  }
  
  def elseIfStmt: Parser[ElseIfStatement] = { ELSEIF~"("~>condition~")"~"{"~block<~"}" ^^ 
    {case conditional~")"~"{"~block => ElseIfStatement(conditional, Block(block))}    
  }
  
  def elseStmt: Parser[ElseStatement] = { ELSE~"{"~>block<~"}" ^^ (b => ElseStatement(Block(b)))}
   
  def booleanReturnStatement: Parser[BooleanReturnStatement] = {
    RETURN~>TRUE ^^ { _ => BooleanReturnStatement(true) } | 
    RETURN~>FALSE ^^ {_ => BooleanReturnStatement(false) }
  }
  
  def ternaryReturnStatement: Parser[TernaryReturnStatement] = {
    RETURN~>"-1" ^^ (x => TernaryReturnStatement(-1)) | 
    RETURN~>"0" ^^ (x => TernaryReturnStatement(0)) |
    RETURN~>"1" ^^ (x => TernaryReturnStatement(1))
  }

  def condition: Parser[Condition] = {
    booleanFunction ^^ { (x => Condition(x)) }
  }
  
  def booleanFunction: Parser[BooleanFunction] = {
    "!"~"("~>booleanFunction<~")" ^^ { (x => Negation(x)) } |
    booleanFunction~AND~booleanFunction ^^ { case lhs~and~rhs => And(lhs, rhs) }
    booleanFunction~OR~booleanFunction ^^ { case lhs~or~rhs => Or(lhs, rhs) } 
    "("~>booleanFunction<~")" |
    reference~"<"~reference ^^ { case lhs~less~rhs => Less(lhs, rhs) }
    reference~"<="~reference ^^ { case lhs~lesseq~rhs => LessOrEqual(lhs, rhs) } |
    reference~"=="~reference ^^ { case lhs~eqeq~rhs => EqualEqual(lhs, rhs) } |
    reference~"!="~reference ^^ { case lhs~noteq~rhs => NotEqual(lhs, rhs) } |     
    reference~">="~reference ^^ { case lhs~greq~rhs => Greater(lhs, rhs) } |
    reference~">"~reference ^^ { case lhs~less~rhs => GreaterOrEqual(lhs, rhs) }
  }
  
  def reference: Parser[Reference] = {
    memberReference |
    value
  }
  
  def memberReference: Parser[Reference] = {
    objectMemberReference |
    vectorReference |
    mapReference |
    NAME ^^ {(x => MemberReference(x))}
  }
  
  def objectMemberReference: Parser[QualifiedMemberReference] = {
    NAME~"."~functionReference ^^ {
      case obj~dot~member => QualifiedMemberReference(obj, member)
    } |
    NAME~"."~memberReference ^^ {
      case obj~dot~member => QualifiedMemberReference(obj, member)
    }
  }
  
  def functionReference: Parser[FunctionReference] = {
    EQUALS~"("~>memberReference<~")" ^^ (EqualsReference(_)) |
    COMPARE~"("~>memberReference<~")" ^^ (CompareReference(_)) |
    FOREACH~>block ^^ (stmts => Foreach(Block(stmts)))
  }
  
  def vectorReference: Parser[VectorReference] = {
    NAME~"["~wholeNumber~"]" ^^ {case id~bracket~wholenumber~bracket1 => VectorReference(id, wholenumber)}
  }
  
  def mapReference: Parser[MapReference] = {
    NAME~"["~reference~"]" ^^ {case id~bracket~ref~bracket1 => MapReference(id, ref)}
  }
  
  def block: Parser[List[Statement]] = {
    "{"~>repsep(statement, ",")<~"}" ^^ (List() ++ _)
  }
}

trait Object extends CodeableObject {
  val OBJECT: Parser[String] = "object"
  def singleton: Parser[SingletonDef] = {
      OBJECT~NAME~"{"~typeList~"}" ^^ {
        case singleton~name~"{"~typelist~"}" => SingletonDef(name, typelist)
      }
  }
}

trait Table extends Types  {
  val TABLE: Parser[String] = "table"
  def table: Parser[TableDef] = {
    TABLE~>NAME~"{"~tableTypeList<~"}" ^^ {case id~"{"~typelist => TableDef(id, typelist)}
  }
}

trait DDL extends JavaTokenParsers with Table with SpecialTypes with Object {
}
