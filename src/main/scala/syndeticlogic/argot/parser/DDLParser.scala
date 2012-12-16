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
      VECTOR~"["~>NAME~"]"~NAME ^^ {case typeName~rbracket~id => VectorDef(typeName, id, NoKey()) 
      }
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
  val AND: Parser[String] = "&&"
  val OR: Parser[String] = "||"
  val EQUALS: Parser[String] = "equals"
  val COMPARE: Parser[String] = "compare"
  val FOREACH: Parser[String] = "foreach"

  val RETURN: Parser[String] = "return"
  val TRUE: Parser[String] = "true"
  val FALSE: Parser[String] = "false"
  val IF: Parser[String] = "if"
  val ELSEIF: Parser[String] = "else if"
  val ELSE: Parser[String] = "else"
  
  def codeable: Parser[Codeable] = {
    trace("codeable")
    CODEABLE~NAME~opt(optionalExtends)~"{"~typeList~opt(method)~opt(method)~"}" ^^ {
      case codeable~name~Some(supertype)~"{"~typelist~Some(optmethod)~Some(optmethod1)~"}" => Codeable(name, supertype, typelist, optmethod, optmethod1)
      case codeable~name~Some(supertype)~"{"~typelist~Some(optmethod)~None~"}" => Codeable(name, supertype, typelist, optmethod, MethodUndefined())
      case codeable~name~Some(supertype)~"{"~typelist~None~None~"}" => Codeable(name, supertype, typelist, MethodUndefined(), MethodUndefined())
      case codeable~name~None~"{"~typelist~Some(optmethod)~Some(optmethod1)~"}" => Codeable(name, "", typelist, optmethod, optmethod1)
      case codeable~name~None~"{"~typelist~Some(optmethod)~None~"}" => Codeable(name, "", typelist, optmethod, MethodUndefined())      
      case codeable~name~None~"{"~typelist~None~None~"}" => { Codeable(name, "", typelist, MethodUndefined(), MethodUndefined())}
    }
  }
  
  def optionalExtends: Parser[String] = EXTENDS~NAME ^^ {case optextends~name => name}

  def method: Parser[Method] = equals | compare
    
  def equals: Parser[EqualsMethod] = {
    trace("equals")
    EQUALS~>"("~NAME~")"~"{"~functionStmt(BooleanReturnType())<~"}" ^^ { case op~name~cp~ob~functionstmt => EqualsMethod(functionstmt, name) } 
  }
  
  def compare: Parser[CompareMethod] = {
    trace("compare")
    COMPARE~>"("~NAME~")"~"{"~functionStmt(TernaryReturnType())<~"}" ^^ { case op~name~cp~ob~functionstmt => CompareMethod(functionstmt, name) } 
  }

  def functionStmt(t: MethodType): Parser[List[Statement]] = {
    statement(t)~opt(rep(statement(t))) ^^ {case stmt~optrepmultistmt => 
      if(optrepmultistmt.get != NoOption) List(stmt) ++ optrepmultistmt.get 
      else List(stmt)
    }
  }

  def statement(t: MethodType): Parser[Statement] = {
    trace("statement ")
    t match {
      case x: BooleanReturnType => ifStatement(t) | foreach(t) | booleanReturnStatement
      case x: TernaryReturnType => ifStatement(t) | foreach(t) | ternaryReturnStatement
    }
  }
  
  def foreach(t: MethodType): Parser[Statement] = {
    trace("foreach")
    FOREACH~>block(t) ^^ (stmts => Foreach(Block(stmts)))
  }
  
  def ifStatement(t: MethodType): Parser[IfThenElseStatement] = {
    trace("ifStatement")
    singleIf(t)~opt(elseIfStmts(t))~opt(elseStmt(t)) ^^ {
      case ifstmt~Some(elseifstmt)~Some(elsestmt) => {
        val clauses: List[SubStatement] = ifstmt :: elseifstmt ++ (elsestmt :: List[SubStatement]())
        IfThenElseStatement(clauses)
      }
      case ifstmt~Some(elseifstmt)~None => {
        val clauses = ifstmt :: elseifstmt ++ List[SubStatement]()
        IfThenElseStatement(clauses)
      }
      case ifstmt~None~Some(elsestmt) => {
        val clauses = ifstmt :: elsestmt :: List[SubStatement]()
        IfThenElseStatement(clauses)
      }
      case ifstmt~None~None => {
        val clauses = ifstmt :: List[SubStatement]()
        IfThenElseStatement(clauses)
      }
    }
  }
 
  def singleIf(t: MethodType): Parser[IfStatement] = {
    trace("singleIf")
    IF~"("~>condition~")"~"{"~block(t)<~"}" ^^ 
    {case conditional~")"~"{"~block => IfStatement(conditional, Block(block))}
  }
  
  def elseIfStmts(t: MethodType): Parser[List[ElseIfStatement]] = {
    trace("elseIfStmts")
    rep(elseIfStmt(t)) ^^ (List() ++ _) |
    elseIfStmt(t) ^^ (id => List[ElseIfStatement](id))
  }
  
  def elseIfStmt(t: MethodType): Parser[ElseIfStatement] = {
    trace("elseIfStmt")
    ELSEIF~"("~>condition~")"~"{"~block(t)<~"}" ^^ 
    {case conditional~")"~"{"~block => ElseIfStatement(conditional, Block(block))}
  }
  
  def elseStmt(t: MethodType): Parser[ElseStatement] = {
    trace("elseStmt")
    ELSE~"{"~>block(t)<~"}" ^^ (b => ElseStatement(Block(b)))}
   
  def booleanReturnStatement: Parser[BooleanReturnStatement] = {
    trace("booleanReturnStatement")
    RETURN~>TRUE ^^ { _ => BooleanReturnStatement(true) } | 
    RETURN~>FALSE ^^ {_ => BooleanReturnStatement(false) }
  }
  
  def ternaryReturnStatement: Parser[TernaryReturnStatement] = {
    trace("ternaryReturnStatement")
    RETURN~>"LESS" ^^ (x => TernaryReturnStatement(LessValue())) | 
    RETURN~>"EQUAL" ^^ (x => TernaryReturnStatement(EqualValue())) |
    RETURN~>"GREATER" ^^ (x => TernaryReturnStatement(GreaterValue()))
  }

  def condition: Parser[Condition] = {
    trace("condition")
    booleanFunction ^^ { (x => Condition(x)) }
  }
  
  def booleanFunction: Parser[BooleanFunction] = {
   trace("booleanFunction")
   "!"~"("~>booleanFunction<~")" ^^ { (x => Negation(x)) } |
   "("~>booleanFunction<~")" |  
    NAME~"."~functionReference ^^ { case name~dot~functionRef => EqualsObject(QualifiedMemberReference(name, functionRef)) } | 
    reference~"<"~reference ^^ { case lhs~less~rhs => Less(lhs, rhs) } |
    reference~"<="~reference ^^ { case lhs~lesseq~rhs => LessOrEqual(lhs, rhs) } |
    reference~"=="~reference ^^ { case lhs~eqeq~rhs => EqualEqual(lhs, rhs) } |
    reference~"!="~reference ^^ { case lhs~noteq~rhs => NotEqual(lhs, rhs) } |     
    reference~">="~reference ^^ { case lhs~greq~rhs => Greater(lhs, rhs) } |
    reference~">"~reference ^^ { case lhs~less~rhs => GreaterOrEqual(lhs, rhs) } |
    booleanFunction~AND~booleanFunction ^^ { case lhs~and~rhs => And(lhs, rhs) } |
    booleanFunction~OR~booleanFunction ^^ { case lhs~or~rhs => Or(lhs, rhs) }
  }
  
  def reference: Parser[Reference] = {
    trace("reference")
    memberReference |
    value
  }
  
  def memberReference: Parser[Reference] = {
    trace("memberReference")
    objectMemberReference |
    vectorReference |
    mapReference |
    NAME ^^ {(x => MemberReference(x))}
  }
  
  def objectMemberReference: Parser[QualifiedMemberReference] = {
    trace("objectMemberReference")
    NAME~"."~functionReference ^^ {
      case obj~dot~member => QualifiedMemberReference(obj, member)
    } |
    NAME~"."~memberReference ^^ {
      case obj~dot~member => QualifiedMemberReference(obj, member)
    }
  }
  
  def functionReference: Parser[FunctionReference] = {
    trace("functionReference")
    EQUALS~"("~>reference<~")" ^^ (EqualsReference(_)) |
    COMPARE~"("~>reference<~")" ^^ (CompareReference(_))
  }
  
  def vectorReference: Parser[VectorReference] = {
    trace("vectorReference")
    NAME~"["~wholeNumber~"]" ^^ {case id~bracket~wholenumber~bracket1 => VectorReference(id, wholenumber)}
  }
  
  def mapReference: Parser[MapReference] = {
    trace("mapReference")
    NAME~"["~reference~"]" ^^ {case id~bracket~ref~bracket1 => MapReference(id, ref)}
  }
  
  def block(t: MethodType): Parser[List[Statement]] = {
    trace("block")
    "{"~>repsep(statement(t), ",")<~"}" ^^ (List() ++ _) |
      statement(t) ^^ (id => List[Statement](id))
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
