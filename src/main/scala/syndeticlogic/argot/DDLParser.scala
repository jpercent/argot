package syndeticlogic.argot

import scala.util.parsing.combinator._

import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
// vector and map are special because they can be used in declarations of types and as definitions of storage objects.
trait SpecialTypes extends Commons {
    val VECTOR: Parser[String] = """[vV][eE][cC][tT][oO][rR]""".r
    val MAP: Parser[String] = """[mM][aA][pP]""".r
    def vector: Parser[VectorDef] = {
      VECTOR~"["~>NAME~"]"~NAME ^^ {case typeName~rbracket~id => VectorDef(typeName, id) }
    }
    def map: Parser[MapDef] = {
      MAP~"["~>NAME~","~NAME~"]"~NAME ^^ {case keyName~comma~valueName~rbracket~id => MapDef(keyName, valueName, id)}
    }
} 

trait Types extends Commons with SpecialTypes {
  val TYPE: Parser[String] = """[tT][yY][pP][eE]""".r
  val BOOLEAN: Parser[String]  = """[bB][oO][oO][lL][eE][aA][nN]""".r
  val BYTE: Parser[String] = """[bB][yY][tT][eE]""".r
  val CHAR: Parser[String] = """[cC][hH][aA][rR]""".r
  val SHORT: Parser[String] = """[sS][hH][oO][rR][tT]""".r
  val INTEGER: Parser[String] = """[iI][nN][tT][eE][gG][eE][rR]""".r
  val LONG: Parser[String] = """[lL][oO][nN][gG]""".r 
  val FLOAT: Parser[String] = """[fF][lL][oO][aA][tT]""".r 
  val DOUBLE: Parser[String] = """[dD][oO][uU][bB][lL][eE]""".r
  val STRING: Parser[String] = """[sS][tT][rR][iI][nN][gG]""".r
  val BINARY: Parser[String] = """[bB][iI][nN][aA][rR][yY]""".r
  val CODEABLE: Parser[String] = """[cC][oO][dD][eE][aA][bB][lL][eE]""".r
  val DECOMPOSED: Parser[String] = """[dD][eE][cC][oO][mM][pP][oO][sS][eE][dD]""".r
  val PRIMARY: Parser[String] = """[pP][rR][iI][mM][aA][rR][yY]""".r
  val FOREIGN: Parser[String] = """[fF][oO][rR][eE][iI][gG][nN]""".r
  val INDEX: Parser[String] = """[iI][nN][eE][xX]""".r
  
  def typeList: Parser[List[ArgotType]] = repsep(argotType, ",") ^^ (List() ++ _) |
      argotType ^^ (id => List[ArgotType](id))

  def argotType: Parser[ArgotType] = {
    TYPE~NAME~opt(key) ^^ {case atype~name~optkey => ArgotTypeType(name, optkey.get)} |
    BOOLEAN~NAME~opt(key) ^^ {case atype~name~optkey => ArgotBoolean(name, optkey.get)} |
    BYTE~NAME~opt(key) ^^ {case atype~name~optkey => ArgotByte(name, optkey.get)} |
    CHAR~NAME~opt(key) ^^ {case atype~name~optkey => ArgotChar(name, optkey.get)} |
    SHORT~NAME~opt(key) ^^ {case atype~name~optkey => ArgotShort(name, optkey.get)} |      
    INTEGER~NAME~opt(key) ^^ {case atype~name~optkey => ArgotInteger(name, optkey.get)} |
    LONG~NAME~opt(key) ^^ {case atype~name~optkey => ArgotLong(name, optkey.get)} |
    FLOAT~NAME~opt(key) ^^ {case atype~name~optkey => ArgotFloat(name, optkey.get)} |      
    DOUBLE~NAME~opt(key) ^^ {case atype~name~optkey => ArgotDouble(name, optkey.get)} |
    STRING~NAME~opt(key) ^^ {case atype~name~optkey => ArgotString(name, optkey.get)} |
    BINARY~NAME~opt(key) ^^ {case atype~name~optkey => ArgotBinary(name,optkey.get)} |
    CODEABLE~NAME~NAME~opt(DECOMPOSED)~opt(key) ^^ {
      case atype~name~name1~decomposed~optkey => CodeableRef(name, name1, decomposed.get, optkey.get)
    } |
    vector |
    map
  }
      
  def key: Parser[Key] = PRIMARY ^^ (x => PrimaryKey()) | FOREIGN ^^ (x => ForeignKey()) | INDEX ^^ (x => IndexKey())
}


/* 
 codeable apple extends fruit  {
   codeable seeds;
   
}

codeable fruit {
  string name,
  integer shelflife,
  vector[integer] shelflives,
  map[key, value] shelflivesToTimeTolive
  
  equals(other) {
    if ( and || !) {
      if (... and/or ...) {
      }
     } else if(...)
 
   }
   
   compare(other) {
       if( and || ! ()) ...
          return -1
       else if..
        return 0
       else..
        return 1
   }
   
   */
trait CodeableObject extends Types with Values with SpecialTypes {
  val EXTENDS: Parser[String] = """[eE][xX][tT][eE][nN][dD][sS]""".r
  val AND: Parser[String] = """[aA][nN][dD]""".r
  val OR: Parser[String] = """[oO][rR]""".r
  val EQUALS: Parser[String] = """[eE][qQ][uU][aA][lL][sS]""".r
  val COMPARE: Parser[String] = """[cC][oO][mM][pP][aA][rR][eE]""".r
  val FOREACH: Parser[String] = """[fF][oO][rR][eE][aA][cC][hH]""".r
  val OTHER: Parser[String] = """([oO][tT][hH][eE][rR])""".r
  val RETURN: Parser[String] = """[rR][eE][tT][uU][rR][nN]""".r
  val TRUE: Parser[String] = """[tT][rR][uU][eE]""".r
  val FALSE: Parser[String] = """[fF][aA][lL][sS][eE]""".r
  val IF: Parser[String] = """[iI][fF]""".r
  val ELSEIF: Parser[String] = """[eE][lL][sS][eE][iI][fF]""".r
  val ELSE: Parser[String] = """[eE][lL][sS][eE]""".r
  
  def codeable: Parser[Codeable] = {
    CODEABLE~NAME~opt(optionalExtends)~"{"~typeList~opt(equals)~opt(compare)~"}" ^^ {
      case codeable~name~supertype~"{"~typelist~optequals~optcompare~"}" => Codeable(name, supertype.get, typelist)
    }
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
      if(optrepmultistmt.get != None) List(stmt) ++ optrepmultistmt.get 
      else List(stmt)
    }
  }
  
  def statement: Parser[Statement] = {
    ifStatement | booleanReturnStatement | ternaryReturnStatement
  }
  
  def ifStatement: Parser[IfThenElseStatement] = 
    singleIf~opt(elseIfStmt)~opt(elseStmt) ^^ {case ifstmt~elseifstmt~elsestmt => {
      if(elseifstmt.get != None && elsestmt.get != None) {
        val clauses = ifstmt :: elseifstmt.get :: elsestmt.get :: List[SubStatement]()
        IfThenElseStatement(clauses)
      } else if(elseifstmt.get != None) {
        val clauses = ifstmt :: elseifstmt.get :: List[SubStatement]()
        IfThenElseStatement(clauses)
      } else if(elsestmt.get != None) {
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
  val OBJECT: Parser[String] = """[oO][bB][jJ][eE][cC][tT]""".r
  def singleton: Parser[SingletonDef] = {
      OBJECT~NAME~"{"~typeList~"}" ^^ {
        case singleton~name~"{"~typelist~"}" => SingletonDef(name, typelist)
      }
  }
}

trait Table extends Types {
  val TABLE: Parser[String] = """[tT][aA][bB][lL][eE]""".r
  def table: Parser[TableDef] = {
    TABLE~NAME~"{"~"}" ^^ {case table~name~"{"~"}" => TableDef(name)}
  }
}

trait DDL extends JavaTokenParsers with Table with SpecialTypes with Object {
}
