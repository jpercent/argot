package syndeticlogic.argot

import scala.util.parsing.combinator._

import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

trait Types extends Commons {
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
      CODEABLE~NAME~NAME~opt(DECOMPOSED)~opt(key) ^^ {case atype~name~name1~decomposed~optkey => CodeableRef(name, name1, decomposed.get, optkey.get)} 
  }  
  
  def key: Parser[Key] = PRIMARY ^^ (x => PrimaryKey()) | FOREIGN ^^ (x => ForeignKey()) | INDEX ^^ (x => IndexKey())
}

/* 
 codeable apple extends fruit  {
   codeable seeds;
   
}

codeable fruit {
  string name,
  integer shelflife
   
  equals {
    if ( and || !) {
      if (... and/or ...) {
      }
     } else if(...)
 
   }
   */
trait CodeableObject extends Types with Values {
  val EXTENDS: Parser[String] = """[eE][xX][tT][eE][nN][dD][sS]""".r
  val AND: Parser[String] = """[aA][nN][dD]""".r
  val OR: Parser[String] = """[oO][rR]""".r
  val EQUALS: Parser[String] = """[eE][qQ][uU][aA][lL][sS]""".r
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
    EQUALS~"{"~>functionStmt<~"}" ^^ (functionstmt => EqualsMethod(functionstmt)) 
  }

  def functionStmt: Parser[List[Statement]] = {
    statement~opt(rep(statement)) ^^ {case stmt~optrepmultistmt => 
      if(optrepmultistmt.get != None) List(stmt) ++ optrepmultistmt.get 
      else List(stmt)
    }
  }
  
  def statement: Parser[Statement] = {
    ifStatement | returnStatement
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
   
  def returnStatement: Parser[ReturnStatement] = {
    RETURN~>TRUE ^^ { _ => ReturnStatement(true) } | 
    RETURN~>FALSE ^^ {_ => ReturnStatement(false) }
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
    arrayReference |
    NAME ^^ {(x => MemberReference(x))}
  }
  
  def objectMemberReference: Parser[QualifiedMemberReference] = {
    NAME~"."~memberReference ^^ {
      case obj~dot~member => QualifiedMemberReference(obj, member)
    }
  }
  
  def arrayReference: Parser[ArrayReference] = {
    NAME~"["~wholeNumber~"]" ^^ {case name~bracket~wholenumber~bracket1 => ArrayReference(name, wholenumber)}
  }
    
  def block: Parser[List[Statement]] = {
    "{"~>repsep(statement, ",")<~"}" ^^ (List() ++ _)
  }
  
  def compare: Parser[Any] = null 
}

trait Singleton extends CodeableObject {
  val SINGLETON: Parser[String] = """[sS][iI][nN][gG][lL][eE][tT][oO][nN]""".r
  def singleton: Parser[SingletonDef] = {
      SINGLETON~NAME~"{"~typeList~"}" ^^ {
        case singleton~name~"{"~typelist~"}" => SingletonDef(name, typelist)
      }
  }
}

trait Vector extends Types {
  val VECTOR: Parser[String] = """[vV][eE][cC][tT][eE][rR]""".r
  def vector: Parser[VectorDef] = {
    VECTOR~NAME~"["~argotType~"]"  ^^ {case vector~name~"["~argottype~"]" => VectorDef(name, argottype)}
  }
}

trait Table extends Types {
  val TABLE: Parser[String] = """[tT][aA][bB][lL][eE]""".r
  def table: Parser[TableDef] = {
    TABLE~NAME~"{"~"}" ^^ {case table~name~"{"~"}" => TableDef(name)}
  }
}
