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
         if () {
         }
     } else if(
   
   equals <- "{"~functionStmts~"}"
   funtionStmts <- functionStmt~opt(rep(functionStmt))
   functionStmt <- returnStmt | ifStmt
   returnStmt <- RETURN BOOLEAN~";"
   ifStmt <- IF~conditionalStmt~blockStmt~opt(rep(elseIfStmt))~opt(else)
   blockStmt <- "{"~opt(block)~"}"
   block <- rep(ifStmt) | returnStmt
   elseIfStmt <- ELSEIF~conditionalStmt~blockStmt
   elseStmt <- blockStmt
   conditionalStmt <-  "("~condition~")"
   condition <- reference~comparator~reference~opt(rep(connector~condition)) 
   reference <- memberReference | memberAccessReference | constant
   constant <- Number | stringLiteral
   memberReference <- opt("!")~NAME 
   memberAccessReference <- opt("!")~NAME~"."~opt("!")~NAME
   comparator <- "==" | "!=" | "<" | ">" | "<=" | ">="
   connector <- "and" | "or" 
 }
  }
 */

trait CodeableObject extends Types {
  val EXTENDS: Parser[String] = """[eE][xX][tT][eE][nN][dD][sS]""".r
  val AND: Parser[String] = """[aA][nN][dD]""".r
  val OR: Parser[String] = """[oO][rR]""".r
  val EQUALS: Parser[String] = """[eE][qQ][uU][aA][lL][sS]""".r
  val RETURN: Parser[String] = """[rR][eE][tT][uU][rR][nN]""".r
  val TRUE: Parser[String] = """[tT][rR][uU][eE]""".r
  val FALSE: Parser[String] = """[fF][aA][lL][sS][eE]""".r
  val IF: Parser[String] = """[iI][fF]""".r
  
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
  
  def ifStatement: Parser[IfStatement] = { IF~"("~>conditionalStmt~")"~"{"~blockStmt<~"}" ^^ 
    {case conditional~")"~"{"~block => IfStatement(conditional, block)}
  }
  
  def returnStatement: Parser[ReturnStatement] = {
    RETURN~>TRUE ^^ { _ => ReturnStatement(true) } | 
    RETURN~>FALSE ^^ {_ => ReturnStatement(false) }
  }
  def conditionalStmt: Parser[Condition] = null
  def blockStmt: Parser[Block] = null
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
