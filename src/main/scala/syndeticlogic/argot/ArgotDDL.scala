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

trait Object extends Types {
  def codeable: Parser[Codeable] = {
    CODEABLE~NAME~"{"~typeList~opt(equals)~opt(compare)~opt(hashcode)~"}" ^^ {
      case codeable~name~"{"~typelist~optequals~optcompare~opthashcode~"}" => Codeable(name, typelist)
    }
  }
  def equals: Parser[Any] = null
  def compare: Parser[Any] = null 
  def hashcode: Parser[Any] = null
}

trait Singleton extends Object {
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
