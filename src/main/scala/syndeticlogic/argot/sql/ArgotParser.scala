package syndeticlogic.argot.sql

import scala.util.parsing.combinator._
import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;

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

class ArgotParser extends JavaTokenParsers {
  
  val INSERT: Parser[String] = """[iI][nN][sS][eE][rR][tT]""".r
  val INTO: Parser[String] = """[iI][nN][tT][oO]""".r
  val VALUES: Parser[String] = """[vV][aA][lL][uU][eE][sS]""".r
  val HIGH: Parser[String] = """[hH][iI][gG][hH][pP][rR][iI][oO][rR][iI][tT][yY]""".r
  val LOW: Parser[String] = """[lL][oO][wW][pP][rR][iI][oO][rR][iI][tT][yY]""".r
  val DELAY: Parser[String] = """[dD][eE][lL][aA][yY]""".r
  val NONE: Parser[String] = """[nN][oO][nN][eE]""".r
  
  val NAME: Parser[String] = ident
  val columnName: Parser[ColumnName] = ident ^^ (id => ColumnName(id))
  val tableName: Parser[TableName] = ident ^^ (id => TableName(id))

  def columns(o: Option[ArgotParser.this.~[ArgotParser.this.~[String,List[syndeticlogic.argot.sql.ColumnName]],String]]): List[ColumnName] = {
    o.get._1._2
  }
  
  def insertStmt: Parser[InsertStmt] = 
    INSERT~INTO~tableName~opt(insertOption)~opt("("~columnList~")")~VALUES~"("~valueList~");" ^^ {
          case insert~into~tname~insertops~clist~values~"("~vlist~");" if insertops == scala.None && clist == scala.None =>
            InsertStmt(tname, None(), ColumnList(List()), ValueList(vlist))
          case insert~into~tname~insertops~clist~values~"("~vlist~");" if insertops == scala.None =>
            InsertStmt(tname, None(), ColumnList(columns(clist)), ValueList(vlist))
          case insert~into~tname~insertops~clist~values~"("~vlist~");" if clist == scala.None =>
            InsertStmt(tname, None(), ColumnList(List()), ValueList(vlist))
          case insert~into~tname~insertops~clist~values~"("~vlist~");" =>
            InsertStmt(tname, insertops.get, ColumnList(columns(clist)), ValueList(vlist))
      }
  
  def insertOption: Parser[InsertOption] = 
    HIGH ^^ (x => HighPriority()) | 
    LOW ^^ (x => HighPriority()) | 
    DELAY ^^ (x => Delayed()) |
    NONE ^^ (x => None())
  
  def columnList: Parser[List[ColumnName]] = repsep(columnName, ",") ^^ (List() ++ _) |
      columnName ^^ (id => List[ColumnName](id)) 
  
  def valueList: Parser[List[Value]] = repsep(value, ",") ^^ (List() ++ _) |
      value ^^ (id => List[Value](id))
      
  def objectValue: Parser[Map[String, Value]] = 
    "{"~> repsep(objectMember, ",") <~"}" ^^ (Map() ++ _)

  def arrayValue: Parser[List[Value]] = "["~> repsep(value, ",") <~"]"
  
  def objectMember: Parser[(String,Value)] = stringLiteral~":"~value ^^ 
      {case name~":"~value => (name, value)}

  //objectValue ^^ (x => ArgotObject(x)) |
          //arrayValue ^^ (x => ArgotArray(x)) | 
  def value: Parser[Value] = 
          stringLiteral ^^ (literal => StringLiteral(literal)) |
          wholeNumber ^^ (x => IntegralNumber(x.toLong)) | 
          floatingPointNumber ^^ (x => RealNumber(x.toDouble)) | 
          "null" ^^ (x => NullValue()) |
          "true" ^^ (x => ArgotBoolean(true)) |
          "false" ^^ (x => ArgotBoolean(false)) 
}

object ParseArgot extends ArgotParser {
  def parse(s: String): Tree = {
    val reader = new StringReader(s)
    parse(reader)
  }
  def parse(r: Reader): Tree = {
    val tree: ParseResult[Tree] = parseAll(insertStmt, r)
    println(tree)
    try { 
      tree.get
    } catch {
      case e: RuntimeException => null
    }
  }
}
/* 
stmt:  insert_stmt { emit("STMT"); } 
   ;


insert_stmt: INSERT insert_opts opt_into NAME
     opt_col_names
     VALUES insert_vals_list
     opt_ondupupdate { emit("INSERTVALS %d %d %s", $2, $7, $4.strval); free($4.strval) }
   ;

opt_ondupupdate: // nil
   | ONDUPLICATE KEY UPDATE insert_asgn_list { emit("DUPUPDATE %d", $4); }
   ;

insert_opts: // nil
      { $$ = 0; }
   | insert_opts LOW_PRIORITY { $$ = $1 | 01 ; }
   | insert_opts DELAYED { $$ = $1 | 02 ; }
   | insert_opts HIGH_PRIORITY { $$ = $1 | 04 ; }
   | insert_opts IGNORE { $$ = $1 | 010 ; }
   ;

opt_into: INTO | // nil
   ;

opt_col_names: // nil
   | '(' column_list ')' { emit("INSERTCOLS %d", $2); }
   ;

insert_vals_list: '(' insert_vals ')' { emit("VALUES %d", $2); $$ = 1; }
   | insert_vals_list ',' '(' insert_vals ')' { emit("VALUES %d", $4); $$ = $1 + 1; }

insert_vals:
     expr { $$ = 1; }
   | DEFAULT { emit("DEFAULT"); $$ = 1; }
   | insert_vals ',' expr { $$ = $1 + 1; }
   | insert_vals ',' DEFAULT { emit("DEFAULT"); $$ = $1 + 1; }
   ;

insert_stmt: INSERT insert_opts opt_into NAME
    SET insert_asgn_list
    opt_ondupupdate
     { emit("INSERTASGN %d %d %s", $2, $6, $4.strval); free($4.strval) }
   ;

insert_stmt: INSERT insert_opts opt_into NAME opt_col_names
    select_stmt
    opt_ondupupdate { emit("INSERTSELECT %d %s", $2, $4.strval); free($4.strval); }
  ;

insert_asgn_list:
     NAME COMPARISON expr
       { if ($2 != 4) { lyyerror(@2,"bad insert assignment to %s", $1); YYERROR; }
       emit("ASSIGN %s", $1.strval); free($1.strval); $$ = 1; }
   | NAME COMPARISON DEFAULT
       { if ($2 != 4) { lyyerror(@2,"bad insert assignment to %s", $1.strval); YYERROR; }
                 emit("DEFAULT"); emit("ASSIGN %s", $1.strval); free($1.strval); $$ = 1; }
   | insert_asgn_list ',' NAME COMPARISON expr
       { if ($4 != 4) { lyyerror(@4,"bad insert assignment to %s", $1); YYERROR; }
                 emit("ASSIGN %s", $3.strval); free($3.strval); $$ = $1 + 1; }
   | insert_asgn_list ',' NAME COMPARISON DEFAULT
       { if ($4 != 4) { lyyerror(@4,"bad insert assignment to %s", $1); YYERROR; }
                 emit("DEFAULT"); emit("ASSIGN %s", $3.strval); free($3.strval); $$ = $1 + 1; }
   ;

  */