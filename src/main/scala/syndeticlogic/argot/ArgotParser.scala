package syndeticlogic.argot

import scala.util.parsing.combinator._

import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

class ArgotParser extends JavaTokenParsers with DML with DDL {
  def argotStatement: Parser[ArgotParseTree] = {
    insertStmt |
    table |
    singleton |
    codeable
  }
  
  def entryPoint: Parser[List[ArgotParseTree]] = rep(argotStatement) ^^ (List() ++ _)
}

object ParseArgot extends ArgotParser {
  val logger: Log = LogFactory.getLog(this.getClass);
  
  def parse(s: String): List[ArgotParseTree] = {
    val reader = new StringReader(s)
    parse(reader)
  }
  
  def parse(r: Reader): List[ArgotParseTree] = {
    val tree: ParseResult[List[ArgotParseTree]] = parseAll(entryPoint, r)
    logger.debug(tree)
    try { tree.get }
    catch { case e: RuntimeException => {
       logger.error(tree)
       throw e
      }
    }
  }
}