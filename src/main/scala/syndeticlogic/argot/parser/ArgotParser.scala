package syndeticlogic.argot.parser

import scala.util.parsing.combinator._

import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

class ArgotParser extends JavaTokenParsers with DML with DDL {
  def argotStatement: Parser[ArgotParseNode] = {
    insertStmt |
    table |
    singleton |
    codeable
  }
  
  def entryPoint: Parser[List[ArgotParseNode]] = rep(argotStatement) ^^ (List() ++ _)
}

object ParseArgot extends ArgotParser {
  val logger: Log = LogFactory.getLog(this.getClass);
  
  def parse(s: String): List[ArgotParseNode] = {
    val reader = new StringReader(s)
    parse(reader)
  }
  
  def parse(r: Reader): List[ArgotParseNode] = {
    val tree: ParseResult[List[ArgotParseNode]] = parseAll(entryPoint, r)
    logger.debug(tree)
    try { tree.get }
    catch { case e: RuntimeException => {
       logger.error(tree)
       throw e
      }
    }
  }
}