package syndeticlogic.argot

import scala.util.parsing.combinator._
import syndeticlogic.argot.ArgotParser;
import syndeticlogic.argot.Insert;
import syndeticlogic.argot.ParseResult;
import syndeticlogic.argot.Tree;

import java.io.Reader;
import java.io.FileReader;
import java.io.StringReader;
import java.lang.RuntimeException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

class ArgotParser extends JavaTokenParsers with Insert {}

object ParseArgot extends ArgotParser {
  val logger: Log = LogFactory.getLog(this.getClass);
  
  def parse(s: String): Tree = {
    val reader = new StringReader(s)
    parse(reader)
  }
  def parse(r: Reader): Tree = {
    val tree: ParseResult[Tree] = parseAll(insertStmt, r)
    logger.debug(tree)
    try { tree.get }
    catch { case e: RuntimeException => {
       logger.error(tree)
       throw e
      }
    }
  }
}