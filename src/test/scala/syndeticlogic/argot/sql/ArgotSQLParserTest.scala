package syndeticlogic.argot.sql

import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
class ArgotSQLParserTest {
  val query0 = "insert into tableName values (\"uno\");"
  val expectedQuery0 = "insert into tableName values (\"uno\")"
  val query1 = "insert into atable (col1,col2, col3) values(1,3,4);"
  val expectedQuery1 = "insert into atable(col1,col2,col3) values (1,3,4)"
  
  def parseTest(inputQuery: String): String = {
    var tree = ParseArgot.parse(inputQuery)
    assertTrue(tree != null)
    var queryBuilder = new QueryBuilder
    queryBuilder.buildQuery(tree)
  }
    
  @Test
  def testIt(): Unit = {
    assertEquals(expectedQuery0, parseTest(query0))
    assertEquals(expectedQuery1, parseTest(query1))
  }
}