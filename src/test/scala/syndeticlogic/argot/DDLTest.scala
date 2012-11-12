package syndeticlogic.argot

import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class DDLTest {
  val table0 = "table fact_table{boolean james,type typeValue}"
  val expectedTable0 = "table fact_table { boolean james, type typeValue }"    
  val table1 = "table customer { integer id primary,integer customer_address_id foreign, integer spend, "+
               "codeable order orders decompose }";
  val expectedTable1 = "table customer { integer id primary, integer customer_address_id foreign, integer spend, "+
               "codeable order orders decompose }";
  
  def parseTest(inputQuery: String): String = {
    var tree = ParseArgot.parse(inputQuery)
    assertTrue(tree != null)
    var queryBuilder = new DDLBuilder
    queryBuilder.build(tree)
  }
    
  @Test
  def testCodeable(): Unit = {
  }
  
  @Test
  def testTable(): Unit = {
    assertEquals(expectedTable0, parseTest(table0))
    assertEquals(expectedTable1, parseTest(table1))    
  }
  
}