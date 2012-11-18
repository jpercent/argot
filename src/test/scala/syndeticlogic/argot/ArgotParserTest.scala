package syndeticlogic.argot

import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io.File
import java.io.FileInputStream
import java.io.ByteArrayOutputStream
import scala.io.Source._

class ArgotParserTest {
  val codeable0 =  "src/test/resources/test-files/codeable0.argot"
  val codeable0Expected =  "src/test/resources/test-files/codeable0Expected.argot"

  val table0 = "src/test/resources/test-files/table0.argot"
  val table0Expected = "src/test/resources/test-files/table0Expected.argot"
  val table1 = "src/test/resources/test-files/table1.argot"
  val table1Expected = "src/test/resources/test-files/table1Expected.argot"
  
  val insert0 = "src/test/resources/test-files/insert0.argot"
  val insert0Expected ="src/test/resources/test-files/insert0Expected.argot"
  val insert1 ="src/test/resources/test-files/insert1.argot"
  val insert1Expected = "src/test/resources/test-files/insert1Expected.argot"
  val insert2 = "src/test/resources/test-files/insert2.argot"
  val insert2Expected = "src/test/resources/test-files/insert2Expected.argot"
    
  def parseTest(inputQuery: String, queryBuilder: ArgotBuilder): String = {
    var tree = ParseArgot.parse(inputQuery)
    assertTrue(tree != null)
    val file: List[String] = for(stmt <- tree) yield queryBuilder.build(stmt)
    file.foldLeft("")((result, current) => result match {
      case "" => current
      case _ => result +"\n"+current
    })
  }

  def test(input: String, expected: String, builder: ArgotBuilder): Unit = {
    assertEquals(fromFile(expected, "utf-8").getLines.mkString, parseTest(fromFile(input, "utf-8").getLines.mkString, builder))
  }
  
  def testException(input: String, expected: String, builder: ArgotBuilder): Unit = {
    var exception = false
    try {
        assertEquals(fromFile(expected, "utf-8").getLines.mkString, parseTest(fromFile(input, "utf-8").getLines.mkString, builder))
    } catch {
      case e: RuntimeException => exception = true 
    }
    assertTrue(exception)
  }
  
  @Test
  def testCodeable(): Unit = {
   
  }
  
  @Test
  def testTable(): Unit = {
    test(table0, table0Expected, new DDLBuilder)
    test(table1, table1Expected, new DDLBuilder)
  }
  
  @Test
  def testInsert(): Unit = {
    test(insert0, insert0Expected, new DMLBuilder)
    test(insert1, insert1Expected, new DMLBuilder)
    testException(insert2, insert2Expected, new DMLBuilder)
  }
}