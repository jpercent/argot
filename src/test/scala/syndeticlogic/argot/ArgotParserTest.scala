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
  val codeable1 =  "src/test/resources/test-files/codeable0.argot"
  val codeable1Expected =  "src/test/resources/test-files/codeable0Expected.argot"

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

  def test(expected: String, input: String, builder: ArgotBuilder): Unit = {
    assertEquals(fromFile(expected, "utf-8").getLines.mkString, parseTest(fromFile(input, "utf-8").getLines.mkString, builder))
  }
  
  def testException(expected: String, input: String, builder: ArgotBuilder): Unit = {
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
    test(codeable0Expected, codeable0, new DDLBuilder)
    test(codeable1Expected, codeable1, new DDLBuilder)
  }
  
  @Test
  def testTable(): Unit = {
    test(table0Expected, table0, new DDLBuilder)
    test(table1Expected, table1, new DDLBuilder)
  }
  
  @Test
  def testInsert(): Unit = {
    test(insert0Expected, insert0, new DMLBuilder)
    test(insert1Expected, insert1, new DMLBuilder)
    testException(insert2Expected, insert2, new DMLBuilder)
  }
}