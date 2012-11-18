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

  @Test
  def testCodeable(): Unit = {
  }
  
  @Test
  def testTable(): Unit = {
    assertEquals(fromFile(table0Expected, "utf-8").getLines.mkString, parseTest(fromFile(table0, "utf-8").getLines.mkString, new DDLBuilder))
    assertEquals(fromFile(table1Expected, "utf-8").getLines.mkString, parseTest(fromFile(table1, "utf-8").getLines.mkString, new DDLBuilder))
  }

  @Test
  def testInsert(): Unit = {
    assertEquals(fromFile(insert0Expected, "utf-8").getLines.mkString, parseTest(fromFile(insert0, "utf-8").getLines.mkString, new DMLBuilder))
    assertEquals(fromFile(insert1Expected, "utf-8").getLines.mkString, parseTest(fromFile(insert1, "utf-8").getLines.mkString, new DMLBuilder))
    
    var exception = false
    try {
        assertEquals(fromFile(insert2Expected, "utf-8").getLines.mkString, parseTest(fromFile(insert2, "utf-8").getLines.mkString, new DMLBuilder))
    } catch {
      case e: RuntimeException => exception = true 
    }
    assertTrue(exception)
  }
}