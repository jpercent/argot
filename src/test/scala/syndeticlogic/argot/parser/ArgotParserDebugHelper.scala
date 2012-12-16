package syndeticlogic.argot.parser

import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io.File
import java.io.FileInputStream
import java.io.ByteArrayOutputStream
import scala.io.Source._

class ArgotParserDebugHelper {
  val argotParser = new ArgotParserTest()
  
  def show(input: String, builder: ArgotBuilder): Unit = {
    System.out.println(argotParser.parseTest(fromFile(input, "utf-8").getLines.mkString, builder))
  }
  
  def showException(input: String, builder: ArgotBuilder): Unit = {
    var exception = false
    try {
      argotParser.parseTest(fromFile(input, "utf-8").getLines.mkString, builder)
    } catch {
      case e: RuntimeException => e.printStackTrace()
      return
    }
    System.out.println("no exception ")
  }
  
  @Test
  def showCodeable3(): Unit = {
    show(argotParser.codeable7, new DDLBuilder)
//    showException(argotParser.insert2, new DMLBuilder)
  }
}