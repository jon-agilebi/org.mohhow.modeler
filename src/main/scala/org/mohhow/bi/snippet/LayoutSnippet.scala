package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import net.liftweb.common.Full
import net.liftweb.common.Empty
import net.liftweb.common.Box

import http._
import SHtml._
import S._

import mapper._
import util._
import Helpers._

import js._
import JsCmds._
import JE.{JsRaw,Str,Call}

import scala.xml._
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib.Repository

object Layout extends SessionVar[Node](null)

class LayoutSnippet {

 def setValue(tagName: String, text: String, id: String): JsCmd = {
  val tag = XML.loadString("<" + tagName + " id='" + id + "'>" + text + "</" + tagName + ">") \\ tagName
  val idSymbol = "#" + id
  val transform = idSymbol #> tag
  val transformedSetup = transform(Setup.is).apply(0)
  Repository.write("scenario", SelectedScenario.is.id, "setup","setup", -1, transformedSetup)
  Setup(transformedSetup)
  JsRaw("updateScorecard('" + id + "', '" + text + "')")
 }
 
 def selectPart(id: String, startIndex: Int, stopIndex:Int, value: String, path:List[String]): JsCmd = {
  val valueSoFar = getValue(path)
  val parts = valueSoFar.split(";").toList
  
  if(startIndex > 0 && stopIndex < parts.length - 1) setValue(path.last, MyUtil.makeSemicolonList(parts take startIndex) + ";" + value + ";" + MyUtil.makeSemicolonList(parts drop (stopIndex + 1)), id)
  else if(startIndex > 0) setValue(path.last, MyUtil.makeSemicolonList(parts take startIndex) + ";" + value, id)
  else if(stopIndex < parts.length - 1) setValue(path.last, value + ";" + MyUtil.makeSemicolonList(parts drop (stopIndex + 1)), id)
  else setValue(path.last, value, id)
 }
 
 def getValue(path: List[String]) = {
  def getSubnode(n:Node, fragment: String):Node = if(n != null && fragment != null && fragment.length > 0 && !(n \ fragment).isEmpty) (n \ fragment).apply(0) else <empty />
  def getNode(n:Node, path: List[String]):Node = path match {
	  case Nil => n
	  case fragment :: tail => getNode(getSubnode(n, fragment), tail)
  }
  
  MyUtil.getSeqHeadText(getNode(Setup.is , path))
 }
 
 def createInputRow(n: Node): NodeSeq = {
  val kind = (n \ "@kind").text
  val id = (n \ "@id").text
  val path = (n \ "@path").text.split(";").toList
  
  val points = List.range(8,64).map(n => (n.toString, n.toString))
  val value = getValue(path)
  
  if(kind == "header") {
		 val h = <tr><td><b>{S.?((n \ "@name").text)}</b></td><td></td></tr>
         val h2 = <tr><td></td><td></td></tr>
	     val b = MyUtil.flattenNodeSeq(n.child.map(createInputRow).toList)
	     val f = <tr><td></td><td></td></tr>
	     MyUtil.flattenNodeSeq(List(h, h2, b, f))	 
  }
  else if(kind == "text") {
	  val parts = value.split(";").toList
	  val colorPart = parts(2) + ";" + parts(3) + ";" + parts(4)
	  val fonts = (Layout.is \\ "font").map(font => (MyUtil.getSeqHeadText(font), MyUtil.getSeqHeadText(font))).toList
	  val fontChoice = SHtml.ajaxSelect(fonts, Box(parts(0)), v => selectPart(id, 0, 0, v, path))
	  val pointChoice = SHtml.ajaxSelect(points, Box(parts(1)), v => selectPart(id, 1, 1, v, path))
	  val colorText = SHtml.ajaxText(colorPart, v => selectPart(id, 2, 4, v, path)) % ("class" -> "colorChoice") 
	  val alphaText = SHtml.ajaxText(parts(5), v => selectPart(id, 5, 5, v, path)) 
	  <tr>
	   	<td><label>{S.?((n \ "@name").text)}</label></td>
	   	<td></td>
	  </tr>
	  <tr>
	   	<td><label>{S.?("font")}</label></td>
	   	<td>{fontChoice}</td>
	  </tr>
	  <tr>
	   	<td><label>{S.?("points")}</label></td>
	   	<td>{pointChoice}</td>
	  </tr>
	  <tr>
	   	<td><label>{S.?("color")}</label></td><td>{colorText}</td>   			
	  </tr>
	  <tr>
	   	<td><label>{S.?("alpha")}</label></td><td>{alphaText}</td>   			
	  </tr>
  }
  else if(kind == "color") {
	  val parts = value.split(";").toList
	  val colorPart = parts(0) + ";" + parts(1) + ";" + parts(2)
	  val colorText = SHtml.ajaxText(colorPart, v => selectPart(id, 0, 2, v, path)) % ("class" -> "colorChoice") 
	  val alphaText = SHtml.ajaxText(parts(3), v => selectPart(id, 3, 3, v, path)) 
	  <tr>
	   	<td><label>{S.?((n \ "@name").text)}</label></td>
	   	<td></td>
	  </tr>
	  <tr>
	   	<td><label>{S.?("color")}</label></td><td>{colorText}</td>   			
	  </tr>
	  <tr>
	   	<td><label>{S.?("alpha")}</label></td><td>{alphaText}</td>   			
	  </tr>
  }
  else if(kind == "selectRange") {
	  val min = (n \ "@min").text
	  val max = (n \ "@max").text
	  val l = List.range(min.toInt, max.toInt).map(n => (n.toString, n.toString))
	  val select = SHtml.ajaxSelect(l, Box(value), v => setValue(path.last, v, id))
	  
	  <tr>
	   	<td><label>{S.?((n \ "@name").text)}</label></td>
	   	<td>{select}</td>
	  </tr>
  }
  else if(kind == "selectPattern") {
	  val pattern = MyUtil.getSeqHeadText(n)
	  val l =  pattern.split(";").map(n => (n.toString, S.?(n.toString)))
	  val select = SHtml.ajaxSelect(l, Box(value), v => setValue(path.last, v, id))
	  
	  <tr>
	   	<td><label>{S.?((n \ "@name").text)}</label></td>
	   	<td>{select}</td>
	  </tr>
  }
  else NodeSeq.Empty
  
 }
	
 def layoutBlock(bl: Node) = {
  <div>
	 <li class='listItem editHeader'>{S.?((bl \ "@name").text)}</li>
	 <li class='listItem editBlock mohhowFormLight'>
  		<table>
	 		{if((bl \ "@kind").text == "header") bl.child.map(createInputRow) else createInputRow(bl)}
        </table>
  	</li>
  </div>
 }
 
 def prepareLayout() {
  if(Setup.is == null) Setup((Repository.read("scenario", SelectedScenario.is.id, "setup","setup", -1) \\ "setup").map(Utility.trim).apply(0))
  if(Layout.is == null) Layout((Repository.read("configuration", -1, "scorecard_layout","scorecard_layout", -1) \\ "layout").apply(0))
 }
	
 def layout():NodeSeq = {	
  prepareLayout()
  Layout.is.child.filter(n => (n \ "@isGroup") != null && (n \ "@isGroup").text == "y").map(layoutBlock)
 }
 
 def display(): NodeSeq = {
  def updateCommand(n: Node): JsCmd = JsRaw("updateScorecard('" + (n \ "@id").text + "', '" + getValue((n \ "@path").text.split(";").toList) + "');")
  def glueCommands(l: JsCmd, r: JsCmd): JsCmd = CmdPair(l, r)
  def initialCmd(): JsCmd = JsRaw("showScorecard();")
  
  prepareLayout()
  val updateTemplate = Layout.is.child.filter(n => (n \ "@isGroup") == null || (n \ "@isGroup").text != "y")
  val cmds = updateTemplate.map(updateCommand)
  val bigCommand = (initialCmd() /: cmds) (glueCommands(_,_))
  Script(bigCommand)
 }
 
 def design (xhtml: NodeSeq): NodeSeq = {
  bind("scorecard", xhtml, "data" -> layout(),
		                   "display" -> display())
 }
}