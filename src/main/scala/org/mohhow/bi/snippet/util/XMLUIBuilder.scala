package org.mohhow.bi.util

import scala.xml._
import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.snippet._

import org.mohhow._

import net.liftweb._
import http._
import SHtml._
import S._
import util._
import Helpers._

import js._
import JsCmds._
import JE.{JsRaw,Str}
import org.mohhow.bi.lib.Repository


object XMLUIBuilder {
	
 def createOption(option: Node, groupName: String, elementId: String): NodeSeq = {
  def choose(chosenValue: String): JsCmd = {
   val elmId = "#" + elementId
   val transform = elmId #> <groupSelection id={elementId}>{chosenValue}</groupSelection>
   val newSetup = transform(Setup.is).apply(0)
   println(newSetup)
   Setup(newSetup)
   Repository.write("scenario", SelectedScenario.is.id, "setup", "setup", -1, newSetup)
     
  }
  
   val key = MyUtil.getNodeText((option \\ "key").apply(0))
   val action = SHtml.ajaxCall(JsRaw("$(this).attr('choice')"), choose _)._2
   val radio = <input type="radio" name={groupName} value={key} /> % ("onclick" -> action) % new UnprefixedAttribute("choice", key.toString, Null)
	 
   <tr><td>{radio}</td><td>{MyUtil.getNodeText((option \\ "description").apply(0))}</td></tr>
 }
 
 def createRadioGroup(group: Node): NodeSeq = {

  val groupName =  MyUtil.getNodeText((group \\ "groupKey").apply(0))
  val elementId = ((group \\ "groupSelection").apply(0) \ "@id").text

  def anOption(opt: Node) = createOption(opt, groupName, elementId)
   
  <h4>{MyUtil.getNodeText((group \\ "groupTitle").apply(0))}</h4><br />
  <table>{MyUtil.flattenNodeSeq((group \\ "option").map(anOption).toList)}</table><br />
 }
 
 def createTable(content: Node, metadata: List[(String, Boolean)], withEnumeration: Boolean) = {
	 
 }

}