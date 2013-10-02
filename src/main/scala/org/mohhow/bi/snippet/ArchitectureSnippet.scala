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
import org.mohhow.bi.lib.JsonUtility
import org.mohhow.bi.lib.WikiParser

class ArchitectureSnippet {
	
 def toXml(item: ArchItem) = <item><type>{item.itemType}</type><name>{item.itemName}</name><detail>{item.itemDetail}</detail><description>{item.itemDescription}</description><counter>{item.counter}</counter><scheme>{item.scheme}</scheme><color>{item.color}</color><id>{item.id}</id></item>	
 def sourceSystems(node: Node): List[String] = WikiParser.findTaggedText(MyUtil.getNodeText(node), "source")
 
 def display(): JsCmd = {
  val keywords = Map("tier" -> S.?("tier"), "domain" -> S.?("domain"),    "cancel" -> S.?("cancel"), "save" -> S.?("save"), "title" -> S.?("title"), "description" -> S.?("description"),
		             "scheme" -> S.?("scheme"), "usage" -> S.?("usage"), "color" -> S.?("color"), "deleteIt" -> S.?("delete"))
		             
  val usageTypes = List(S.?("sourceTier"), S.?("presentationTier"), S.?("virtualTier"))
  val systems = (Vision.is  \\ "editText").flatMap(sourceSystems).toList
  val cmd = JsRaw("setKeywords(" + JsonUtility.map2Json(keywords) + ", " + JsonUtility.list2Json(usageTypes) + ", " + JsonUtility.list2Json(systems)+ ");")
			 
  val items = ArchItem.findAll(By(ArchItem.fkScenario, SelectedScenario.is.id), By(ArchItem.isCurrent, 1), OrderBy(ArchItem.counter, Ascending))
  val serializedItems = <items>{items.map(toXml)}</items>
  CmdPair(cmd, JsRaw("createArchitecture('" + serializedItems.toString + "');"))
 }
 
 def processItem(item: Node) = {
	 val itemType = MyUtil.getSeqHeadText(item \ "type")
	 val itemName = MyUtil.getSeqHeadText(item \ "name")
	 val itemDetail = MyUtil.getSeqHeadText(item \ "detail")
	 val itemDescription = MyUtil.getSeqHeadText(item \ "description")
	 val itemCounter = MyUtil.getSeqHeadText(item \ "counter").toLong
	 val itemColor = MyUtil.getSeqHeadText(item \ "color")
	 val itemId = MyUtil.getSeqHeadText(item \ "id").toLong
	 val itemScheme = MyUtil.getSeqHeadText(item \ "scheme")
	 
	 if(itemId <= 0) {
		 val newItem = ArchItem.create
		 newItem.fkScenario(SelectedScenario.is.id).itemType(itemType).itemName(itemName).itemDetail(itemDetail).itemDescription(itemDescription).color(itemColor).counter(itemCounter).scheme(itemScheme).validFrom(new Date).isCurrent(1).save
	 }
	 else {
		 val items = ArchItem.findAll(By(ArchItem.id, itemId))
		 if(!items.isEmpty) items(0).itemType(itemType).itemName(itemName).itemDetail(itemDetail).itemDescription(itemDescription).color(itemColor).counter(itemCounter).scheme(itemScheme).save 
	 }
 }
 
 def saveArchitecture(xml: String): JsCmd = {
  val model = XML.loadString("<model>" + xml + "</model>")
  val items = model \\ "item"
  items.map(processItem)
  
  Noop	 
 }

 def model (xhtml: NodeSeq): NodeSeq = {
  bind("model", xhtml, "display" -> Script(display()),
	   "save"  -> <button class='standardButton'>{S.?("save")}</button> % ("onclick" -> SHtml.ajaxCall(JsRaw("$('#architecture_metadata').html()"), saveArchitecture _)._2)
  )
 }
}