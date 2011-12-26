package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import http._
import SHtml._
import S._

import js._
import JsCmds._
import JE.{JsRaw,Str}

import mapper._
import util._
import Helpers._

import scala.xml._

import java.util.Date

object SelectedTable extends SessionVar[PTable](null)

class PhysicalModelSnippet {

 def selectTable(id : Long) = {
  val pt = PTable.findAll(By(PTable.id, id)).apply(0)
  SelectedTable(pt)
 }  

 def createListItem(pt : PTable) = {
  val name = pt.name.toString
  val myId = pt.id.is
  val item = link("/attribute", () => selectTable(myId), <span>{name}</span>)
  <li class='listItem'>{item}</li>
 }

 def addTable() : JsCmd = {
  val newTable = PTable.create
  newTable.name("<table name>").fkScenario(SelectedScenario.is).validFrom(new Date).isCurrent(1).isDerivedFromModel(1)
  newTable.save
  JsRaw("$('#physical_table_overview').append('" + createListItem(newTable) + "')")
 }
  
 def work (xhtml: NodeSeq): NodeSeq = {
  def showHeader() = {
	  if(SelectedTable.is != null){
			 PTable.findAll(By(PTable.id, SelectedTable.is.id.is)).map(_.header).apply(0)
	  }
	  else{
	 	  val tables = PTable.findAll(By(PTable.fkScenario, SelectedScenario.is))
	 	  if(!tables.isEmpty) tables.apply(0).header else <span>No physical table</span>
	  }
  }
  
  def showRows() = {
	  if(SelectedTable.is != null){
		  List.flatten(PAttribute.findAll(By(PAttribute.fkPTable, SelectedTable.is.id.is)).map(_.tr2.toList)).toSeq
	  }
	  else{
	 	  val tables = PTable.findAll(By(PTable.fkScenario, SelectedScenario.is))
	 	  if(!tables.isEmpty) {
	 	 	  List.flatten(PAttribute.findAll(By(PAttribute.fkPTable, tables.apply(0).id.is)).map(_.tr2.toList)).toSeq
	 	  }
	 	  else <span/>
	  }
  }
		
  bind("physical", xhtml, "add" -> ajaxButton("add table", addTable _) % ("class" -> "standardButton"),
		                  "header" -> showHeader(),
		                  "overview" -> PTable.findAll().map(createListItem).toSeq,
		                  "rows" -> showRows())
  }
}