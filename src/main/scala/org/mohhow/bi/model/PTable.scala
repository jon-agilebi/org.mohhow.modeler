package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

import js._
import JsCmds._
import JE.{JsRaw,Str}
import scala.xml._

object PTable extends PTable with LongKeyedMetaMapper[PTable] {
   override def dbTableName = "P_TABLE"
}

class PTable extends LongKeyedMapper[PTable] with IdPK {
    def getSingleton = PTable
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }
	
 object schema extends MappedPoliteString(this, 100) {
   override def dbColumnName = "SCHEMA"
 }
	
 object name extends MappedPoliteString(this, 100) {
   override def dbColumnName = "NAME"
 }
	
 object description extends MappedPoliteString(this, 1024) {
   override def dbColumnName = "DESCRIPTION"
 }
	
 object tablespace extends MappedPoliteString(this, 100) {
   override def dbColumnName = "TABLESPACE"
 }
	
 object tableType extends MappedPoliteString(this, 100) {
   override def dbColumnName = "TABLE_TYPE"
 }
	
 object fkCube extends MappedLong(this) {
   override def dbColumnName = "FK_CUBE"
 }
	
 object fkDimension extends MappedLong(this) {
   override def dbColumnName = "FK_DIMENSION"
 }
	
 object fkLevel extends MappedLong(this) {
   override def dbColumnName = "FK_LEVEL"
 }
	
 object isSource extends MappedLong(this) {
   override def dbColumnName = "IS_SOURCE"
 }
	
 object isTarget extends MappedLong(this) {
   override def dbColumnName = "IS_TARGET"
 }
	
 object isDerivedFromModel extends MappedLong(this) {
   override def dbColumnName = "IS_DERIVED_FROM_MODEL"
 }
	
 object validFrom extends MappedDateTime(this) {
   override def dbColumnName = "VALID_FROM"
 }
	
 object validUntil extends MappedDateTime(this) {
   override def dbColumnName = "VALID_UNTIL"
 }
	
 object isCurrent extends MappedLong(this) {
   override def dbColumnName = "IS_CURRENT"
 }
 
 def add() : JsCmd = {
 	val attr = PAttribute.create
 	attr.name("new_column").fkPTable({id})
 	attr.save
 	val newRow = attr.tr2.toString.trim
 	val command = "$('#physicalRows').append('" + newRow + "');"
  JsRaw(command)
 }
 
 def deleteIt(attributeId : String) : JsCmd = {
    val attribute = PAttribute.findAll(By(PAttribute.id, attributeId.toLong)).apply(0)
    attribute.delete_!
    Noop
  }
 
 def editHeader(): JsCmd = {
  val head = myHeader("edit")
  val command = "$('#physicalTableHead').replaceWith('" + head.toString + "')"
  JsRaw(command) 
 }
 
 def cancelEditHeader(): JsCmd = {
  val head = myHeader("display")
  
  JsRaw("$('#physicalTableHead').replaceWith('" + head.toString + "')") 
 }
 
 def commandLine(mode: String) = {
  if(mode == "display") 
    <span>{ajaxButton("edit header", editHeader _) % new UnprefixedAttribute("class", "standardButton", Null)}</span>
  else
    <span>{ajaxButton("cancel", cancelEditHeader _) % new UnprefixedAttribute("class", "standardButton", Null)}</span>
 }
 
 def myHeaderId(mode: String) = if(mode == "display") "physicalTableHead" else "physicalTableEditHead"
 
 def header = myHeader("display") //JxIfElse(JsRaw("$('#physicalTable[mode='display']).length > 0"), myHeader("display"), myHeader("edit"))
	
 def saveText(tableId: Long, selectionKind: String, text : String) : JsCmd = {
  val table = PTable.findAll(By(PTable.id, tableId)).apply(0)
  
  selectionKind match {
  	case "schema" => table.schema(text)
	case "name" => table.name(text)
	case "description" => table.description(text)
  }
	 
  table.save	  
  if(selectionKind == "name") RedirectTo("/attribute") else Noop
 }
 
 def myHeader(mode: String) = {
 	val addIt = SHtml.ajaxButton("+", add _) % new UnprefixedAttribute("class", "standardButton", Null) 
 	val command = "$('#physicalTable').attr('currentSelection')"
 	//val removeIt = <button onclick={SHtml.ajaxCall(JsRaw(command), deleteIt _)._2}> - </button> % new UnprefixedAttribute("class", "standardButton", Null) 
 	
 	<thead id={myHeaderId(mode)}>
		<tr>
 			<td>Schema</td>
 			<td>{SHtml.ajaxText(schema.toString, text => saveText(id, "schema", text)) % new UnprefixedAttribute("size", "30", Null) % new UnprefixedAttribute("maxlength", "30", Null) }</td>
 			<td></td><td></td><td></td><td></td><td></td><td></td><td>{commandLine(mode)}</td>
 		</tr>
		<tr>
 			<td>Table Name</td><td>{SHtml.ajaxText(name.toString, text => saveText(id, "name", text)) % new UnprefixedAttribute("size", "30", Null) % new UnprefixedAttribute("maxlength", "30", Null) }</td>
 			<td></td><td></td><td></td><td></td><td></td><td></td><td></td>
 		</tr>
		<tr><td>Description</td><td>{SHtml.ajaxText(description.toString, text => saveText(id, "description", text)) % new UnprefixedAttribute("size", "30", Null) % new UnprefixedAttribute("maxlength", "1024", Null) }</td>
			<td></td><td></td><td></td><td></td><td></td><td></td><td>{SHtml.ajaxButton("+", add _) % new UnprefixedAttribute("class", "standardButton", Null)}</td>
		</tr>
		<tr><td>Attributes</td><td>SQL Data Type</td><td>Length</td><td>Precision</td><td>Is Not Null</td><td>Is Primary Key</td><td>References</td><td>Part of Unique Key</td><td>Comment</td></tr>
	</thead>
 }
}