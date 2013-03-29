package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import S._
import net.liftweb.common.Full

import mapper._
import util._
import Helpers._

import js._
import JsCmds._
import JE.{JsRaw,Str}
import scala.xml._
import org.mohhow.bi.util.{Utility => MyUtil}

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
 	SetHtml("physicalRows", List.flatten(PAttribute.findAll(By(PAttribute.fkPTable, id), OrderBy(PAttribute.id, Ascending)).map(_.tr2.toList)).toSeq)
 	
 }
 
 def deleteIt(attributeId: String): JsCmd = {
  println("attributeId" + attributeId) 
  try {
	  PAttribute.findAll(By(PAttribute.id, attributeId.toLong)).apply(0).delete_!
  }
  catch {
	 case e: Exception => println(e.toString)
  }
   
  SetHtml("physicalRows", List.flatten(PAttribute.findAll(By(PAttribute.fkPTable, id), OrderBy(PAttribute.id, Ascending)).map(_.tr2.toList)).toSeq)
 }
 
 def saveText(tableId: Long, selectionKind: String, text : String) : JsCmd = {
  val table = PTable.findAll(By(PTable.id, tableId)).apply(0)
  
  selectionKind match {
  	case "schema" => table.schema(text)
	case "name" => table.name(text)
	case "description" => table.description(text)
	case "tableType" => table.tableType(text)
  }
	 
  table.save	  
  if(selectionKind == "name") RedirectTo("/attribute") else Noop
 }
 
 def header = {
 	val addIt = SHtml.ajaxButton("+", add _) % new UnprefixedAttribute("class", "standardButton", Null) 
 	val removeAction =  SHtml.ajaxCall(JsRaw("$('.physicalEditRow.zebraHover').attr('rowID')"), deleteIt _)._2
 	val removeIt = <button>-</button>  % ("onclick" -> removeAction) % new UnprefixedAttribute("class", "standardButton", Null) 
 	val chooseTableType = SHtml.ajaxSelect(("", "") :: MyUtil.tableTypes.map(t => (t, S.?(t))), Full(tableType) , tt => saveText(id, "tableType", tt))
 	
 	<thead>
		<tr>
 			<td>{S.?("schema")}</td>
 			<td colspan="3">{SHtml.ajaxText(schema.toString, text => saveText(id, "schema", text)) % new UnprefixedAttribute("size", "30", Null) % new UnprefixedAttribute("maxlength", "30", Null) }</td>
 			<td></td><td></td><td colspan="3">{chooseTableType}</td>
 		</tr>
		<tr>
 			<td>{S.?("tableName")}</td><td colspan="3">{SHtml.ajaxText(name.toString, text => saveText(id, "name", text)) % new UnprefixedAttribute("size", "30", Null) % new UnprefixedAttribute("maxlength", "30", Null) }</td>
 			<td></td><td></td><td></td><td></td><td></td>
 		</tr>
		<tr><td>{S.?("description")}</td><td colspan="3">{SHtml.ajaxText(description.toString, text => saveText(id, "description", text)) % new UnprefixedAttribute("size", "30", Null) % new UnprefixedAttribute("maxlength", "1024", Null) }</td>
			<td></td><td></td><td></td><td></td><td>{addIt}{removeIt}</td>
		</tr>
		<tr><td>{S.?("attributes")}</td><td>{S.?("sqlDataType")}</td><td>{S.?("length")}</td><td>{S.?("precision")}</td><td>{S.?("isNotNull")}</td><td>{S.?("isPrimaryKey")}</td><td>{S.?("references")}</td><td>{S.?("partOfUniqueKey")}</td><td>{S.?("comment")}</td></tr>
	</thead>
 }
}