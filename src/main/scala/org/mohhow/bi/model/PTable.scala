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
	
 object name extends MappedPoliteString(this, 100) {
   override def dbColumnName = "NAME"
 }
	
 object description extends MappedPoliteString(this, 1024) {
   override def dbColumnName = "DESCRIPTION"
 }
 
 object tier extends MappedLong(this) {
   override def dbColumnName = "TIER"
 }
	
 object domain extends MappedLong(this) {
   override def dbColumnName = "DOMAIN"
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
	
 object dependsOn extends MappedLong(this) {
   override def dbColumnName = "DEPENDS_ON"
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
  def createAttr(tableId: Long, refId: Long): Long = {
	  val attr = PAttribute.create
	  attr.name("new_column").fkPTable(tableId)
	  attr.dependsOn(refId).save
	  attr.id
  }
  
  val refId = createAttr(id, 0)
  PTable.findAll(By(PTable.dependsOn, id)).map(tbl => createAttr(tbl.id, refId))
  SetHtml("physicalRows", List.flatten(PAttribute.findAll(By(PAttribute.fkPTable, id), OrderBy(PAttribute.id, Ascending)).map(_.tr2.toList)).toSeq)
 }
 
 def deleteIt(attributeId: String): JsCmd = {
  println("attributeId" + attributeId) 
  try {
	  PAttribute.findAll(By(PAttribute.dependsOn, attributeId.toLong)).map(attr => attr.delete_!)
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
	case "name" => table.name(text)
	case "description" => table.description(text)
	case "tableType" => table.tableType(text)
	case "tier" => table.tier(text.toLong)
	case "domain" => table.domain(text.toLong)
  }
	 
  table.save	  
  if(selectionKind == "name") RedirectTo("/attribute") else Noop
 }
 
 def header = {
 	val addIt = SHtml.ajaxButton("+", add _) % new UnprefixedAttribute("class", "standardButton", Null) 
 	val removeAction =  SHtml.ajaxCall(JsRaw("$('.physicalEditRow.zebraHover').attr('rowID')"), deleteIt _)._2
 	val removeIt = <button>-</button>  % ("onclick" -> removeAction) % new UnprefixedAttribute("class", "standardButton", Null) 
 	val chooseTableType = SHtml.ajaxSelect(("", "") :: MyUtil.tableTypes.map(t => (t, S.?(t))), Full(tableType) , tt => saveText(id, "tableType", tt))
 	
 	val tiers = ArchItem.findAll(By(ArchItem.fkScenario, fkScenario.toLong), By(ArchItem.itemType, "tier"))	
 	val chooseTier = SHtml.ajaxSelect(("-1", S.?("noMapping")) :: tiers.map(t => (t.id.toString, t.itemName.toString)), Full(tier.toString) , txt => saveText(id, "tier", txt))
 	
 	val domains = ArchItem.findAll(By(ArchItem.fkScenario, fkScenario.toLong), By(ArchItem.itemType, "domain"))	
 	val chooseDomain = SHtml.ajaxSelect(("-1", S.?("noMapping")) :: domains.map(t => (t.id.toString, t.itemName.toString)), Full(domain.toString) , txt => saveText(id, "domain", txt))
 	
 	<thead>
		<tr>
 			<td>{S.?("tier")}</td><td colspan="3">{chooseTier}</td>
 			<td></td><td></td><td colspan="3">{chooseTableType}</td>
 		</tr>
 		<tr>
 			<td>{S.?("domain")}</td><td colspan="3">{chooseDomain}</td>
 			<td></td><td></td><td></td><td></td><td></td>
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