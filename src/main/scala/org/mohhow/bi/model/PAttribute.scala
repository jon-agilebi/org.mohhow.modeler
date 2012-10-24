package org.mohhow.model
		
import net.liftweb._
import net.liftweb.common._
import mapper._
import http._
import SHtml._
import util._

import js._
import JsCmds._
import JE.{JsRaw,Str}
import scala.xml._

object PAttribute extends PAttribute with LongKeyedMetaMapper[PAttribute] {
   override def dbTableName = "P_ATTRIBUTE"
}

class PAttribute extends LongKeyedMapper[PAttribute] with IdPK {
    def getSingleton = PAttribute
	
 object fkPTable extends MappedLongForeignKey(this, PTable) {
   override def dbColumnName = "FK_P_TABLE"
 }

 object name extends MappedPoliteString(this, 100) {
   override def dbColumnName = "NAME"
 }
	
 object dataType extends MappedPoliteString(this, 100) {
   override def dbColumnName = "DATA_TYPE"
 }
	
 object length extends MappedLong(this) {
   override def dbColumnName = "LENGTH"
 }
	
 object scale extends MappedLong(this) {
   override def dbColumnName = "SCALE"
 }
	
 object isNotNullable extends MappedLong(this) {
   override def dbColumnName = "IS_NOT_NULLABLE"
 }
	
 object isPrimaryKey extends MappedLong(this) {
   override def dbColumnName = "IS_PRIMARY_KEY"
 }
	
 object reference extends MappedLong(this) {
   override def dbColumnName = "REFERENCE"
 }
	
 object isPartOfUniqueKey extends MappedLong(this) {
   override def dbColumnName = "IS_PART_OF_UNIQUE_KEY"
 }
	
 object comment extends MappedPoliteString(this, 1024) {
   override def dbColumnName = "COMMENT"
 }
	
 object isDerivedFromModel extends MappedLong(this) {
   override def dbColumnName = "IS_DERIVED_FROM_MODEL"
 }
 
 object fkModelAttribute extends MappedLong(this) {
   override def dbColumnName = "FK_MODEL_ATTRIBUTE"
 }
 
 object fkMeasure extends MappedLong(this) {
   override def dbColumnName = "FK_MEASURE"
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
 
 def chooseTableAttribute(attributeId: Long, selectionKind: String, selected: Boolean): JsCmd = {
  val attr = PAttribute.findAll(By(PAttribute.id, attributeId)).apply(0)
  
  if(selected){
	  
	  selectionKind match {
	 	  case "isNotNullable" => attr.isNotNullable(1)
	 	  case "isPrimaryKey" => attr.isPrimaryKey(1)
	 	  case "partOfUniqueKey" => attr.isPartOfUniqueKey(1)
	  }
  }
  else {
	  
	  selectionKind match {
	 	  case "isNotNullable" => attr.isNotNullable(0)
	 	  case "isPrimaryKey" => attr.isPrimaryKey(0)
	 	  case "partOfUniqueKey" => attr.isPartOfUniqueKey(0)
	  }
  }
	   
  attr.save	   
  Noop
 }

 def saveText(attributeId: Long, selectionKind: String, text : String) : JsCmd = {
  val attr = PAttribute.findAll(By(PAttribute.id, attributeId)).apply(0)
  
  selectionKind match {
	 	  case "name" => attr.name(text)
	 	  case "length" => attr.length(text.toLong)
	 	  case "scale" => attr.scale(text.toLong)
	 	  case "reference" => attr.reference(0)
	 	  case "comment" => attr.comment(text)
	  }
	 
	  attr.save	   
  Noop
 }
 
 val allDataTypes = List(("VARCHAR2", "VARCHAR2"), ("NUMBER", "NUMBER"), ("DATE", "DATE"), ("TIMESTAMP", "TIMESTAMP"))
 
 def saveSelection(attributeId: Long, selection: String) : JsCmd = {
  val attr = PAttribute.findAll(By(PAttribute.id, attributeId)).apply(0)
  attr.dataType(selection).save
  Noop
 }
 
 def getRef(id: Long) = {
  val matches = PAttribute.findAll(By(PAttribute.id, id))
  if(matches.isEmpty) "" else PTable.findAll(By(PTable.id, matches(0).fkPTable)).apply(0).name + "." +  matches.apply(0).name
 }
	  
 def tr2() : NodeSeq = {
	 val notNullable = (isNotNullable > 0)
	 val primaryKey = (isPrimaryKey > 0)
	 val partOfUniqueKey = (isPartOfUniqueKey > 0)
	 
        <tr class="physicalEditRow" rowID={id.toString}>
			<td>
	 			{SHtml.ajaxText(name.toString, text => saveText(id, "name", text)) % new UnprefixedAttribute("size", "30", Null) % new UnprefixedAttribute("maxlength", "30", Null) }  
			</td>
			<td>
				{SHtml.ajaxSelect(allDataTypes, Full(dataType.toString), v => saveSelection (id, v))}
            </td>
			<td>
				{SHtml.ajaxText(length.toString, text => saveText(id, "length", text)) % new UnprefixedAttribute("size", "8", Null) % new UnprefixedAttribute("maxlength", "8", Null) }
			</td>
			<td>
				{SHtml.ajaxText(scale.toString, text => saveText(id, "scale", text)) % new UnprefixedAttribute("size", "8", Null) % new UnprefixedAttribute("maxlength", "8", Null) }
			</td>
            <td>
				{SHtml.ajaxCheckbox(notNullable, selected => chooseTableAttribute (id, "isNotNullable", selected))}
			</td>
			<td>
				{SHtml.ajaxCheckbox(primaryKey, selected => chooseTableAttribute (id, "isPrimaryKey", selected))}
			</td>
			<td>
				{SHtml.ajaxText(getRef(reference).toString, text => saveText(id, "reference", text)) % new UnprefixedAttribute("size", "20", Null) % new UnprefixedAttribute("maxlength", "100", Null) }
			</td>
			<td>
				{SHtml.ajaxCheckbox(partOfUniqueKey, selected => chooseTableAttribute (id, "isPartOfUniqueKey", selected))}
			</td>
			<td>
				{SHtml.ajaxText(comment.toString, text => saveText(id, "comment", text)) % new UnprefixedAttribute("comment", "20", Null) % new UnprefixedAttribute("maxlength", "1024", Null) }
			</td>
		</tr>
 }     
}