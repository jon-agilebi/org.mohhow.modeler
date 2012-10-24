package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object ProtocolItem extends ProtocolItem with LongKeyedMetaMapper[ProtocolItem] {
   override def dbTableName = "PROTOCOL_ITEM"
}

class ProtocolItem extends LongKeyedMapper[ProtocolItem] with IdPK {
    def getSingleton = ProtocolItem
	
 object fkMinutes extends MappedLongForeignKey(this, Minutes) {
   override def dbColumnName = "FK_MINUTES"
 }
	
 object itemNumber extends MappedLong(this) {
   override def dbColumnName = "ITEM_NUMBER"
 }

 object classification extends MappedPoliteString(this, 20) {
   override def dbColumnName = "CLASSIFICATION"
 }

 object itemText extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "ITEM_TEXT"
 }
 
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
 
 def findMinutes() = Minutes.findAll(By(Minutes.id, fkMinutes)).apply(0)

 def tr = {
	<tr><td>{fkMinutes}</td><td>{itemNumber}</td><td>{classification}</td><td>{itemText}</td><td>{dateCreated}</td></tr>
 }
     
}