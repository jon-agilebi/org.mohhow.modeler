package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object ArchItem extends ArchItem with LongKeyedMetaMapper[ArchItem] {
   override def dbTableName = "ARCH_ITEM"
}

class ArchItem extends LongKeyedMapper[ArchItem] with IdPK {
    def getSingleton = ArchItem
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }
	
 object itemType extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ITEM_TYPE"
 }
 
 object itemName extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ITEM_NAME"
 }
 
 object itemDetail extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ITEM_DETAIL"
 }
 
 object itemDescription extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "ITEM_DESCRIPTION"
 }
 
 object color extends MappedPoliteString(this, 50) {
   override def dbColumnName = "COLOR"
 }
 
 object scheme extends MappedPoliteString(this, 50) {
   override def dbColumnName = "SCHEME"
 }
 
 object counter extends MappedLong(this) {
   override def dbColumnName = "COUNTER"
 }
 
 object roleOf extends MappedLong(this) {
   override def dbColumnName = "ROLE_OF"
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
}