package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object ModelVertex extends ModelVertex with LongKeyedMetaMapper[ModelVertex] {
   override def dbTableName = "MODEL_VERTEX"
}

class ModelVertex extends LongKeyedMapper[ModelVertex] with IdPK {
    def getSingleton = ModelVertex
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }
    
 object referenceId extends MappedLong(this) {
   override def dbColumnName = "REFERENCE_ID"
 }
	
 object elementType extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ELEMENT_TYPE"
 }
 
 object elementName extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ELEMENT_NAME"
 }
 
 object elementDetail extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ELEMENT_DETAIL"
 }
 
 object elementDescription extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "ELEMENT_DESCRIPTION"
 }
 
 object elementKind extends MappedPoliteString(this, 50) {
   override def dbColumnName = "ELEMENT_KIND"
 }
 
 object x extends MappedLong(this) {
   override def dbColumnName = "X"
 }
 
 object y extends MappedLong(this) {
   override def dbColumnName = "Y"
 }
 
 object scale extends MappedLong(this) {
   override def dbColumnName = "SCALE"
 }
 
 object roleOf extends MappedLong(this) {
   override def dbColumnName = "ROLE_OF"
 }
 
 object validFrom extends MappedDateTime(this) {
   override def dbColumnName = "VALID_FROM"
 }
 
 object isDegenerated extends MappedLong(this) {
   override def dbColumnName = "IS_DEGENERATED"
 }
	
 object validUntil extends MappedDateTime(this) {
   override def dbColumnName = "VALID_UNTIL"
 }
	
 object isCurrent extends MappedLong(this) {
   override def dbColumnName = "IS_CURRENT"
 }
}