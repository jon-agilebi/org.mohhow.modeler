package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object ModelEdge extends ModelEdge with LongKeyedMetaMapper[ModelEdge] {
   override def dbTableName = "MODEL_EDGE"
}

class ModelEdge extends LongKeyedMapper[ModelEdge] with IdPK {
    def getSingleton = ModelEdge
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }
  
 object referenceId extends MappedLong(this) {
   override def dbColumnName = "REFERENCE_ID"
 }
    
 object head extends MappedLongForeignKey(this, ModelVertex) {
   override def dbColumnName = "FK_HEAD"
 }
 
 object tail extends MappedLongForeignKey(this, ModelVertex) {
   override def dbColumnName = "FK_TAIL"
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