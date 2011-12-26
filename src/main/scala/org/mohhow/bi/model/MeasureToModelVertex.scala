package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object MeasureToModelVertex extends MeasureToModelVertex with LongKeyedMetaMapper[MeasureToModelVertex] {
   override def dbTableName = "MEASURE_TO_MODEL_VERTEX"
}

class MeasureToModelVertex extends LongKeyedMapper[MeasureToModelVertex] with IdPK {
    def getSingleton = MeasureToModelVertex
	
 object fkMeasure extends MappedLongForeignKey(this, Measure) {
   override def dbColumnName = "FK_MEASURE"
 }
 
 object fkLevel extends MappedLongForeignKey(this, ModelVertex) {
   override def dbColumnName = "FK_LEVEL"
 }
    
 object aggregation extends MappedPoliteString(this, 50) {
   override def dbColumnName = "AGGREGATION"
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