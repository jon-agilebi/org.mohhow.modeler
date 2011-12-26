package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object MeasureRange extends MeasureRange with LongKeyedMetaMapper[MeasureRange] {
   override def dbTableName = "MEASURE_RANGE"
}

class MeasureRange extends LongKeyedMapper[MeasureRange] with IdPK {
    def getSingleton = MeasureRange
	
 object fkMeasure extends MappedLongForeignKey(this, Measure) {
   override def dbColumnName = "FK_MEASURE"
 }
    
 object lowerBound extends MappedDouble(this) {
   override def dbColumnName = "LOWER_BOUND"
 }
 
 object upperBound extends MappedDouble(this) {
   override def dbColumnName = "UPPER_BOUND"
 }
 
 object rangeValue extends MappedDouble(this) {
   override def dbColumnName = "RANGE_VALUE"
 }
 
 object meaning extends MappedPoliteString(this, 100) {
   override def dbColumnName = "MEANING"
 }
 
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
}