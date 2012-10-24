package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Measure extends Measure with LongKeyedMetaMapper[Measure] {
   override def dbTableName = "MEASURE"
}

class Measure extends LongKeyedMapper[Measure] with IdPK {
    def getSingleton = Measure
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }
    
 object fkFeature extends MappedLongForeignKey(this, Feature) {
   override def dbColumnName = "FK_FEATURE"
 }
 
 object fkSynonym extends MappedLongForeignKey(this, Measure) {
   override def dbColumnName = "FK_SYNONYN"
 }
 
 object fkCube extends MappedLongForeignKey(this, ModelVertex) {
   override def dbColumnName = "FK_CUBE"
 }
	
 object shortName extends MappedPoliteString(this, 50) {
   override def dbColumnName = "SHORT_NAME"
 }
	
 object longName extends MappedPoliteString(this, 200) {
   override def dbColumnName = "LONG_NAME"
 }
	
 object subject extends MappedPoliteString(this, 50) {
   override def dbColumnName = "MEASURE_SUBJECT"
 }
 
 object status extends MappedPoliteString(this, 50) {
   override def dbColumnName = "STATUS"
 }

 object definition extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "DEFINITION"
 }
	
 object formula extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "FORMULA"
 }
 
 object mockup extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "MOCKUP"
 }
	
 object unit extends MappedPoliteString(this, 50) {
   override def dbColumnName = "UNIT"
 }
 
 object reason extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "REASON"
 }
 
 object requiredActualityUnit extends MappedPoliteString(this, 50) {
   override def dbColumnName = "REQUIRED_ACTUALITY_UNIT"
 }
 
 object requiredActualityValue extends MappedLong(this) {
   override def dbColumnName = "REQUIRED_ACTUALITY_VALUE"
 }
 
 object timespanOfInterestUnit extends MappedPoliteString(this, 50) {
   override def dbColumnName = "TIMESPAN_OF_INTEREST_UNIT"
 }
 
 object timespanOfInterestValue extends MappedLong(this) {
   override def dbColumnName = "TIMESPAN_OF_INTEREST_VALUE"
 }
 
 object requiredStorageUnit extends MappedPoliteString(this, 50) {
   override def dbColumnName = "REQUIRED_STORAGE_UNIT"
 }
 
 object requiredStorageValue extends MappedLong(this) {
   override def dbColumnName = "REQUIRED_STORAGE_VALUE"
 }
 
 object minimalValue extends MappedDouble(this) {
   override def dbColumnName = "MINIMAL_VALUE"
 }
 
 object maximalValue extends MappedDouble(this) {
   override def dbColumnName = "MAXIMAL_VALUE"
 }
 
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
 
 object dateDiscarded extends MappedDateTime(this) {
   override def dbColumnName = "DATE_DISCARDED"
 } 
}