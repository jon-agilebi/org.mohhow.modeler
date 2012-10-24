package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Feature extends Feature with LongKeyedMetaMapper[Feature] {
   override def dbTableName = "FEATURE"
}

class Feature extends LongKeyedMapper[Feature] with IdPK {
    def getSingleton = Feature
	
 object fkPb extends MappedLongForeignKey(this, ProductBacklog) {
   override def dbColumnName = "FK_PB"
 }
    
 object fkSprint extends MappedLongForeignKey(this, Sprint) {
   override def dbColumnName = "FK_SPRINT"
 }
 
 object featureNumber extends MappedLong(this) {
   override def dbColumnName = "FEATURE_NUMBER"
 }

 object name extends MappedPoliteString(this, 100) {
   override def dbColumnName = "NAME"
 }
 
 object featureType extends MappedPoliteString(this, 100) {
   override def dbColumnName = "FEATURE_TYPE"
 }
	
 object parentFeature extends MappedLong(this) {
   override def dbColumnName = "PARENT_FEATURE"
 }
 
 object storyPoints extends MappedPoliteString(this, 10) {
   override def dbColumnName = "STORY_POINTS"
 }
 
 object priority extends MappedLong(this) {
   override def dbColumnName = "PRIORITY"
 }
	
 object description extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "DESCRIPTION"
 }
 
 object completionDate extends MappedDateTime(this) {
   override def dbColumnName = "COMPLETION_DATE"
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