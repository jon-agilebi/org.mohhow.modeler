package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Release extends Release with LongKeyedMetaMapper[Release] {
   override def dbTableName = "RELEASE"
}

class Release extends LongKeyedMapper[Release] with IdPK {
    def getSingleton = Release
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }

 object majorRelease extends MappedLong(this) {
   override def dbColumnName = "MAJOR_RELEASE"
 }
 
 object minorRelease extends MappedLong(this) {
   override def dbColumnName = "MINOR_RELEASE"
 }
 
 object patch extends MappedLong(this) {
   override def dbColumnName = "PATCH"
 }

 object kind extends MappedPoliteString(this, 50) {
   override def dbColumnName = "KIND"
 }
 
 object begin extends MappedDateTime(this) {
   override def dbColumnName = "RELEASE_BEGIN"
 }
 
 object end extends MappedDateTime(this) {
   override def dbColumnName = "RELEASE_END"
 }
 
 object scheduledEnd extends MappedDateTime(this) {
   override def dbColumnName = "SCHEDULED_END"
 }
 
 object announcementDate extends MappedDateTime(this) {
   override def dbColumnName = "ANNOUNCEMENT_DATE"
 }
 
 object freezeDate extends MappedDateTime(this) {
   override def dbColumnName = "FREEZE_DATE"
 }
 
 object status extends MappedPoliteString(this, 50) {
   override def dbColumnName = "STATUS"
 }
 
 object creationDate extends MappedDateTime(this) {
   override def dbColumnName = "CREATION_DATE"
 }
 
 def prettyNumber = if(patch == null) majorRelease.toString + "." + minorRelease.toString
 				    else majorRelease.toString + "." + minorRelease.toString + "." + patch.toString 
} 