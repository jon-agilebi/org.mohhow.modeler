package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._
import java.text.SimpleDateFormat
import org.mohhow.bi.util._

object Sprint extends Sprint with LongKeyedMetaMapper[Sprint] {
   override def dbTableName = "SPRINT"
}

class Sprint extends LongKeyedMapper[Sprint] with IdPK {
    def getSingleton = Sprint
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }
 
 object fkRelease extends MappedLongForeignKey(this, Release) {
   override def dbColumnName = "FK_RELEASE"
 }
 
 object isReleaseSprint extends MappedLong(this) {
   override def dbColumnName = "IS_RELEASE_SPRINT"
 }

 object sprintBegin extends MappedDateTime(this) {
   override def dbColumnName = "SPRINT_BEGIN"
 }
	
 object sprintEnd extends MappedDateTime(this) {
   override def dbColumnName = "SPRINT_END"
 }
 
 object numberOfWorkingDays extends MappedLong(this) {
   override def dbColumnName = "NUMBER_WORKING_DAYS"
 }
 
 object sprintNumber extends MappedLong(this) {
   override def dbColumnName = "SPRINT_NUMBER"
 }
 
 object purpose extends MappedPoliteString(this, 255) {
   override def dbColumnName = "PURPOSE"
 }
 
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
 
 def prettyName = "Sprint " + sprintNumber.toString
 
}