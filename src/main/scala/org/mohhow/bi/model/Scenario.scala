package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Scenario extends Scenario with LongKeyedMetaMapper[Scenario] {
   override def dbTableName = "SCENARIO"
}

class Scenario extends LongKeyedMapper[Scenario] with IdPK {
    def getSingleton = Scenario
	
 object name extends MappedPoliteString(this, 100) {
   override def dbColumnName = "NAME"
 }
    
 object prefix extends MappedPoliteString(this, 50) {
   override def dbColumnName = "URL"
 }
    
 object url extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "URL"
 }
    
 object owner extends MappedLongForeignKey(this, User) {
   override def dbColumnName = "OWNER"
 }
	
 object description extends MappedPoliteString(this, 1000) {
   override def dbColumnName = "DESCRIPTION"
 }

 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
}