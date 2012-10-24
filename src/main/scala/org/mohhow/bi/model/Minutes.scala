package org.mohhow.model
		
import net.liftweb._
import mapper._
import util._

import http._
import SHtml._
import org.mohhow.bi.util.{Utility => MyUtil}

object Minutes extends Minutes with LongKeyedMetaMapper[Minutes] {
   override def dbTableName = "MINUTES"
}

class Minutes extends LongKeyedMapper[Minutes] with IdPK {
    def getSingleton = Minutes
	
 object fkMeeting extends MappedLongForeignKey(this, Meeting) {
   override def dbColumnName = "FK_MEETING"
 }
	
 object version extends MappedLong(this) {
   override def dbColumnName = "VERSION"
 }

 object status extends MappedPoliteString(this, 50) {
   override def dbColumnName = "STATUS"
 }
 
 object datePublished extends MappedDateTime(this) {
   override def dbColumnName = "DATE_PUBLISHED"
 }

 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }

 def header = { 
  if(status == "published") S.?("version") + " " + version.is.toString + ", " + S.?("publishedOn") + " " + MyUtil.formatDate(datePublished.is)
  else S.?("version") + " " + version.is.toString + ", " + S.?("notYetPublished")
 }
 
 def findMeeting() = Meeting.findAll(By(Meeting.id, fkMeeting)).apply(0)

}