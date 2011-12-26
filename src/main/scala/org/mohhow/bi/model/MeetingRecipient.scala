package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object MeetingRecipient extends MeetingRecipient with LongKeyedMetaMapper[MeetingRecipient] {
   override def dbTableName = "MEETING_RECIPIENT"
}

class MeetingRecipient extends LongKeyedMapper[MeetingRecipient] with IdPK {
    def getSingleton = MeetingRecipient
	
 object fkMeeting extends MappedLongForeignKey(this, Meeting) {
   override def dbColumnName = "FK_MEETING"
 }

 object fkUser extends MappedLongForeignKey(this, User) {
   override def dbColumnName = "FK_USER"
 }
 
 object isAttendee extends MappedBoolean(this) {
   override def dbColumnName = "IS_ATTENDEE"
 }

 object isReviewer extends MappedBoolean(this) {
   override def dbColumnName = "IS_REVIEWER"
 }

object feedbackStatus extends MappedPoliteString(this, 50) {
   override def dbColumnName = "FEEDBACK_STATUS"
 }
	
  object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
   
}


