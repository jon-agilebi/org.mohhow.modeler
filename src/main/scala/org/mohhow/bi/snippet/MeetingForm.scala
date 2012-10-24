package org.mohhow.snippet

import net.liftweb._
import http._
import util.Helpers._
import common._

import org.mohhow.model._
import org.mohhow.bi.util.{Utility => MyUtil}
import java.util.Date
import java.util.regex.Pattern

import scala.xml._
import mapper._

object MeetingForm extends LiftScreen{
	
 def nvl(s: String) = if (s == null) "" else s
 
 val datePattern = Pattern.compile("\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d");
 val timePattern = Pattern.compile("\\d\\d:\\d\\d")
 
 val meetingCategory = select(S.?("category"), nvl(RelevantMeeting.is.category), List(S.?("kickoff"), S.?("requirementsGathering"), S.?("prototypeDiscussion"), S.?("sprintReview")))
 val topic = field(S.?("topic"), nvl(RelevantMeeting.is.topic), valMinLen(1, S.?("textToShort")), valMaxLen(255, S.?("textToLong")))
 
 val dateField = new Field { 
    type ValueType = String 
    override def name = S.?("meetingDate") 
    lazy val manifest = buildIt[String]
    override def default = MyUtil.formatDate(RelevantMeeting.is.meetingBegin)
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
  } 
 
 val meetingDate = dateField
 val meetingBegin = field(S.?("begin"), nvl(MyUtil.timeInDay(RelevantMeeting.is.meetingBegin)), valRegex(timePattern, "Time format must be hh:mm"))
 val meetingEnd = field(S.?("end"), nvl(MyUtil.timeInDay(RelevantMeeting.is.meetingEnd)), valRegex(timePattern, "Time format must be hh:mm"))
 
 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def finish() {
	val meeting = RelevantMeeting.is
	val minutes = RelevantMinutes.is
    meeting.topic(topic).category(meetingCategory).meetingBegin(MyUtil.asDate(meetingDate, meetingBegin)).meetingEnd(MyUtil.asDate(meetingDate, meetingEnd)).moderator(User.currentUser).save
    meeting.save
    if(minutes != null) minutes.fkMeeting(meeting).save
    
    val candidates =  MyUtil.filterScenarioRoles(Nil, ScenarioRole.findAll(By(ScenarioRole.fkScenario, SelectedScenario.is.id)).toList)
    
    for(candidate <- candidates) {
    	val mr = MeetingRecipient.findAll(By(MeetingRecipient.fkUser, candidate.fkUser), By(MeetingRecipient.fkMeeting, meeting.id))
    	
    	if(Participants.is.contains(candidate.fkUser) && (Participants.is(candidate.fkUser)._1 || Participants.is(candidate.fkUser)._2)) {
    		
    		if(mr.isEmpty) {
    			val newRecipient = MeetingRecipient.create
    			newRecipient.fkMeeting(meeting).fkUser(candidate.fkUser).isAttendee(Participants.is(candidate.fkUser)._1).isReviewer(Participants.is(candidate.fkUser)._2).save
    		}
    		else mr.apply(0).fkMeeting(meeting).fkUser(candidate.fkUser).isAttendee(Participants.is(candidate.fkUser)._1).isReviewer(Participants.is(candidate.fkUser)._2).save
    	}
    	else {
    		if(!mr.isEmpty) mr.apply(0).delete_!
    	}
    }
    
    S.redirectTo("/plan")
 }
}