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
 
 val meeting = RelevantMeeting.is
 
 val datePattern = Pattern.compile("\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d");
 val timePattern = Pattern.compile("\\d\\d:\\d\\d")
 
 val meetingCategory = select("Category", nvl(meeting.category), List("Kickoff", "Requirements Gathering", "Discussion on Prototype", "Sprint Review"))
 val topic = field("Topic", nvl(meeting.topic), valMinLen(1, "Topic too short"), valMaxLen(255, "Topic too long"))
 
 val dateField = new Field { 
    type ValueType = String 
    override def name = "Meeting Date" 
    lazy val manifest = buildIt[String]
    override def default = MyUtil.formatDate(meeting.meetingBegin)
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
  } 
 
 val meetingDate = dateField //"Date", "") //, valRegex(datePattern, "Date format must be dd.mm.yyyy"))
 val meetingBegin = field("Begin", nvl(MyUtil.timeInDay(meeting.meetingBegin)), valRegex(timePattern, "Time format must be hh:mm"))
 val meetingEnd = field("End", nvl(MyUtil.timeInDay(meeting.meetingEnd)), valRegex(timePattern, "Time format must be hh:mm"))
 
 def finish() {
	val minutes = RelevantMinutes.is
    meeting.topic(topic).category(meetingCategory).meetingBegin(org.mohhow.bi.util.Utility.asDate(meetingDate, meetingBegin)).meetingEnd(org.mohhow.bi.util.Utility.asDate(meetingDate, meetingEnd)).moderator(User.currentUser).save
    meeting.save
    if(minutes != null) minutes.fkMeeting(meeting).save
    
    val candidates = ScenarioRole.findAll(By(ScenarioRole.fkScenario, SelectedScenario.is.id))
    
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