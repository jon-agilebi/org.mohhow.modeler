package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._
import java.text.SimpleDateFormat
import org.mohhow.bi.util._

object Meeting extends Meeting with LongKeyedMetaMapper[Meeting] {
   override def dbTableName = "MEETING"
}

class Meeting extends LongKeyedMapper[Meeting] with IdPK {
    def getSingleton = Meeting
	
 object fkScenario extends MappedLongForeignKey(this, Scenario) {
   override def dbColumnName = "FK_SCENARIO"
 }

 object meetingBegin extends MappedDateTime(this) {
   override def dbColumnName = "MEETING_BEGIN"
 }
	
 object meetingEnd extends MappedDateTime(this) {
   override def dbColumnName = "MEETING_END"
 }
	
 object topic extends MappedPoliteString(this, 255) {
   override def dbColumnName = "TOPIC"
 }
 
 object category extends MappedPoliteString(this, 255) {
   override def dbColumnName = "CATEGORY"
 }
 
 object moderator extends MappedLongForeignKey(this, User) {
   override def dbColumnName = "MODERATOR"
 }
 
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
 
 def headerAsRow = {
	 
	<tr><td>{category}</td><td>{topic}</td><td>{Utility.formatDate(meetingBegin)}</td><td>{Utility.timeInDay(meetingBegin) + " - " + Utility.timeInDay(meetingEnd)}</td><td>{Utility.formatDate(dateCreated)}</td></tr>
 }

 def header = {
  try {
      val beginDay = Utility.formatDate(meetingBegin)
      val beginTime = Utility.timeInDay(meetingBegin)
      val endTime = Utility.timeInDay(meetingEnd)

      beginDay + ", " + beginTime + " - " + endTime + ", " + topic
		 
  }
  catch{
	  case e: Exception => println(e.toString); "bad header"
  }
 }    
}