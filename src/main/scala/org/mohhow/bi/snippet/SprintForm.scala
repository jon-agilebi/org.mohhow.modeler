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

object SprintForm extends LiftScreen{
	
 def nvl(s: String) = if (s == null) "" else s
 val twoDigitsPattern = Pattern.compile("\\d{1,2}+")	
 
 val beginField = new Field { 
    type ValueType = String 
    override def name = "Sprint Begin" 
    lazy val manifest = buildIt[String]
    override def default = MyUtil.formatDate(SelectedSprint.is.sprintBegin)
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
  } 
 
 val endField = new Field { 
    type ValueType = String 
    override def name = "Sprint End" 
    lazy val manifest = buildIt[String]
    override def default = MyUtil.formatDate(SelectedSprint.is.sprintEnd)
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
  } 
 
 val sprintBegin = beginField
 val sprintEnd = endField
 
 val sprintWorkingDays = field("Number of Working Days", nvl(SelectedSprint.is.numberOfWorkingDays.toString), valRegex(twoDigitsPattern, "Priority must be a number less or equal to 99"))
 val purpose = textarea("Sprint Objective", nvl(SelectedSprint.is.purpose), valMaxLen(255, "Description too long"))
 
 def finish() {
	val sprint = SelectedSprint.is
	sprint.sprintBegin(MyUtil.asDate(sprintBegin)).sprintEnd(MyUtil.asDate(sprintEnd)).numberOfWorkingDays(sprintWorkingDays.toString.toLong).purpose(purpose).save
 }
}