package org.mohhow.snippet

import org.mohhow._
import model._

import net.liftweb._
import http._
import SHtml._
import S._

import js._
import JsCmds._
import JE.{JsRaw,Str}

import mapper._
import util._
import Helpers._

import scala.xml._
import org.mohhow.bi.lib.Repository
import net.liftweb.common.Empty
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}

object SelectedSprint extends SessionVar[Sprint](null)
object Candidates extends SessionVar[Set[Long]](Set())
object EmptySprint extends SessionVar[Set[Long]](Set())
object EarliestBegin extends RequestVar[Long](19000101)

class SprintSnippet {
	
 def addSprint(): JsCmd = {
  val newSprint = Sprint.create
  newSprint.fkScenario(SelectedScenario.is.id).dateCreated(new Date).sprintNumber(getNextSprintNumber())
  SelectedSprint(newSprint)
  RedirectTo("/sprintEdit")
 }
 
 def selectSprint(id : Long) = {
  val s = Sprint.findAll(By(Sprint.id, id)).apply(0)
  SelectedSprint(s)
 }  

 def createSprintListItem(s : Sprint) = {
  val item = link("/sprint", () => selectSprint(s.id.is), <span>{s.prettyName}</span>)
  <li class='listItem'>{item}</li>
 }
 
 def getNextSprintNumber(): Long = {
  val allSprints = Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), OrderBy(Sprint.sprintNumber, Descending))
  if(allSprints.isEmpty) 1 else {
	 val maxSprint = allSprints.apply(0)
	 val nextNumber = 1 + maxSprint.sprintNumber
	 nextNumber
  }
 }
 
 def removeSprint(): JsCmd = {
  val allSprints = Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), OrderBy(Sprint.sprintNumber, Descending))
  if(!allSprints.isEmpty) {
	  val latestSprint = allSprints.apply(0);
	  if (SelectedSprint.is != null && latestSprint.id == SelectedSprint.is.id) {
		  if(allSprints.size > 1) SelectedSprint(allSprints.apply(1))
		  else SelectedSprint(null)
	  }
		 
	  latestSprint.delete_!
	  RedirectTo("/plan")
  }
  else Noop
 }
 
 def toggleCompletion(isCompleted: Boolean): JsCmd = {
  for(featureId <- EmptySprint.is) {
	val f = Feature.findAll(By(Feature.id, featureId)).apply(0)
	if(isCompleted) f.completionDate(new Date).save
	if(!isCompleted) f.completionDate(null).save
  }
  RedirectTo("/sprint")
 }
 
 def editSprint (xhtml: NodeSeq): NodeSeq = {
  def complete = toggleCompletion(true)
  def unComplete = toggleCompletion(false)
  def edit(): JsCmd = RedirectTo("sprintEdit")
  bind("sprint", xhtml, "edit" -> ajaxButton(S.?("editSprint"), edit _) % ("class" -> "standardButton"),
		  				"complete" -> ajaxButton(S.?("markAsCompleted"), complete _) % ("class" -> "standardButton"),
		              	"unComplete" -> ajaxButton(S.?("unmarkCompletion"), unComplete _) % ("class" -> "standardButton"))
 }
 
 def serializeBurndown(sp: Sprint): Node = {
  
  def drtn(begin: Date, end: Date) = MyUtil.duration(MyUtil.dateAsNumber(begin), MyUtil.dateAsNumber(end))
  
  def goDown(m: Int, l:List[Int]): List[Int] = l match {
   case Nil => Nil
   case n :: ns => (m - n) :: goDown(m - n, ns) 
  }
  
  val ftrs = Feature.findAll(By(Feature.fkSprint, sp.id))
  val sumPoints = (0 /: ftrs.map(_.storyPoints.toString.toInt)) (_ + _)
  val completionDays = ftrs.map(_.completionDate).sort(MyUtil.dateAsNumber(_) < MyUtil.dateAsNumber(_)).filter(MyUtil.dateAsNumber(_)  > 19000101).distinct
  println(completionDays.toString)
  val points = completionDays.map(d =>  (0 /: ftrs.filter(_.completionDate == d).map(_.storyPoints.toString.toInt)) (_ + _))
  println(points.toString)
  val daysAndPoints = completionDays zip goDown(sumPoints, points)
  
  val burndownList = daysAndPoints.map(d => <item><date>{drtn(sp.sprintBegin, d._1).toString}</date><remainder>{d._2.toString}</remainder></item>)
  
  <burndown>
  	<points>{sumPoints.toString}</points>
  	<begin>{MyUtil.formatDate(sp.sprintBegin)}</begin>
  	<end>{MyUtil.formatDate(sp.sprintEnd)}</end>
    <duration>{drtn(sp.sprintBegin, sp.sprintEnd)}</duration>
    {burndownList}
  </burndown>
  
 } 
 
 def burnItDown():NodeSeq = {
  if(SelectedSprint.is != null) {
	  val burndownXml = serializeBurndown(SelectedSprint.is)
	  val cmd1 = SetHtml("burndownData", burndownXml)
	  val cmd2 = JsRaw("showBurndown();")
	  Script(CmdPair(cmd1, cmd2))
  }
  else NodeSeq.Empty
 }
 
 def serializePlan(): Node = {
  def serializeRow(row: (Long, String, String, String, String, String, Long, Long)) = {
	  <row>
	  	<kind>{row._2}</kind>
	  	<name>{row._3}</name>
	  	<begin>{row._4}</begin>
	  	<end>{row._5}</end>
	  	<duration>{row._6}</duration>
        <absoluteDuration>{row._8}</absoluteDuration>
	  </row>
  }
  
  def serializeSprint(sp: Sprint) = {
   val beginAsNumber = MyUtil.dateAsNumber(sp.sprintBegin)
   val endAsNumber = MyUtil.dateAsNumber(sp.sprintEnd)
   val spBegin = MyUtil.formatDate(sp.sprintBegin, S.?("dateFormat"))
   val spEnd = MyUtil.formatDate(sp.sprintEnd, S.?("dateFormat"))
   val duration = MyUtil.duration(MyUtil.dateAsNumber(sp.sprintBegin), MyUtil.dateAsNumber(sp.sprintEnd))
   val durationFromStart = MyUtil.duration(EarliestBegin.is, MyUtil.dateAsNumber(sp.sprintEnd))
	  
   (beginAsNumber, "sprint", "Sprint" + sp.sprintNumber.toString, spBegin, spEnd, duration.toString, endAsNumber, durationFromStart)
  }
  
  def serializeRelease(r: Release) = {
   val endAsNumber = MyUtil.dateAsNumber(r.scheduledEnd)
   val rBegin = MyUtil.formatDate(r.begin, S.?("dateFormat"))
   val rEnd = MyUtil.formatDate(r.scheduledEnd, S.?("dateFormat"))
   val duration = MyUtil.duration(MyUtil.dateAsNumber(r.begin), MyUtil.dateAsNumber(r.scheduledEnd))
   val durationFromStart = MyUtil.duration(EarliestBegin.is, MyUtil.dateAsNumber(r.scheduledEnd))
	  
   (endAsNumber, "release", "Release " + r.prettyNumber, rBegin, rEnd, duration.toString, endAsNumber, durationFromStart)
  }
		  								
   
  val sprintDates = Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id)).map(sp => (MyUtil.dateAsNumber(sp.sprintBegin), MyUtil.dateAsNumber(sp.sprintEnd))).toList
  val releaseDates = Release.findAll(By(Release.fkScenario, SelectedScenario.is.id)).map(r => (MyUtil.dateAsNumber(r.begin), MyUtil.dateAsNumber(r.end))).toList
  
  if(!sprintDates.isEmpty || !releaseDates.isEmpty) {
	  val earliestBegin = (sprintDates ::: releaseDates).map(_._1).min
	  EarliestBegin(earliestBegin)
	  val latestEnd = (sprintDates ::: releaseDates).map(_._2).max
	  
	  val sprints = Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), OrderBy(Sprint.sprintBegin, Ascending)).map(serializeSprint).toList
	  val releases = Release.findAll(By(Release.fkScenario, SelectedScenario.is.id), OrderBy(Release.begin, Ascending)).map(serializeRelease).toList
	 
	  val allRows = ((sprints ++ releases) sort(_._1 < _._1)).map(serializeRow).toSeq
	  
	  <plan>
  		<observationLine>{MyUtil.formatDate(new Date, S.?("dateFormat"))}</observationLine>
	  	<earliestBegin>{MyUtil.formatDate(MyUtil.dateFromNumber(earliestBegin),S.?("dateFormat"))}</earliestBegin>
	  	<latestEnd>{MyUtil.formatDate(MyUtil.dateFromNumber(latestEnd),S.?("dateFormat"))}</latestEnd>
	  	<completeDuration>{MyUtil.duration(earliestBegin, latestEnd).toString}</completeDuration>
	  	<observationDuration>{MyUtil.duration(earliestBegin, MyUtil.dateAsNumber(new Date)).toString}</observationDuration>
	  	{allRows}
	  </plan>
  }
  else <plan />
 }
 
 def sprintInPlan (xhtml: NodeSeq): NodeSeq = {
	 
  def empty(): JsCmd = Noop	 
	 
  val addButton = if(MyUtil.isAnalyst()) ajaxButton(S.?("addSprint"), addSprint _) % ("class" -> "standardButton") else ajaxButton(S.?("addSprint"), empty _) % ("class" -> "standardButton") % ("disabled" -> "")
  val removeButton = if(MyUtil.isAnalyst()) ajaxButton(S.?("removeLatestSprint"), removeSprint _) % ("class" -> "standardButton") else ajaxButton(S.?("removeLatestSprint"), empty _) % ("class" -> "standardButton") % ("disabled" -> "")
	 
  bind("sprint", xhtml, "addSprint" -> addButton,
		              	"removeLatestSprint" -> removeButton,
		              	"sprints" -> Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), OrderBy(Sprint.sprintNumber, Ascending)).map(createSprintListItem),
		              	"burndown" -> burnItDown(),
		              	"releasePlan" -> serializePlan())
 }

 def toggleFeature(featureId: Long, mode: String, selection: Boolean) = {
  if(mode == "backlog" & selection) Candidates(Candidates.is + featureId)
  else if (mode == "backlog") Candidates(Candidates.is - featureId)
  else if (mode == "sprint" & selection) EmptySprint(EmptySprint.is + featureId)
  else EmptySprint(EmptySprint.is - featureId)
  Noop
 }
 
 def sprintDisplay(mode: String, f: Feature) = {
  def completion() =  if(mode == "sprint") <td>{MyUtil.formatDate(f.completionDate)}</td> else NodeSeq.Empty
  <tr>
	 <td>{SHtml.ajaxCheckbox(false, selected => toggleFeature(f.id, mode, selected))}</td>
     <td>{f.name}</td>
     <td>{f.featureType}</td>
     <td>{f.storyPoints}</td>
     <td>{f.priority}</td>
     {completion()}
  </tr>
 }
 
 def backlogStories(): NodeSeq = {
  def hasNoChildren(f: Feature) = Feature.findAll(By(Feature.parentFeature,f.id)).isEmpty	 
	 
  if(ChosenBacklog.is != null) {
	Feature.findAll(By(Feature.fkPb, ChosenBacklog.is.id), NullRef(Feature.fkSprint)).filter(feature => hasNoChildren(feature)).map( f => sprintDisplay("backlog", f))
  }
  else <empty />
 }
 
 def sprintStories(): NodeSeq = {
  if(ChosenBacklog.is != null) {
	 Feature.findAll(By(Feature.fkPb, ChosenBacklog.is.id), By(Feature.fkSprint, SelectedSprint.is.id)).map(f => sprintDisplay("sprint", f))
  }
  else <empty />
 }
 
 def toProduct(): JsCmd = {
  for(featureId <- EmptySprint.is) {
	val f = Feature.findAll(By(Feature.id, featureId)).apply(0)
	val nullSprint: Sprint = null
	f.fkSprint(nullSprint).save
	EmptySprint(EmptySprint.is - featureId)
  }
	
  RedirectTo("/sprint")
 }
 
 def toSprint(): JsCmd = {
  for(featureId <- Candidates.is) {
   val f = Feature.findAll(By(Feature.id, featureId)).apply(0)
   f.fkSprint(SelectedSprint.is).save
   Candidates(Candidates.is - featureId)
  }
	
  RedirectTo("/sprint")
 }
 
 def addPoints(p1: String, p2: String) = if(p1 == "Gogol" || p2 == "Gogol") "Gogol" else {
  val sum = p1.toLong + p2.toLong
  sum.toString
 }
 
 def sumStories(sprint: Sprint): String = { 
  val points = Feature.findAll(By(Feature.fkSprint, sprint.id)).map(f => f.storyPoints.toString)
  ("0" /: points) (addPoints)
 }
 
 def scope(sprint: Sprint): Node = {
  <div>
	<label><b>{S.?("begin")}: </b></label>{MyUtil.formatDate(sprint.sprintBegin)} <br />
	<label><b>{S.?("end")}: </b></label>{MyUtil.formatDate(sprint.sprintEnd)} <br />
	<label><b>{S.?("numberWorkingDays")}: </b></label>{sprint.numberOfWorkingDays} <br /><br />
	<label><b>{S.?("sprintObjective")}</b></label><br />
		 {sprint.purpose}
  </div>	 
 }
 
 def computeVelocity(sprint: Sprint) = {
  if(sprint.numberOfWorkingDays > 0) {
	  if(sumStories(sprint) == "Gogol") "extremely fast" 
	  else {
	 	  val velocity = sumStories(sprint).toFloat / sprint.numberOfWorkingDays.toFloat
	 	  velocity.toString
	  }
  }
  else "--"
 }
 
 def findLatestVelocity(sprint: Sprint) = {
  if(sprint.sprintNumber > 1) {
	  val predecessor = sprint.sprintNumber - 1
	  val sprints = Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), By(Sprint.sprintNumber, predecessor))
	  <span><b>{S.?("latestVelocity")}: </b>{computeVelocity(sprints(0))}</span>
  }
  else <span><b>{S.?("latestVelocity")}: </b>--</span>
 }
 
 def findMedianVelocity() = {
	val sprints = Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), NotNullRef(Sprint.sprintEnd)).map(computeVelocity)
	
	if(sprints.size > 0) {
		val median = sprints.size/2
		<span><b>{S.?("medianVelocity")}: </b>{sprints(median)}</span>
	}
	else <span><b>{S.?("medianVelocity")}: </b>--</span>
 }
 
 def sprint (xhtml: NodeSeq): NodeSeq = {
  
  bind("sprint", xhtml, "toProduct"  -> ajaxButton("<<", toProduct _) % ("class" -> "standardButton"),
		              	"toSprint" -> ajaxButton(">>", toSprint _) % ("class" -> "standardButton"),
		              	"backlogStories" -> backlogStories(),
		              	"sprintStories" -> sprintStories(),
		              	"sumStories" -> <span><b>{S.?("sumStories")}: </b>{sumStories(SelectedSprint.is)}</span>,
		              	"latestVelocity" -> findLatestVelocity(SelectedSprint.is),
		              	"medianVelocity" -> findMedianVelocity(),
		              	"scope" -> scope(SelectedSprint.is),
		              	"scopeEditor" -> <div class="mohhowFormLight"><div class="lift:SprintForm"></div></div>)
 }
}