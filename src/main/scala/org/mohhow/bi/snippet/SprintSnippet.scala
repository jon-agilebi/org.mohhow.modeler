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
  //EmptySprint
  RedirectTo("/sprint")
 }
 
 def editSprint (xhtml: NodeSeq): NodeSeq = {
  def complete = toggleCompletion(true)
  def unComplete = toggleCompletion(false)
  def edit(): JsCmd = RedirectTo("sprintEdit")
  bind("sprint", xhtml, "edit" -> ajaxButton("Edit Sprint", edit _) % ("class" -> "standardButton"),
		  				"complete" -> ajaxButton("Mark Feature as Completed", complete _) % ("class" -> "standardButton"),
		              	"unComplete" -> ajaxButton("Unmark Completion", unComplete _) % ("class" -> "standardButton"))
 }
 
 def sprintInPlan (xhtml: NodeSeq): NodeSeq = {
  bind("sprint", xhtml, "addSprint" -> ajaxButton("Add Sprint", addSprint _) % ("class" -> "standardButton"),
		              	"removeLatestSprint" -> ajaxButton("Remove Latest Sprint", removeSprint _) % ("class" -> "standardButton"),
		              	"sprints" -> Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id)).map(createSprintListItem))
 }

 def toggleFeature(featureId: Long, mode: String, selection: Boolean) = {
  if(mode == "backlog" & selection) Candidates(Candidates.is + featureId)
  else if (mode == "backlog") Candidates(Candidates.is - featureId)
  else if (mode == "sprint" & selection) EmptySprint(EmptySprint.is + featureId)
  else EmptySprint(EmptySprint.is - featureId)
  Noop
 }
 
 def sprintDisplay(mode: String, f: Feature) = {
  def completion() =  if(mode == "sprint") <td>{MyUtil.formatDate(f.completionDate)}</td> else Empty
  <tr>
	 <td>{SHtml.ajaxCheckbox(false, selected => toggleFeature(f.id, mode, selected))}</td>
     <td>{f.name}</td>
     <td>{f.featureType}</td>
     <td>{f.storyPoints}</td>
     <td>{f.priority}</td>
     <td>{MyUtil.formatDate(f.completionDate)}</td> <!-- {completion()} -->
  </tr>
 }
 
 def backlogStories(): NodeSeq = {
  if(ChosenBacklog.is != null) {
	Feature.findAll(By(Feature.fkPb, ChosenBacklog.is.id), NullRef(Feature.fkSprint)).map( f => sprintDisplay("backlog", f))
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
 
 def sumStories(sprint: Sprint): String = {
  def addPoints(p1: String, p2: String) = if(p1 == "Gogol" || p2 == "Gogol") "Gogol" else {
	  val sum = p1.toLong + p2.toLong
	  sum.toString
  }
	 
  val points = Feature.findAll(By(Feature.fkSprint, sprint.id)).map(f => f.storyPoints.toString)
  ("0" /: points) (addPoints)
 }
 
 def scope(sprint: Sprint): Node = {
  <div>
	<label><b>Begin:</b></label>{MyUtil.formatDate(sprint.sprintBegin)} <br />
	<label><b>End:</b></label>{MyUtil.formatDate(sprint.sprintEnd)} <br />
	<label><b>Number of Working Days:</b></label>{sprint.numberOfWorkingDays} <br /><br />
	<label><b>Sprint Objective</b></label><br />
		 {sprint.purpose}
  </div>	 
 }
 
 def computeVelocity(sprint: Sprint) = {
  if(sprint.numberOfWorkingDays > 0) {
	  if(sumStories(sprint) == "Gogol") "extremely fast" 
	  else {
	 	  val velocity = sumStories(sprint).toLong / sprint.numberOfWorkingDays
	 	  velocity.toString
	  }
  }
  else "--"
 }
 
 def findLatestVelocity(sprint: Sprint) = {
  if(sprint.sprintNumber > 1) {
	  val predecessor = sprint.sprintNumber - 1
	  val sprints = Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), By(Sprint.sprintNumber, predecessor))
	  <span><b>Latest Velocity: </b>{computeVelocity(sprints(0))}</span>
  }
  else <span><b>Latest Velocity: </b>--</span>
 }
 
 def findMedianVelocity() = {
	val sprints = Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), NotNullRef(Sprint.sprintEnd)).map(computeVelocity)
	
	if(sprints.size > 0) {
		val median = sprints.size/2
		<span><b>Median Velocity: </b>{sprints(median)}</span>
	}
	else <span><b>Median Velocity: </b>--</span>
 }
 
 def criteria(): NodeSeq = {
  def ac(f: Feature) = MyUtil.flattenNodeSeq(AcceptanceCriterion.findAll(By(AcceptanceCriterion.fkFeature, f.id)).map(c => <li>{c.text}</li>).toList)
  MyUtil.flattenNodeSeq(Feature.findAll(By(Feature.fkSprint, SelectedSprint.is.id)).map(f => ac(f)).toList)
 }
 
 def sprint (xhtml: NodeSeq): NodeSeq = {
  
  bind("sprint", xhtml, "toProduct"  -> ajaxButton("<<", toProduct _) % ("class" -> "standardButton"),
		              	"toSprint" -> ajaxButton(">>", toSprint _) % ("class" -> "standardButton"),
		              	"backlogStories" -> backlogStories(),
		              	"sprintStories" -> sprintStories(),
		              	"sumStories" -> <span><b>Sum of Story Points: </b>{sumStories(SelectedSprint.is)}</span>,
		              	"latestVelocity" -> findLatestVelocity(SelectedSprint.is),
		              	"medianVelocity" -> findMedianVelocity(),
		              	"scope" -> scope(SelectedSprint.is),
		              	"scopeEditor" -> <div class="mohhowFormLight"><div class="lift:SprintForm"></div></div>,
		              	"acceptanceCriteria" -> criteria())
 }
}