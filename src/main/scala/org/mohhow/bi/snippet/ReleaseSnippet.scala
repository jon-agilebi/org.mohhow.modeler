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
import org.mohhow.bi.lib.Repository;
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}

object SelectedRelease extends SessionVar[Release](null)

class ReleaseSnippet {
	
 def selectRelease(id : Long) = {
  val release = Release.findAll(By(Release.id, id)).apply(0)
  SelectedRelease(release)
 } 
	
 def createListItem(release : Release) = {
  val name = "Release " + release.prettyNumber
  val myId = release.id.is
  val item = link("/release", () => selectRelease(myId), <span>{name}</span>)
  <li class='listItem'>{item}</li>
 }
 
 def showStatus(): NodeSeq = {
   val release = SelectedRelease.is

   if(release == null) <nothing />
   else {
	   <div>
	   	<label>Release Kind: </label>{release.kind} <br/>
	   	<label>Status: </label>{release.status} <br/>
	   	<label>Begin: </label>{MyUtil.formatDate(release.begin)} <br/>
	   	<label>Scheduled End: </label>{MyUtil.formatDate(release.scheduledEnd)} <br/>
	   	<label>Announced End: </label>{MyUtil.formatDate(release.announcementDate)} <br/>
	   	<label>Freeze Date: </label>{MyUtil.formatDate(release.end)} <br/>
	   </div>
   }
 }
 
 def artefacts(): NodeSeq = {
   def nothing() = {}
   val release = SelectedRelease.is

   if(release == null) <nothing />
   else {
	   <div>
	   	 <h4>Metadata</h4>
	   		<br />
	  			{link("/vision", nothing , <span>metadata.xml</span>)}
	  		<br /><br />
	   	<h4>DDL</h4>
	   </div>
   }
 }
 
 def delta(): NodeSeq = {
   val release = SelectedRelease.is

   if(release == null) <nothing />
   else {
	   <div>
	   	<br />
	   	<h4>DDL</h4>
	   </div>
   }
 }
 
 
 
 def showRelease(release: Release): Node = {
  
	 <div>
		 <h4>Release {release.prettyNumber}</h4>
	   	<label>Release Kind: </label>{release.kind} <br/>
	   	<label>Status: </label>{release.status} <br/>
	   	<label>Begin: </label>{org.mohhow.bi.util.Utility.formatDate(release.begin)} <br/>
	   	<label>Scheduled End: </label>{org.mohhow.bi.util.Utility.formatDate(release.scheduledEnd)} <br/>
	   	<label>End: </label>{org.mohhow.bi.util.Utility.formatDate(release.end)} <br/>
	   </div>
 }
 
 def overview (xhtml: NodeSeq): NodeSeq = {
  bind("release", xhtml, "all"  -> Release.findAll().map(showRelease).toSeq)
 }
 
 def createNewRelease(): JsCmd = {
  val newRelease = Release.create
  newRelease.creationDate(new Date).begin(new Date)
  SelectedRelease(newRelease)
  RedirectTo("/releaseEdit")
 }
 
 def changeReleaseStatus(targetStatus: String): JsCmd = {
  val release = SelectedRelease.is
  if(release != null) {
	  if(targetStatus == "announced") {
	 	  if(release.status != "active"){
	 	 	  Alert("You can only make announcements on active projects")
	 	  }
	 	  else {
	 		  release.status("announced").announcementDate(new Date).save
	 		  RedirectTo("/release")
	 	  }
	  }
	  else if(targetStatus == "freezed")
	  {
	 	  if(release.status != "announced"){
	 	 	  Alert("You can only freeze announced projects")
	 	  }
	 	  else {
	 	 	  release.status("freezed").freezeDate(new Date).save
	 	 	  RedirectTo("/release")
	 	  }
	  }
	  else Noop
  }
  else Alert("No Release chosen")
 }
 
 def freeze = changeReleaseStatus("freezed")
 def announce = changeReleaseStatus("announced")

 def menu (xhtml: NodeSeq): NodeSeq = {
  bind("menu", xhtml, "create"  -> ajaxButton("Create New Release", createNewRelease _) % ("class" -> "standardButton"),
		              "announce" -> ajaxButton("Announce Release Freeze", announce _) % ("class" -> "standardButton"),
		              "freeze" -> ajaxButton("Freeze", freeze _) % ("class" -> "standardButton"),
		              "generate" -> ajaxButton("Generate", Noop _) % ("class" -> "standardButton"),
		              "show" -> Release.findAll().map(createListItem),
		              "showStatus" -> showStatus(),
		              "artefacts" -> artefacts(),
		              "delta" -> delta())
 }
 
 def getScenarioWithReleases(sc: Scenario): Node = {
  val releases = Release.findAll(By(Release.fkScenario, sc.id))
  if(releases.isEmpty) <li class='listItem'>{sc.name}</li> else <li class='listItem'>{sc.name}<ul>{releases.map(createListItem).toSeq}</ul></li>
 }
 
 def allScenariosWithReleases (xhtml: NodeSeq): NodeSeq = {
  bind("deployment", xhtml, "releases" -> Scenario.findAll().map(getScenarioWithReleases).toSeq,
	"environments" -> <nothing />)
 }
}