package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import http._
import SHtml._
import S._
import mapper._

import js._
import JsCmds._
import JE.{JsRaw,Str}

import util._
import Helpers._

import scala.xml._
import net.liftweb.common.Full
import net.liftweb.common.Empty
import net.liftweb.common.Box
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}
import js.jquery.JqJsCmds._
import scala.collection.mutable
import org.mohhow.bi.lib.WikiParser
import org.mohhow.bi.lib.Authentification

object ChosenBacklog extends SessionVar[ProductBacklog](null)
object ChosenFeature extends SessionVar[Feature](null)
object RelevantFeature extends SessionVar[Feature](null)
object IsNewFeature extends SessionVar[Boolean](false)
object RelevantMeeting extends SessionVar[Meeting](null)
object RelevantMinutes extends SessionVar[Minutes](null)
object Participants extends SessionVar[mutable.Map[Long, (Boolean, Boolean)]](null)
object ItemToBeCommented extends SessionVar[Long](0)
object SelectedProtocolItem extends SessionVar[ProtocolItem](null)
object ItemsOfFeature extends SessionVar[Set[Long]](Set())
object Feedback extends SessionVar[String](null)

class BacklogSnippet {
	
 val itemClassification = List(("information", S.?("information")), ("task", S.?("task")), ("request", S.?("request")))
	 
 /**
  * generates a single backlog item 
  */
 
 def createBacklogTreeItem(f: Feature): NodeSeq = {
  val subFeatures = Feature.findAll(By(Feature.parentFeature, f.id))
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('featureId')"), selectItem _)._2
  val item = <a>{f.name}</a> % ("onclick" -> action) % new UnprefixedAttribute("featureId", f.id.toString, Null)
  if(subFeatures.length > 0) {
	  <li class="treeItem">
	  	<span class="handle closed">__</span>
	  	<span class="emphasizable">{item}</span>
	  	<ul>{subFeatures.map(createBacklogTreeItem).toSeq}</ul>
	  </li>
  }
  else 
  	  <li class="treeItem">
  		<span class="leaf">__</span>
  		<span class="emphasizable">{item}</span>
  	  </li>
 }
 
 def selectItem(id : String) : JsCmd = {
  val f = Feature.findAll(By(Feature.id, id.toLong)).apply(0)
  ChosenFeature(f)
  JsCmds.SetHtml("featureDisplay", displayFeature(f))
 }  
 
 def findItemsForFeature(f: Feature): scala.collection.immutable.Set[Long] = {
  val items = ProtocolToBacklog.findAll(By(ProtocolToBacklog.fkFeature, RelevantFeature.is.id))
  var aSet = ItemsOfFeature.is 
  
  for(item <- items){
	  aSet += item.id
  }
  
  aSet
 }
 
 /** 
  * creates the initial backlog tree which contains all  root features
  */
 
 def createBacklogTree() : NodeSeq = {
  val features = Feature.findAll(By(Feature.parentFeature, 0), By(Feature.fkPb, ChosenBacklog.is.id), OrderBy(Feature.featureNumber, Ascending))
  
  // set the first choice on the feature with the least feature number
  
  if(!features.isEmpty){
	  ChosenFeature(features.apply(0))
	  RelevantFeature(features.apply(0))
	  ItemsOfFeature(findItemsForFeature(features.apply(0)))
  }
  
  MyUtil.flattenNodeSeq(features.map(createBacklogTreeItem))
 }
 
 def getNextFeatureNumber(backlogId: Long): Long = {
  val allFeature = Feature.findAll(By(Feature.fkPb, backlogId), OrderBy(Feature.featureNumber, Descending))
  
  if(allFeature.isEmpty) 1 else {
		 val maxFeature = allFeature.apply(0)
		 val nextFeatureNumber = 1 + maxFeature.featureNumber
		 nextFeatureNumber
  }
 }
 
 def addFeature(addBelow : Boolean) : JsCmd = {
  if(ChosenBacklog.is != null) {
    var parentId : Long = if(ChosenFeature.is != null && addBelow) ChosenFeature.is.id else if (ChosenFeature.is != null) ChosenFeature.is.parentFeature else 0
    val myFeature = Feature.create
    val newIndex = getNextFeatureNumber(ChosenBacklog.is.id)
    myFeature.parentFeature(parentId).featureNumber(newIndex).fkPb(ChosenBacklog.is)
  	RelevantFeature(myFeature)
  	IsNewFeature(true)
  	ItemsOfFeature(Set())
  	RedirectTo("/backlogEdit")
  }
 }
	
 def editFeature() : JsCmd = {
  if(ChosenFeature.is != null){
	  	RelevantFeature(ChosenFeature.is)
	  	IsNewFeature(false)
		ItemsOfFeature(findItemsForFeature(ChosenFeature.is))
		RedirectTo("/backlogEdit")
  }
  else Alert(S.?("noFeatureSelection"))
 }
  
 def deleteFeature() : JsCmd = {
  if(ChosenFeature.is != null){
  		ChosenFeature.is.delete_!
  		//JsCmds.SetHtml("feature_tree", createBacklogTree())
  		RedirectTo("/backlog")
  }
  else Alert(S.?("noFeatureSelection")) 
 }
  
 def backlogMenu (xhtml: NodeSeq): NodeSeq = {
  bind("backlog", xhtml, "edit"  -> ajaxButton(S.?("edit"), editFeature _) % ("class" -> "standardButton"),
   		                 "add" 	-> ajaxButton(S.?("add"), () => addFeature(false)) % ("class" -> "standardButton"),
   		                 "delete"  -> ajaxButton(S.?("remove"), deleteFeature _) % ("class" -> "standardButton"),
   		                 "addBelow" -> ajaxButton(S.?("addBelow"), () => addFeature(true)) % ("class" -> "standardButton")) 
 }
 
 def backlogTree (xhtml: NodeSeq): NodeSeq = {
  bind("backlog", xhtml, "tree"  -> createBacklogTree()) 
 }
 
 /**
  * next all methods used to display the details of a feature
  * 
  */
 
 def getProtocolItems(f: Feature): NodeSeq = {
  val pTb = ProtocolToBacklog.findAll(By(ProtocolToBacklog.fkFeature, f.id))
  val items = for {
		 			link <- pTb
		 			pItems = ProtocolItem.findAll(By(ProtocolItem.id, link.fkProtocolItem))
		 			pItem = if(pItems.isEmpty) null else pItems(0)
	 			  } yield pItem
		 
  val rows = for {
			 	 	item <- items.filter(_ != null)
			 	 	minutes = Minutes.findAll(By(Minutes.id, item.fkMinutes))
			 	 	minute = if(minutes.isEmpty) null else minutes(0)
			 	 	meeting = Meeting.findAll(By(Meeting.id, minute.fkMeeting)).apply(0)
	 			 } yield <tr><td>{meeting.header}</td><td>{item.itemNumber}</td><td>{item.itemText}</td></tr>
	 			
  rows.toSeq
 }
 
 def getSpecifications(f: Feature): NodeSeq = {
  val bTs = BacklogToSpecification.findAll(By(BacklogToSpecification.fkFeature, f.id))
  val specs = for {
		            link <- bTs
		            spec = Specification.findAll(By(Specification.id, link.fkSpecification)).apply(0)
	 			  } yield <tr><td>{spec.name}</td><td>{spec.description}</td></tr>
	 
	 specs.toSeq
 }
 
 def findDescription(f: Feature) = {
  if(f.description != null && f.description.length > 0 && (f.featureType == S.?("businessQuestion") || f.featureType == S.?("complianceRequest"))) MyUtil.tagIt(WikiParser.parseBusinessQuestion(f.description)._2)
  else f.description 
 }
 
 def displayFeature(f: Feature): NodeSeq = {
  <b>{S.?("title")}: </b><span>{f.name}</span> <br /><br />
  <b>{S.?("type")}: </b><span>{f.featureType}</span><br /><br />
  <b>{S.?("priority")}: </b><span>{f.priority}</span><br /><br />
  <b>{S.?("storyPoints")}: </b><span>{f.storyPoints}</span><br /><br />
  <b>{S.?("description")}: </b> <br /><br /><span>{findDescription(f)}</span><br /><br />
  <b>{S.?("interpretItem")}:</b><br /><br />
  <table class="singleSelection">
	<thead>
		<tr>
			<td>{S.?("meeting")}</td>
			<td>{S.?("itemNumber")}</td>
			<td>{S.?("itemText")}</td>
		</tr>
	</thead>
	<tbody>
		{getProtocolItems(f)}				 
	</tbody>
  </table>
  <br /><br />
  <b>{S.?("specifiedIn")}:</b><br /><br />
  <table class="singleSelection">
	<thead>
		<tr>
			<td>{S.?("specification")}</td>
			<td>{S.?("description")}</td>
		</tr>
	</thead>
	<tbody>
		{getSpecifications(f)}			 
	</tbody>
  </table>
 }
 
 def rowItem(item: ProtocolItem) = {
	def chooseItem(anItem: ProtocolItem, selection: Boolean): JsCmd = {
		if(selection) ItemsOfFeature(ItemsOfFeature.is + anItem.id) else ItemsOfFeature(ItemsOfFeature.is - anItem.id)
		Noop
	}
	
	val itemSelection = !ProtocolToBacklog.findAll(By(ProtocolToBacklog.fkProtocolItem, item.id), By(ProtocolToBacklog.fkFeature, RelevantFeature.is.id)).isEmpty
	  
	<tr><td>{SHtml.ajaxCheckbox(itemSelection, selected => chooseItem(item, selected))}</td><td>{item.findMinutes().findMeeting().header}</td><td>{item.itemText}</td></tr>
 }
 
 def searchItems(text: String): JsCmd = {
  def getLatestPublishedMinutes(meeting: Meeting): List[Minutes] = {
   val candidates = Minutes.findAll(By(Minutes.fkMeeting, meeting.id), By(Minutes.status, "published"), OrderBy(Minutes.version, Descending))
   if(candidates.isEmpty) List() else List(candidates(0)) 
  }
	 
  val items = ProtocolItem.findAll(Like(ProtocolItem.itemText, "%" + text + "%"))
  val meetings = Meeting.findAll(By(Meeting.fkScenario, SelectedScenario.is))
  val actualMinutes = List.flatten(meetings.map(getLatestPublishedMinutes))
  
  val rows = items.filter(item => actualMinutes.exists(anotherItem => anotherItem.id == item.fkMinutes)).map(rowItem).toSeq
  SetHtml("backlogProtocolRefercences", rows)
 }
 
 def showChosenItems(): NodeSeq = {
  val refs = ProtocolToBacklog.findAll(By(ProtocolToBacklog.fkFeature, RelevantFeature.is.id))
  List.flatten(refs.map(ptb => ProtocolItem.findAll(By(ProtocolItem.id, ptb.fkProtocolItem)).toList)).map(rowItem).toSeq
 }
 
 def featureEdit (form: NodeSeq): NodeSeq = {
  val feature = RelevantFeature.is
  bind("feature", form, "searchProtocolItems" -> ajaxText("", text => searchItems(text)) % ("size", "50") % ("maxlength", "50"),
		                "givenProtocolItems" -> showChosenItems())
 }
 
 /**
  * 
  * Next all methods required for meetings, protocols etc.
  * 
  */
 
 def fillParticipants(m: Meeting) = {
  val participants = mutable.Map.empty[Long, (Boolean, Boolean)]
  val recipients = MeetingRecipient.findAll(By(MeetingRecipient.fkMeeting, m.id))
  
  for(recipient <- recipients)  participants + (recipient.fkUser.toLong -> (recipient.isAttendee, recipient.isReviewer))
  Participants(participants) 
 }
 
 def selectMeeting(id : String) : JsCmd = {
  val selectedMeeting = Meeting.findAll(By(Meeting.id,id.toLong)).apply(0)
  fillParticipants(selectedMeeting)
  RelevantMeeting(selectedMeeting)
  Noop
 } 
 
 def meetingAsRow(m: Meeting) = {
	val action = SHtml.ajaxCall(JsRaw("$(this).attr('meeting')"), selectMeeting _)._2
	<tr>
		<td>{m.category}</td>
		<td>{m.topic}</td>
		<td>{MyUtil.formatDate(m.meetingBegin)}</td>
		<td>{MyUtil.timeInDay(m.meetingBegin) + " - " + MyUtil.timeInDay(m.meetingEnd)}</td>
		<td>{MyUtil.formatDate(m.dateCreated)}</td>
	</tr> % new UnprefixedAttribute("meeting", m.id.toString, Null) % ("onclick" -> action)
 }
 
 def getMeetings() = {
  if(SelectedScenario.is != null) {
	  Meeting.findAll(By(Meeting.fkScenario, SelectedScenario.is.id), OrderBy(Meeting.meetingBegin, Ascending)).map(meetingAsRow).toSeq
  }
  else <nothing />
 }
 
 def addMeeting(): JsCmd = {
  val newMeeting = Meeting.create
  newMeeting.fkScenario(SelectedScenario.is).dateCreated(new Date)
  RelevantMeeting(newMeeting)
  val newMinutes = Minutes.create
  newMinutes.version(1).status("initial").dateCreated(new Date)
  RelevantMinutes(newMinutes)
  RedirectTo("/meetingEdit")
 }
 
 def editMeeting(): JsCmd = {
  if(RelevantMeeting.is != null) RedirectTo("/meetingEdit") else Alert(S.?("noMeetingSelection"))
 }
 
 def meetings(xhtml: NodeSeq): NodeSeq = {
  bind("meeting", xhtml, "meetings" ->  getMeetings(),
		                 "add"      ->  ajaxButton(S.?("add"), addMeeting _) % ("class" -> "standardButton"),
		                 "edit"		->  ajaxButton(S.?("edit"), editMeeting _) % ("class" -> "standardButton"))
 }
 
 def chooseParticipant(isParticipant: Boolean, userId: Long, choice: Boolean) : JsCmd = {
  if(Participants.is == null) fillParticipants(RelevantMeeting.is)
   
  if(Participants.is.contains(userId)) {
	  val p = Participants.is(userId)
	  Participants(Participants.is - userId)
	  if(isParticipant && choice) Participants(Participants.is + (userId -> (choice, p._2)))
	  else if (choice) Participants(Participants.is + (userId -> (p._1, choice)))
  }
  else {
	  if(isParticipant && choice) Participants(Participants.is + (userId -> (choice, false)))
	  else if(choice) Participants(Participants.is + (userId -> (false, choice)))
  }
  Noop
 }
 
 def participant(sr: ScenarioRole): Node = {
  val mr = MeetingRecipient.findAll(By(MeetingRecipient.fkUser, sr.fkUser), By(MeetingRecipient.fkMeeting, RelevantMeeting.is.id))
  val isParticipant = !mr.isEmpty && mr(0).isAttendee
  val isInformed = !mr.isEmpty && mr(0).isReviewer
  val participantSelection = SHtml.ajaxCheckbox(isParticipant, selected => chooseParticipant (true, sr.fkUser, selected))
  val informedSelection = SHtml.ajaxCheckbox(isInformed, selected => chooseParticipant (false, sr.fkUser, selected))
  val user = User.findAll(By(User.id, sr.fkUser)).apply(0)
  <tr><td>{participantSelection}</td><td>{informedSelection}</td><td>{user.firstName + " " + user.lastName}</td><td>{sr.role}</td></tr>
 }
 
 def participants(xhtml: NodeSeq): NodeSeq = {
  bind("meeting", xhtml, "participants" ->  MyUtil.filterScenarioRoles(Nil, ScenarioRole.findAll(By(ScenarioRole.fkScenario, SelectedScenario.is.id)).toList).map(participant).toSeq)
 }
 
 def editProtocol() : JsCmd = {
  if(RelevantMeeting.is != null){
	JsRaw("$('#protocolDisplay').fadeOut();$('#protocolEdit').fadeIn();")
  }
  else Noop
 }
 
 def saveProtocol(): JsCmd = JsRaw("$('#protocolEdit').fadeIn();$('#protocolDisplay').fadeOut();")
 
 def getNextProtocolVersionId(meetingId: Long): Long = {
  val versions = Minutes.findAll(By(Minutes.fkMeeting, meetingId), OrderBy(Minutes.id, Descending))

  if(versions.isEmpty) 1 else {
		 val version = versions.apply(0)
		 val nextVersionNumber = 1 + version.version
		 nextVersionNumber
  }
 }
 
 def copyItem(newVersionId : Long, item: ProtocolItem) {
  val newItem = ProtocolItem.create
  newItem.fkMinutes(newVersionId).itemNumber(item.itemNumber).classification(item.classification).itemText(item.itemText).dateCreated(new Date).save 
 }

 def newProtocolVersion() : JsCmd = {
  if(RelevantMeeting.is != null){
    val newMinutes = Minutes.create
    newMinutes.fkMeeting(RelevantMeeting.is).version(getNextProtocolVersionId(RelevantMeeting.is.id)).status("new").save
    var copyIt = (item: ProtocolItem) => copyItem(newMinutes.id, item)
    ProtocolItem.findAll(By(ProtocolItem.fkMinutes, RelevantMinutes.is.id)).map(copyIt)    
    RelevantMinutes(newMinutes)
  }
  RedirectTo("protocol")
 }

 def changeStatus(newStatus : String) : JsCmd = {
  val pv = RelevantMinutes.is
    
  if(pv != null){
    if(newStatus == "published") pv.datePublished(new Date)
	pv.status(newStatus).save
	if(newStatus == "published") RelevantMinutes(pv) else RelevantMinutes(null)
	RedirectTo("protocol")
  }
  else Alert(S.?("noMinutesSelection"))
 }

 def publishProtocol = changeStatus("published")
 def deleteProtocol = changeStatus("deprecated")

 def protocolMenu(xhtml: NodeSeq): NodeSeq = {
  bind("protocol", xhtml, 
	   "newVersion" -> ajaxButton(S.?("newVersion"), newProtocolVersion _) % ("class" -> "standardButton"),
	   "edit" 		-> ajaxButton(S.?("edit"), editProtocol _) % ("class" -> "standardButton"),
	   "delete" 	-> ajaxButton(S.?("deleteVersion"), deleteProtocol _) % ("class" -> "standardButton"),
	   "publish" 	-> ajaxButton(S.?("publish"), publishProtocol _) % ("class" -> "standardButton"))
 }

 def createProtocolListItem(m : Meeting) = {
  val header = m.header
  val pId = m.id.is
  val item = link("/protocol", () => selectProtocol(pId), <span>{header}</span>) 
  <li class="listItem">{item}</li>
 }
 
 def selectProtocol(id : Long) = {
  val meeting = Meeting.findAll(By(Meeting.id, id)).apply(0)
  RelevantMeeting(meeting)
 	  
  val allMinutes = Minutes.findAll(By(Minutes.fkMeeting, id), OrderBy(Minutes.version, Descending), NotBy(Minutes.status, "deprecated"))
  if(!allMinutes.isEmpty) RelevantMinutes(allMinutes.apply(0))
  RedirectTo("/protocol")
 }  

 def selectVersion(id : Long) = {
  RelevantMinutes(Minutes.findAll(By(Minutes.id, id)).apply(0))
  RedirectTo("/protocol")
 }  

 def protocolVersionHeader (): NodeSeq = { 
  if(RelevantMinutes.is != null) {
	<br />
	<h3>{RelevantMeeting.is.header}</h3><br />
	<h3>{RelevantMinutes.is.header}</h3>
	<h3 style="float:right">{S.?("otherVersions")}: {SHtml.ajaxSelect(versionsOfProtocol, Empty , v => {selectVersion(v.toLong)})}</h3><br />
	<br /><hr /><br />
  } 
  else <span />
 }

 def versionsOfProtocol () = { 

  if(RelevantMeeting.is != null){
    val minutes = Minutes.findAll(By(Minutes.fkMeeting, RelevantMeeting.is.id.is), NotBy(Minutes.status, "deprecated"))
        	
    if(RelevantMinutes.is != null) minutes.filterNot(_.version == RelevantMinutes.is.version).map(v => (v.id.toString, v.version.toString)).toSeq
    else minutes.map(v => (v.id.toString, v.version.toString)).toSeq
        	
  }  
  else Nil
 }

 def showParticipants(): NodeSeq = {
  def getParticipant(participant: MeetingRecipient) = {
	  val users = User.findAll(By(User.id, participant.fkUser))
	  
	  if(!users.isEmpty) users(0).firstName + " " + users(0).lastName
	  else ""
  }
  
  def printUser(usrs: List[String]): String = usrs match {
   case List() => ""
   case List(aUser) => aUser
   case head :: trail => if(head.length > 0) head + ", " + printUser(trail) else printUser(trail)
  }
  
  def isJustReviewer(participant: MeetingRecipient) = participant.isReviewer && !participant.isAttendee
  
  if(RelevantMeeting.is != null){
	  val participants = MeetingRecipient.findAll(By(MeetingRecipient.fkMeeting, RelevantMeeting.is.id))
	  
	  <h3>{S.?("attendees")}</h3><br />
	  <span>{printUser(participants.filter(_.isAttendee).map(getParticipant))}</span><br /><br />
	  <h3>{S.?("reviewer")}</h3><br />
	  <span>{printUser(participants.filter(isJustReviewer).map(getParticipant))}</span>
	  
  } else <nothing />
 }
 
 def createReviewBlock(): NodeSeq = {
  def sendFeedback(): JsCmd = {
	  if(Feedback.is != null) {
	 	  
	 	  val user = User.currentUser openOr null
	 	  val recipient = findRecipient(user)
	 	  if(recipient != null) recipient.feedbackStatus(Feedback.is).save
	 	  RedirectTo("/protocol")
	  }
	  else Alert(S.?("noFeedbackStatusSelection"))
  }
  
  def selectFeedback(status: String): JsCmd = {
	Feedback(status)
	Noop
  }
  
  def rejection(status: String) = if(status == "rejected") <span>{S.?("disagreementOnMinutes")}</span> else <span />
  
  def findRecipient(user: User) = {
   val recipients = MeetingRecipient.findAll(By(MeetingRecipient.fkMeeting, RelevantMeeting.is.id), By(MeetingRecipient.fkUser, user.id))
   if(!recipients.isEmpty) recipients(0) else null
  }
  
  if(RelevantMeeting.is != null && RelevantMinutes.is != null && RelevantMinutes.is.status == "published") {
	  val user = User.currentUser openOr null
	  val recipient = findRecipient(user)
	  
	  if(recipient != null) {
	 	  
	 	  if(recipient.feedbackStatus == "accepted") <p>{S.?("alreadyAgreedWithMinutes")}</p>
	 	  else {
	 	 	 
	 	 	  val action = SHtml.ajaxCall(JsRaw("$(this).val()"), selectFeedback _)._2
	 	 	  val acceptIt =  <input type="radio" name="reviewFeedback" value="accepted" /> % ("onchange" -> action)
	 	 	  val rejectIt = <input type="radio" name="reviewFeedback" value="rejected" /> % ("onchange" -> action)
				
	 	 	  <p>
	 	 	  	{rejection(recipient.feedbackStatus)}<br />
	 		  	{acceptIt} {S.?("agreeWithMinutes")}<br />
	 		  	{rejectIt} {S.?("rejectMinutes")}<br />
	 		  	{ajaxButton(S.?("send"), sendFeedback _) % ("class" -> "standardButton") % ("style" -> "float: right")}
	 		  </p>
	 	  }
	  }
	  else <nothing />
  }
  else <nothing />
 }
 
 def showComment(c: ProtocolComment) = {
  val usrs = User.findAll(By(User.id, c.fkUser))
	 
  if(usrs.isEmpty) <tr class="commentRow"><td></td><td></td><td>{c.comment}</td></tr>
  else <tr class="commentRow"><td></td><td></td><td>{"[" + usrs(0).firstName + " " + usrs(0).lastName + "] " + c.comment}</td></tr>
 }
 
 def cancelComment(): JsCmd = {
  Unblock
 }
 
 def saveComment(commentText: String): JsCmd = {
  val newComment = ProtocolComment.create
  newComment.fkProtocolItem(ItemToBeCommented.is).fkUser(User.currentUser).comment(commentText).dateCreated(new Date).save
  CmdPair(Unblock, RedirectTo("protocol"))
 }

 def protocolDisplay(xhtml: NodeSeq): NodeSeq = {
	bind("protocol", xhtml, "list" 	-> Meeting.findAll(By(Meeting.fkScenario, SelectedScenario.is.id), OrderBy(Meeting.meetingBegin, Ascending)).map(createProtocolListItem).toSeq,
			        "header" 		-> protocolVersionHeader (),
			        "participants" 	-> showParticipants(),
			        "items" 		-> showProtocolItems("display"),
			        "review" 	 	-> createReviewBlock(),
			        "cancelComment" -> ajaxButton(S.?("cancel"), cancelComment _) % ("class" -> "standardButton") % ("style" -> "float: right"),
			        "saveComment" 	-> <button class='standardButton' style='float: right'>{S.?("save")}</button> % ("onclick" -> SHtml.ajaxCall(JsRaw("$('#commentEdit').val()"), saveComment _)._2))
 }

 def updateProtocolItem(attribute: String, protocolItemId: Long, text: String){
	 println(text)
	val oneItemList = ProtocolItem.findAll(By(ProtocolItem.id, protocolItemId))
	if(!oneItemList.isEmpty){
		val oneItem = oneItemList.apply(0)
		
		if(attribute == "classification") oneItem.classification(text) else oneItem.itemText(text)
		
		if(oneItem.classification.toString == null || oneItem.classification.toString.length == 0) oneItem.classification("information")
		oneItem.save
	}
	Noop
 }

 def getProtocolItem(mode: String, item: ProtocolItem) = {
  def writeComment(itemId: Long): JsCmd = {
	  ItemToBeCommented(itemId)
	  
	  ModalDialog(<div class="modalMessage">
		  			<lift:BacklogSnippet.protocolDisplay>
		  				<label>{S.?("enterComment")}</label><br />
			  			<textarea id="commentEdit" rows="8" cols="45" style="margin: 4px"/>
                		<protocol:saveComment /><protocol:cancelComment/>
                	</lift:BacklogSnippet.protocolDisplay>
              	</div>)
  }
  def plainComment() = writeComment(item.id)
  
  def selectMinuteItem(id : String) : JsCmd = {
   val item = ProtocolItem.findAll(By(ProtocolItem.id, id.toLong)).apply(0)
   SelectedProtocolItem(item)
   JsRaw("$('.editProtocolItem').removeClass('emphasize');$(this).addClass('emphasize');")
  } 
  
  val commentIt = ajaxButton(S.?("addComment"), plainComment _) % ("class" -> "standardButton") % ("style" -> "float: right")
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('itemId')"), selectMinuteItem _)._2
  
  if(mode == "display"){
	  
	  val comments = ProtocolComment.findAll(By(ProtocolComment.fkProtocolItem, item.id)).map(showComment).toSeq
	  val row = <tr><td>{item.itemNumber}</td><td>{S.?(item.classification)}</td><td>{item.itemText}{commentIt}</td></tr> % new UnprefixedAttribute("itemId", item.id.toString, Null) 
	   
	  if(comments.isEmpty) row else MyUtil.flattenNodeSeq(List(row, comments))
  }
  else{
	  
	  <tr>
       <td>{item.itemNumber}</td>
       <td>{SHtml.ajaxSelect(itemClassification, Full(item.classification) , text => updateProtocolItem ("classification",item.id, text))}</td>
       <td>{SHtml.ajaxTextarea(item.itemText, text => updateProtocolItem ("itemText", item.id, text))}</td>
      </tr> % ("onclick" -> action) % new UnprefixedAttribute("class", "editProtocolItem", Null) % new UnprefixedAttribute("itemId", item.id.toString, Null)
  }
 }
 
 def getDisplayItem(item: ProtocolItem) = getProtocolItem("display", item)
 def getEditItem(item: ProtocolItem) = getProtocolItem("edit", item)

 def showProtocolItems (mode: String): NodeSeq = {
  if(RelevantMinutes.is != null){
	val items = ProtocolItem.findAll(By(ProtocolItem.fkMinutes, RelevantMinutes.is.id), OrderBy(ProtocolItem.itemNumber, Ascending))
		 
	if(mode == "display") {
		<table class="protocolTable">
			<col width="5%" text-align="center"/>
			<col width="10%" text-align="center"/>
			<col width="85%" />
			{items.map(getDisplayItem).toSeq}
		</table>
	}
	else {
		<table id="protocolItemEditTable" class="protocolTable">
			<col width="5%" text-align="center"/>
			<col width="10%" text-align="center"/>
			<col width="85%" />
		    <thead>
		    	<tr>
                    <td>{S.?("itemNumber")}</td>
                    <td>{S.?("classification")}</td>
                    <td>{S.?("itemText")}</td>
		        </tr>
		    </thead>
		    <tbody>
			 	{items.map(getEditItem).toSeq}
			</tbody>
		</table>
	} 
  }
  else <span />
 }
 
 def getNextItemNumber(protocolVersionId: Long): Long = {
  val items = ProtocolItem.findAll(By(ProtocolItem.fkMinutes, protocolVersionId), OrderBy(ProtocolItem.itemNumber, Descending))

  if(items.isEmpty) 1 
  else {
   val item = items.apply(0)
   val nextNumber = 1 + item.itemNumber
   nextNumber
  }
 }
 
 def addItem() : JsCmd = {
  if(RelevantMinutes.is != null){
	  val pv = RelevantMinutes.is
	  val pi = ProtocolItem.create
	  val itemNumber = getNextItemNumber(pv.id)
	  pi.fkMinutes(pv).dateCreated(new Date).itemNumber(itemNumber)
	  pi.save
	  SetHtml("protocolItemEditTableContainer", showProtocolItems("edit"))
  }
  else Alert(S.?("NoMinutesSelection"))
 }

 def removeItem() : JsCmd = {
  if(SelectedProtocolItem.is != null) {
	  val item = SelectedProtocolItem.is
	  item.delete_!
	  SelectedProtocolItem(null)
	  SetHtml("protocolItemEditTableContainer", showProtocolItems("edit"))
  }
  else Alert(S.?("noItemSelection"))
 }
 
 def saveQuit(): JsCmd = RedirectTo("protocol")

 def protocolItemEdit(xhtml: NodeSeq): NodeSeq = {
	bind("protocol", xhtml, "addItem" 		-> ajaxButton(S.?("add"), addItem _) % ("class" -> "standardButton"),
					 		"saveQuit" 		-> ajaxButton(S.?("saveQuit"), saveQuit _) % ("class" -> "standardButton"),
			                "removeItem" 	-> ajaxButton(S.?("remove"), removeItem _) % ("class" -> "standardButton"),
			                "itemEdit" 		-> showProtocolItems(S.?("edit")))
 }
}