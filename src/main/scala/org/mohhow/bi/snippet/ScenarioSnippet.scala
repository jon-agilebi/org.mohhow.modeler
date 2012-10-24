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
import org.mohhow.bi.lib.WikiParser
import org.mohhow.bi.util.{Utility => MyUtil}
import java.util.Date
import scala.util.matching.Regex

import net.liftweb.common.Full
import net.liftweb.common.Box

object ScenarioToEdit extends SessionVar[Scenario](null)
object SelectedScenario extends SessionVar[Scenario](null)

object Setup extends SessionVar[Node](null)
object Vision extends SessionVar[NodeSeq](null)
object Blocks extends SessionVar[NodeSeq](null)
object SectionText extends RequestVar[String](null)
object SelectedSectionNumber extends SessionVar[Long](0)
object Units extends SessionVar[List[(Long, String, String, String)]](Nil)
object SectionHeader extends SessionVar[List[(Long, String, Long, String)]](null)
object SectionHeaderEdit extends SessionVar[(String, String)](null)
object SelectedUnitId extends SessionVar[Long](0)

class ScenarioSnippet {

 /**
  * Here you find the code for creating and updating scenarios. Furthermore, the code to handle the scenario setup 
  * and to work with the product vision
  *
  * At first comes the code for scenario creation and editing
  */
	
 /**
  * create a new scenario and open the edit page for further processing	
  */
	
 def canCreateScenarios(): Boolean = {
  val user = User.currentUser openOr null
  if(user != null) {
	  val sumItUp = (0 /: ClientToUser.findAll(By(ClientToUser.fkUser, user.id)).map(link => link.canBeOwner.toInt)) (_ + _)
	  if(sumItUp > 0) return true;
  }
  
  return false;
 }
	
 def createScenario() : JsCmd = {
  
  if(canCreateScenarios()) {
   val newScenario = Scenario.create
   newScenario.dateCreated(new Date)
   ScenarioToEdit(newScenario)
   RedirectTo("/scenarioEdit")  
  }
  else Alert(S.?("cannotCreateScenario"))  
 }
 
 /**
  * open the edit page 
  */
 
 def canEditThisScenario(scenario: Scenario): Boolean = {
  val user = User.currentUser openOr null
  if(user != null) {
	  val sumItUp = (0 /: ClientToUser.findAll(By(ClientToUser.fkUser, user.id), By(ClientToUser.fkClient, scenario.fkClient)).map(link => link.canBeOwner.toInt)) (_ + _)
	  if(sumItUp > 0) return true;
  }
  
  return false;
 }
 
 def editScenario() : JsCmd = {
  if(SelectedScenario.is != null){
	  
	  if(canEditThisScenario(SelectedScenario.is)) {
	 	  ScenarioToEdit(SelectedScenario.is)
	 	  RedirectTo("/scenarioEdit")
	  }
	  else Alert(S.?("cannotEditScenario"))
  }
  else Alert(S.?("noScenarioSelection"))
 }
 
 def selectedScenarioName(): String = {
  if(SelectedScenario.is != null) SelectedScenario.is.name
  else S.?("noScenarioSelection")
 }
 
 /**
  * remove the selected scenario
  * 
  */
 
 def removeScenario() : JsCmd = {
  if(SelectedScenario.is != null){
	  val theScenario = SelectedScenario.is
	  val backlogs = ProductBacklog.findAll(By(ProductBacklog.fkScenario, theScenario.id))
	  
	  if(Measure.findAll(By(Measure.fkScenario, theScenario.id)).isEmpty &&
	     Meeting.findAll(By(Meeting.fkScenario, theScenario.id)).isEmpty &&
	     ModelVertex.findAll(By(ModelVertex.fkScenario, theScenario.id)).isEmpty &&
	     Release.findAll(By(Release.fkScenario, theScenario.id)).isEmpty &&
	     Specification.findAll(By(Specification.fkScenario, theScenario.id)).isEmpty &&
	     Sprint.findAll(By(Sprint.fkScenario, theScenario.id)).isEmpty &&
	     ScenarioRole.findAll(By(ScenarioRole.fkScenario, theScenario.id)).isEmpty &&
	     (backlogs.isEmpty || Feature.findAll(By(Feature.fkPb, backlogs(0).id)).isEmpty)) {
	 	  
	 	  Scenario.findAll(By(Scenario.id, SelectedScenario.is.id)).map(selection => selection.delete_!)
	 	  SelectedScenario(null)
	 	  RedirectTo("/index")
	  }
	  else Alert(S.?("scenarioCannotBeDeleted"))
	  
  }
  else Alert(S.?("noScenarioSelection"))
 }
 
 /**
  * create one row for the scenario overview table. The selection of the checkbox in the first row determines the selected scenario for the  complete application.
  */
 
 def scenarioRow(scenario: Scenario) = { 
  val scenarioSelection = SelectedScenario.is != null && SelectedScenario.is.id == scenario.id
  <tr><td>{SHtml.ajaxCheckbox(scenarioSelection, selected => chooseScenario (scenario.id, selected))}</td><td>{scenario.name}</td><td>{MyUtil.getUserName(scenario.owner)}</td><td>{org.mohhow.bi.util.Utility.formatDate(scenario.dateCreated)}</td></tr>
 }
 
 def chooseScenario(scenarioId: Long, selected: Boolean): JsCmd = {
  def getUser(userId: Long) = {
   val usrs = User.findAll(By(User.id, userId))
	  if(usrs.isEmpty) null else usrs(0)
  }
  if(selected){
	  SelectedScenario(Scenario.findAll(By(Scenario.id, scenarioId.toLong)).apply(0))
	  val backlogs = ProductBacklog.findAll(By(ProductBacklog.fkScenario, scenarioId));
	  if (!backlogs.isEmpty) ChosenBacklog(backlogs.apply(0))
	  try {
		  Setup((Repository.read("scenario", scenarioId, "setup","setup", -1) \\ "setup").map(Utility.trim).apply(0))
		  Vision((Repository.read("scenario", scenarioId, "vision", "vision", -1) \\ "vision").apply(0))
		  Units(addToUnitModel(Nil, Setup.is \\ "unit"))
		  Blocks((Repository.read("scenario", scenarioId, "blocks", "blocks", -1) \\ "block"))
		  
		  ScenarioOwner(getUser(SelectedScenario.is.owner))
		  Analysts(MyUtil.findRoles(scenarioId, "analyst"))
		  Designer(MyUtil.findRoles(scenarioId, "designer"))
		  ReleaseManager(MyUtil.findRoles(scenarioId, "releaseManager"))
	  } 
	  catch {
	 	  case ex: Exception => println("Exception during scenario choice: " + ex.toString)
	  }
  }
  RedirectTo("/index") 
 }
 
 /**
  * collects all detail information to be displayed
  */
 
 def selectedDescription(): NodeSeq = {
  def getLatestSprint(): Option[Sprint] = {
	val sprints =  Sprint.findAll(By(Sprint.fkScenario, SelectedScenario.is.id), OrderBy(Sprint.sprintNumber, Descending))
	if (sprints.isEmpty) None else Some(sprints.apply(0)) 
  }	
  
  def getSprintObjective(s: Option[Sprint]) = s match {
	  case None => ""
	  case Some(sprint) => sprint.purpose
  }
  
  def getSprintSchedule(s: Option[Sprint]) = s match {
	  case None => ""
	  case Some(sprint) => MyUtil.formatDate(sprint.sprintBegin) + " - " + MyUtil.formatDate(sprint.sprintEnd)
  }
  
  def getFeatures(spr: Sprint) = Feature.findAll(By(Feature.fkSprint, spr.id)).map(f => <li>{f.featureNumber.toString + " " + f.name}</li>).toSeq
  
  def getSprintBacklog(s: Option[Sprint]) = s match {
	  case None => ""
	  case Some(sprint) => <ul style="list-style-position:inside">{getFeatures(sprint)} </ul>
  }

  def nothing() = {}
  
  def clientLongName(sc: Scenario) = {
   var result = ""
   if(sc != null ) {
    val clients = Client.findAll(By(Client.id, sc.fkClient))
	if(!clients.isEmpty) result = clients(0).longName
   }
   result
  }
  
  if(SelectedScenario.is != null){
	 val sprint = getLatestSprint()
	 
	 <div>
	  <h4>{S.?("description")}</h4>
	  {SelectedScenario.is.description}
	  <br /><br />
	  <h4>{S.?("namespace")}</h4>
	  {S.?("prefix")}: {SelectedScenario.is.prefix} <br />
	  {S.?("url")}: {SelectedScenario.is.url}<br />
	  {S.?("client")}: {clientLongName(SelectedScenario.is)}
	  <br />
	  <br />
 
	  <h4>{S.?("actualSprint")}</h4>
      <br />
	  {S.?("sprintObjective")}: {getSprintObjective(sprint)} <br /><br />
	  {S.?("sprintBacklog")}: <br /><br /> {getSprintBacklog(sprint)}<br /><br />
      {S.?("sprintSchedule")}: {getSprintSchedule(sprint)}
	 </div>
  } 
  else <div />
 }
 
 def allScenariosOfUserClients() = {
  if (User.loggedIn_?) {
   val user = User.currentUser openOr null
   if(user != null) Scenario.findAll(ByList(Scenario.fkClient, user.clients.all.map(_.id.toLong).toList)).map(scenarioRow).toSeq
   else NodeSeq.Empty 	  
  }
  else NodeSeq.Empty
 }
 
 def help(): JsCmd = RedirectTo("/static/" + S.?("userGuide"))
 	
 def scenarios(xhtml: NodeSeq) = {
  bind("scenario", xhtml,
   	   "add"     -> ajaxButton(S.?("addScenario"), createScenario _) % ("class" -> "standardButton"),
   	   "edit"    -> ajaxButton(S.?("editScenario"), editScenario _) % ("class" -> "standardButton"),
   	   "remove"  -> ajaxButton(S.?("removeScenario"), removeScenario _) % ("class" -> "standardButton"),
   	   "display" -> <span align="right">{selectedScenarioName}</span>,
   	   "allScenarios" -> allScenariosOfUserClients(),
   	   "selectedDescription" -> selectedDescription(),
   	   "help" -> ajaxButton("?", help _) % ("class" -> "standardButton") % ("align" -> "right"))
 }
 		
 /**
  * Scenario setup, units at first
  * Unit values are stored in a list; saving here means to write this list into an XML node of the setup file
  */
 
 def updateUnits(units: List[(Long, String, String, String)], unitId: Long, selectionKind: String, text : String): List[(Long, String, String, String)] = units match {
	 case Nil => Nil
	 case head :: tail => {
		 head match {
			 case (id, subject, name, symbol) =>
			 	if(id == unitId) {
			 		selectionKind match {
			 			case "subject" =>  (id, text, name, symbol) :: tail
			 			case "name" => (id, subject, text, symbol) :: tail
			 			case "symbol" => (id, subject, name, text)  :: tail
			 			case "remove" => tail
			 		}
			 	}
			 	else  head :: updateUnits(tail, unitId, selectionKind, text)
	      } 
	 }
 }
 
 def saveText(unitId: Long, selectionKind: String, text : String) : JsCmd = {
  val newUnits = updateUnits(Units.is, unitId, selectionKind, text)
  Units(newUnits)
  Noop
 }

 def createUnitModel(unit: Node, unitId: Long) = (unitId, MyUtil.getNodeText((unit \\ "subject").apply(0)), MyUtil.getNodeText((unit \\ "name").apply(0)), MyUtil.getNodeText((unit \\ "symbol").apply(0)))
  
 def addToUnitModel(unitModel: List[(Long, String, String, String)], units: NodeSeq): List[(Long, String, String, String)] = {
  def nextId(l:  List[(Long, String, String, String)]) = if(l.isEmpty) 0 else l.map(_._1).sort(_ > _).head + 1
  if(units.isEmpty) unitModel else addToUnitModel(createUnitModel(units.head, nextId(unitModel)) :: unitModel, units.tail) 
 }
 
 def selectUnit(id : String) : JsCmd = {
  SelectedUnitId(id.toLong)
  JsRaw("$(this).addClass('emphasized')")
 }

 def showUnit(item: (Long, String, String, String)) = item match {
  case (id, subject, name, symbol) => {
	
	  val action = SHtml.ajaxCall(JsRaw("$(this).attr('unitId')"), selectUnit _)._2
	  <tr>
	  	<td>
	  		{ajaxText(subject, text => saveText(id, "subject", text)) % ("size" -> "20") % ("maxLength" -> "100")}
	  		
	    </td>
	  	<td>
	  		{ajaxText(name, text => saveText(id, "name", text)) % ("size" -> "20") % ("maxLength" -> "100")}
	    </td>
	  	<td>
	  		{ajaxText(symbol, text => saveText(id, "symbol", text)) % ("size" -> "10") % ("maxLength" -> "10")}
	    </td>
	  </tr> % ("onclick" -> action) % new UnprefixedAttribute("unitId", id.toString, Null) 
  }
  case _ => <nothing />
 }
	
 def addUnit: JsCmd = {
  Units(addToUnitModel(Units.is, <unit><subject></subject><name></name><symbol></symbol></unit>)) 
  SetHtml("unitTableRows", Units.is.map(showUnit).toSeq)
 }
 
 def removeUnit: JsCmd = {
  Units(updateUnits(Units.is, SelectedUnitId.is, "remove", ""))
  SetHtml("unitTableRows", Units.is.map(showUnit).toSeq)
 }
 
 def saveUnits(): JsCmd = {
  def serializeModel(modelItem: (Long, String, String, String)) = <unit><subject>{modelItem._2}</subject><name>{modelItem._3}</name><symbol>{modelItem._4}</symbol></unit>
  val units = <units>{MyUtil.flattenNodeSeq(Units.is.map(serializeModel))}</units>
  val transform = "units" #> units
  val setup = Setup.is
  Setup(transform(setup).apply(0))
  Repository.write("scenario", SelectedScenario.is.id, "setup","setup", -1, Setup.is) 
  Noop
 }
  
 def unitSetup (xhtml: NodeSeq): NodeSeq = {
  bind("unit", xhtml, "rows"  -> Units.is.map(showUnit).toSeq,
	   "add" -> ajaxButton("+", addUnit _) % ("class" -> "standardButton"),
	   "remove" -> ajaxButton("-", removeUnit _) % ("class" -> "standardButton"),
	   "save" -> ajaxButton(S.?("save"), saveUnits _) % ("class" -> "standardButton"))
 }
 
 /**
  * Here comes the setup for the product vision
  */
 
 def selectSection(sectionNumber: String): JsCmd = {
  SelectedSectionNumber(sectionNumber.toLong)
  JsRaw("$('#visionTable tbody tr').removeClass('zebraHover');$('#visionTable tbody tr[sectionNumber=" + sectionNumber +"]').addClass('zebraHover');")
 }
 
 def createSectionHeader(section: Node) = {  
  val sectionNumber = MyUtil.getNodeText((section \\ "sectionNumber").apply(0)).toLong
  val title = MyUtil.getNodeText((section \\ "title").apply(0))
 
  (sectionNumber, title, sectionNumber, "unchanged")
 }
 
 def changeSectionText(sectionNumber: Long, text: String): JsCmd = {
  def changeTextInList(l: List[(Long, String, Long, String)], sectionNumber: Long, text: String): List[(Long, String, Long, String)] = l match  {
  	case Nil => Nil
  	case (sn, st, snOld, status) :: tail => {
  		if(sn == sectionNumber)  (sn, text, snOld, status) :: changeTextInList(tail, sectionNumber, text) else (sn, st, snOld, status) :: changeTextInList(tail, sectionNumber, text)
  	}
  }
  
  SectionHeader(changeTextInList(SectionHeader.is, sectionNumber, text))
  Noop
 }
 
 def showSectionHeader(sectionHeader: (Long, String, Long, String)) = sectionHeader match {
  case (sectionNumber, title, oldNumber, status) => {
	  def sectionHead(t: String) = changeSectionText(sectionNumber, t)
	  val action = SHtml.ajaxCall(JsRaw("$(this).attr('sectionNumber')"), selectSection _)._2
	  val textAction = SHtml.ajaxCall(JsRaw("$(this).val()"), sectionHead _)._2
	  val text = <input type="text" value={title} size="100" maxlength="100" /> % ("onblur" -> textAction)
	  <tr><td>{sectionNumber}</td><td>{text}</td></tr> % ("onclick" -> action)	% ("sectionNumber" -> sectionNumber.toString)
  }
 }
 
 def showAllSectionHeader () = {
  if(SectionHeader.is == null ) SectionHeader((Vision.is \\ "section").map(createSectionHeader).toList.sortBy( _._1 ))
  SectionHeader.is.map(showSectionHeader).toSeq
 }

 def addSection(): JsCmd = {
  val maxSectionNumber = SectionHeader.is.sortBy( _._1 ).last._1
  val newSectionNumber = maxSectionNumber + 1
  SectionHeader(SectionHeader.is ::: List(( newSectionNumber, "<title>", newSectionNumber, "new")))
  val cmd1 = SetHtml("#visionTableRows", showAllSectionHeader())
  val cmd2 = JsRaw("$('#visionTable tbody tr').removeClass('zebraHover');$('#visionTable tbody tr[sectionNumber=" + newSectionNumber +"]').addClass('zebraHover');")
  CmdPair(cmd1, cmd2)
 }
 
 def removeSection(): JsCmd = {
  def reEnumerate(l: List[(Long, String, Long, String)], sectionNumber: Long): List[(Long, String, Long, String)] = l match {
	  case Nil => Nil
	  case (sn, st, snOld, status) :: tail => (sectionNumber, st, snOld, status) :: reEnumerate(tail, sectionNumber + 1)
  }
  
  if(SelectedSectionNumber.is > 0){
	 SectionHeader(reEnumerate(SectionHeader.is.filterNot(_._1 == SelectedSectionNumber.is), 1))
	 SetHtml("#visionTableRows", showAllSectionHeader())
  }
  else Alert("There is no section selected") 
 }
 
 def swapList(l: List[(Long, String, Long, String)], sectionNumber: Long, direction: String): List[(Long, String, Long, String)] = l match  {
  case Nil => Nil
  case (sn, st, snOld, status) :: tail => {
	if((direction == "down" && sn == sectionNumber) || (direction == "up" && sn == sectionNumber + 1)) {
		(sn - 1, st, snOld, status) :: swapList(tail, sectionNumber, direction)
	}
	else if ((direction == "down" && sn == sectionNumber - 1) || (direction == "up" && sn == sectionNumber)) {
		(sn + 1, st, snOld, status) :: swapList(tail, sectionNumber, direction)
	}
	else (sn, st, snOld, status) :: swapList(tail, sectionNumber, direction)
  }
 }
 
 def swapSections(direction: String): JsCmd = {
  if(SelectedSectionNumber.is  > 0){
	 SectionHeader(swapList(SectionHeader.is, SelectedSectionNumber.is, direction).sort(_._1 < _._1))
	 SetHtml("#visionTableRows", showAllSectionHeader())
  }
  else Alert("There is no section selected") 
 }
 
 def findSection(sectionNumber: Long) = {
  def isTheNumber(section: Node) = MyUtil.getNodeText((section \\ "sectionNumber").apply(0)).toLong == sectionNumber
  (Vision.is \\ "section").filter(isTheNumber).apply(0)
 }
 
 def sections(header: List[(Long, String, Long, String)]): List[Node] = header match {
 
  case Nil => Nil
  case (sectionNumber, title, oldNumber, status ) :: tail => {
   var editText: String = ""
   var displayText: String = ""

   if(status != "new") {
	val oldSection = findSection(oldNumber)
	editText = MyUtil.getNodeText((oldSection \\ "editText").apply(0))
	displayText = MyUtil.getNodeText((oldSection \\ "displayText").apply(0))
   }
			 
   val section = <section>
		 		  <sectionNumber>{sectionNumber}</sectionNumber>
		 		  <title>{title}</title>
		 		  <editText>{editText}</editText>
		 		  <displayText>{displayText}</displayText>
			 	</section>
		
   section :: sections(tail)
  }
 }
 
 def saveSectionSetup(): JsCmd = {
  val newVision = <vision>{MyUtil.flattenNodeSeq(sections(SectionHeader.is))}</vision>
  Repository.write("scenario", SelectedScenario.is.id, "vision", "vision", -1, newVision)
  Vision((Repository.read("scenario", SelectedScenario.is.id, "vision", "vision", -1) \\ "vision").apply(0))
  Noop
 }
 
 def visionSetup (xhtml: NodeSeq): NodeSeq = {
  def down() = swapSections("down")
  def up() = swapSections("up")
  
  bind("sections", xhtml, "rows"  -> showAllSectionHeader(),
	   "add" -> ajaxButton("+", addSection _) % ("class" -> "standardButton"),
	   "remove" -> ajaxButton("-", removeSection _) % ("class" -> "standardButton"),
	   "up" -> ajaxButton(S.?("up"), up _) % ("class" -> "standardButton"),
	   "down" -> ajaxButton(S.?("down"), down _) % ("class" -> "standardButton"),
	   "save" -> ajaxButton(S.?("save"), saveSectionSetup _) % ("class" -> "standardButton"))
 }
 
 /**
  * The following methods are used on the vision page itself
  * 
  */
 
 def finishText(text: String): JsCmd = {
  SectionText(text)
  Noop
 }
 
 def transformSection(section: Node): Node = {
  
  val sectionNumber = SectionHeaderEdit.is._1	
  val sectionTitle = SectionHeaderEdit.is._2
  val numberOfSection = MyUtil.getNodeText((section \\ "sectionNumber").apply(0))
	 
  if(numberOfSection == sectionNumber) {
	  
	  <section>
	  	<sectionNumber>{sectionNumber.toString}</sectionNumber>
		<title>{sectionTitle.toString}</title>
		<editText>{SectionText.is}</editText>
		<displayText>{WikiParser.transformVisionText(SectionText.is)}</displayText>
	  </section>
  }
  else section
 }
 
 def saveSection() : JsCmd = {
  val newVision = <vision>{(Vision.is \\ "section").map(transformSection)}</vision>
  Repository.write("scenario", SelectedScenario.is.id, "vision", "vision", -1, newVision)
  Vision((Repository.read("scenario", SelectedScenario.is.id, "vision", "vision", -1) \\ "vision").apply(0))
  RedirectTo("/vision")
 }
 
 def cancelEdit(sectionNumber: String) : JsCmd = {
  val command = "$('#editContainer" + sectionNumber + "').fadeOut(function(){$('#displayContainer" + sectionNumber + "').fadeIn()});"
  JsRaw(command)
 }
 
 def edit(sectionNumber: String, sectionTitle: String) : JsCmd = {
  SectionHeaderEdit((sectionNumber, sectionTitle))
  val command = "$('#displayContainer" + sectionNumber + "').fadeOut(function() { $('#editContainer" + sectionNumber + "').fadeIn();})"
  JsRaw(command)
 }

 def createSection(section: Node): NodeSeq = {
  
  val sectionNumber = MyUtil.getNodeText((section \\ "sectionNumber").apply(0))
  val sectionTitle = MyUtil.getNodeText((section \\ "title").apply(0))
  val editIt = ajaxButton(S.?("edit"), () => edit(sectionNumber, sectionTitle)) % ("class" -> "standardButton") % ("style" -> "float:right")
  val cancelIt = ajaxButton(S.?("cancel"), () => cancelEdit(sectionNumber)) % ("class" -> "standardButton") % ("style" -> "float:right")
  val saveIt = ajaxButton(S.?("save"), saveSection _) % ("class" -> "standardButton") % ("style" -> "float:right")
  val textArea = ajaxTextarea(MyUtil.getNodeText((section \\ "editText").apply(0)), text => finishText(text)) % ("rows" -> "15") % ("cols" -> "130") % ("class" -> "wikiEditor")
  
  <div class="message" id ={"displayContainer" + sectionNumber}>
    <p class="messageTitle">{MyUtil.getNodeText((section \\ "title").apply(0))}{editIt}</p>
    {MyUtil.tagIt(MyUtil.getSeqHeadText(section \\ "displayText"))}
   </div>
   <div class="message editContainer" id ={"editContainer" + sectionNumber}>
    <p class="messageTitle">{MyUtil.getNodeText((section \\ "title").apply(0))}{cancelIt}{saveIt}</p><br />
    {textArea}
   </div>
 }

 def showVisionText (xhtml: NodeSeq) = {
  bind("vision", xhtml, "text"  -> MyUtil.flattenNodeSeq((Vision.is \\ "section").map(createSection).toList))
 }
 
 /**
  * configuration of design guidelines
  */
 
 def updateDesignConfiguration(tagName: String, text: String, id: String): JsCmd = {
  val tag = XML.loadString("<" + tagName + " id='" + tagName + "'>" + text + "</" + tagName + ">") \\ tagName
  val transform = id #> tag
  val transformedSetup = transform(Setup.is).apply(0)
  Repository.write("scenario", SelectedScenario.is.id, "setup","setup", -1, transformedSetup)
  Setup(transformedSetup)
  Noop
 }
 
 def design (xhtml: NodeSeq): NodeSeq = {
  
  val setup = Repository.read("scenario", SelectedScenario.is.id, "setup","setup", -1) \\ "setup"
  def chooseSnowflakeVsStar(option: String) = updateDesignConfiguration("snowflake", option, "snowflake")
  def chooseAccountModel(option: String) = updateDesignConfiguration("accountModel", option, "accountModel")
  val snowflakeChoice = MyUtil.getSeqHeadText(setup \\ "snowflake")
  val accountChoice = MyUtil.getSeqHeadText(setup \\ "accountModel")
  
  def viewOption(selection: Boolean, kind: String):JsCmd = {
   if(selection) updateDesignConfiguration(kind, "Y", kind)
   else updateDesignConfiguration(kind, "N", kind)
  }
  
  def extraViewOption(selection: Boolean) = viewOption(selection, "extraViewLayer")
  def stableViewOption(selection: Boolean) = viewOption(selection, "stableViewLayer")
  def isChoice(tagName: String) = if(MyUtil.getSeqHeadText(setup \\ tagName) == "Y") true else false
  val extraViewChoice = isChoice("extraViewLayer")
  val stableViewChoice = isChoice("stableViewLayer")
  def editTFact(prefix: String) = updateDesignConfiguration("prefixTFact", prefix, "prefixTFact")
  def editSFact(prefix: String) = updateDesignConfiguration("prefixSFact", prefix, "prefixSFact")
  def editDim(prefix: String) = updateDesignConfiguration("prefixDim", prefix, "prefixDim")
  val prefixTFact = MyUtil.getSeqHeadText(setup \\ "prefixTFact")
  val prefixSFact = MyUtil.getSeqHeadText(setup \\ "prefixSFact")
  val prefixDim = MyUtil.getSeqHeadText(setup \\ "prefixDim")
  
  val nameDimMeasure = MyUtil.getSeqHeadText(setup \\ "nameDimMeasure")
  val namePrimaryKey = MyUtil.getSeqHeadText(setup \\ "namePrimaryKey")
  val nameReferences = MyUtil.getSeqHeadText(setup \\ "nameReferences")
  val defaultKeyType = MyUtil.getSeqHeadText(setup \\ "defaultKeyType")
  val defaultMeasureType = MyUtil.getSeqHeadText(setup \\ "defaultMeasureType")
  val defaultAttributeType = MyUtil.getSeqHeadText(setup \\ "defaultAttributeType")
  
  def editDimMeasure(name: String) = updateDesignConfiguration("nameDimMeasure", name, "nameDimMeasure")
  def editPrimaryKey(name: String) = updateDesignConfiguration("namePrimaryKey", name, "namePrimaryKey")
  def editReferences(name: String) = updateDesignConfiguration("nameReferences", name, "nameReferences")
  def editKeyType(name: String) = updateDesignConfiguration("defaultKeyType", name, "defaultKeyType")
  def editMeasureType(name: String) = updateDesignConfiguration("defaultMeasureType", name, "defaultMeasureType")
  def editAttributeType(name: String) = updateDesignConfiguration("defaultAttributeType", name, "defaultAttributeType")
  
  val tableTypes = List(("noTable", S.?("noTable")), ("allTable", S.?("allTable")), 
		           ("dimensionTable", S.?("dimensionTable")), ("factTable", S.?("factTable")), ("accountFactTable", S.?("accountFactTable")))
  
  def tableTypeSelection(tableType: String, attrId: String) = SHtml.ajaxSelect(tableTypes, Box(tableType), v => updateDesignConfiguration(attrId, v, attrId))
  def addAttr(text: String, attrId: String) = SHtml.ajaxText(text, v => updateDesignConfiguration(attrId, v, attrId))
  
  def additionalAttributes() = {
	  val timestampETLTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "timestampETLTableType")
	  val identifierETLTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "identifierETLTableType")
	  val consecutiveNumberTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "consecutiveNumberTableType")
	  val validFromTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "validFromTableType")
	  val validUntilTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "validUntilTableType")
	  val isActualTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "isActualTableType")
	  val asIsTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "asIsTableType")
	  val toBeTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "toBeTableType")
	  val forecastTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "forecastTableType")
	  val planTableTypeChoice = MyUtil.getSeqHeadText(setup \\ "planTableType")
	  
	  val timestampETLName = MyUtil.getSeqHeadText(setup \\ "timestampETLName")
	  val identifierETLName = MyUtil.getSeqHeadText(setup \\ "identifierETLName")
	  val consecutiveNumberName = MyUtil.getSeqHeadText(setup \\ "consecutiveNumberName")
	  val validFromName = MyUtil.getSeqHeadText(setup \\ "validFromName")
	  val validUntilName = MyUtil.getSeqHeadText(setup \\ "validUntilName")
	  val isActualName = MyUtil.getSeqHeadText(setup \\ "isActualName")
	  val asIsName = MyUtil.getSeqHeadText(setup \\ "asIsName")
	  val toBeName = MyUtil.getSeqHeadText(setup \\ "toBeName")
	  val forecastName = MyUtil.getSeqHeadText(setup \\ "forecastName")
	  val planName = MyUtil.getSeqHeadText(setup \\ "planName")
	  
	  val timestampETLType = MyUtil.getSeqHeadText(setup \\ "timestampETLType")
	  val identifierETLType = MyUtil.getSeqHeadText(setup \\ "identifierETLType")
	  val consecutiveNumberType = MyUtil.getSeqHeadText(setup \\ "consecutiveNumberType")
	  val validFromType = MyUtil.getSeqHeadText(setup \\ "validFromType")
	  val validUntilType = MyUtil.getSeqHeadText(setup \\ "validUntilType")
	  val isActualType = MyUtil.getSeqHeadText(setup \\ "isActualType")
	  val asIsType = MyUtil.getSeqHeadText(setup \\ "asIsType")
	  val toBeType = MyUtil.getSeqHeadText(setup \\ "toBeType")
	  val forecastType = MyUtil.getSeqHeadText(setup \\ "forecastType")
	  val planType = MyUtil.getSeqHeadText(setup \\ "planType")
	  
	  
	  <table class='protocolTable'>
		  <col width="40%" />
		  <col width="20%" />
		  <col width="20%" />
		  <col width="20%" />
	  	  <thead>
	  		<tr><td></td><td>{S.?("usage")}</td><td>{S.?("attributeName")}</td><td>{S.?("dataType")}</td></tr>
	  	  </thead>
	  	  <tbody>
		  	<tr><td>{S.?("etlTimestampDescription")}</td><td>{tableTypeSelection(timestampETLTableTypeChoice, "timestampETLTableType")}</td><td>{addAttr(timestampETLName, "timestampETLName")}</td><td>{addAttr(timestampETLType, "timestampETLType")}</td></tr>
		  	<tr><td>{S.?("etlIdentifierDescription")}</td><td>{tableTypeSelection(identifierETLTableTypeChoice, "identifierETLTableType")}</td><td>{addAttr(identifierETLName, "identifierETLName")}</td><td>{addAttr(identifierETLType, "identifierETLType")}</td></tr>
		  	<tr><td>{S.?("consecutiveNumberDescription")}</td><td>{tableTypeSelection(consecutiveNumberTableTypeChoice, "consecutiveNumberTableType")}</td><td>{addAttr(consecutiveNumberName, "consecutiveNumberName")}</td><td>{addAttr(consecutiveNumberType, "consecutiveNumberType")}</td></tr>
		  	<tr><td>{S.?("validFromDescription")}</td><td>{tableTypeSelection(validFromTableTypeChoice, "validFromTableType")}</td><td>{addAttr(validFromName, "validFromName")}</td><td>{addAttr(validFromType, "validFromType")}</td></tr>
		  	<tr><td>{S.?("validFromDescription")}</td><td>{tableTypeSelection(validUntilTableTypeChoice, "validUntilTableType")}</td><td>{addAttr(validUntilName, "validUntilName")}</td><td>{addAttr(validUntilType, "validUntilType")}</td></tr>
		  	<tr><td>{S.?("isActualFlagDescription")}</td><td>{tableTypeSelection(isActualTableTypeChoice, "isActualTableType")}</td><td>{addAttr(isActualName, "isActualName")}</td><td>{addAttr(isActualType, "isActualType")}</td></tr>
		  	<tr><td>{S.?("asIsValueDescription")}</td><td>{tableTypeSelection(asIsTableTypeChoice, "asIsTableType")}</td><td>{addAttr(asIsName, "asIsName")}</td><td>{addAttr(asIsType, "asIsType")}</td></tr>
		  	<tr><td>{S.?("toBeValueDescription")}</td><td>{tableTypeSelection(toBeTableTypeChoice, "toBeTableType")}</td><td>{addAttr(toBeName, "toBeName")}</td><td>{addAttr(toBeType, "toBeType")}</td></tr>
		  	<tr><td>{S.?("forecastValueDescription")}</td><td>{tableTypeSelection(forecastTableTypeChoice, "forecastTableType")}</td><td>{addAttr(forecastName, "forecastName")}</td><td>{addAttr(forecastType, "forecastType")}</td></tr>
		  	<tr><td>{S.?("planValueDescription")}</td><td>{tableTypeSelection(planTableTypeChoice, "planTableType")}</td><td>{addAttr(planName, "planName")}</td><td>{addAttr(planType, "planType")}</td></tr>
		  </tbody>
	 </table>
  }
  
  def makeRadio(checked: Boolean, value: String, name: String) = {
   val action = if(name == "snowflake") SHtml.ajaxCall(JsRaw("$(this).val()"), chooseSnowflakeVsStar _)._2 else SHtml.ajaxCall(JsRaw("$(this).val()"), chooseAccountModel _)._2
   if(checked) <input type="radio" /> % ("onclick" -> action) % new UnprefixedAttribute("name", name, Null) % new UnprefixedAttribute("value", value, Null) % new UnprefixedAttribute("checked", "checked", Null)
   else <input type="radio" /> % ("onclick" -> action) % new UnprefixedAttribute("name", name, Null) % new UnprefixedAttribute("value", value, Null)
  }
  
  def snowflake() = {
   if(snowflakeChoice == "star") <span>{makeRadio(true, "star", "snowflake")} {S.?("starOption")}<br />{makeRadio(false, "snowflake", "snowflake")} {S.?("snowflakeOption")}</span>
   else <span>{makeRadio(false, "star", "snowflake")} {S.?("starOption")}<br />{makeRadio(true, "snowflake", "snowflake")} {S.?("snowflakeOption")}</span>
  }
  
  def accountModel() = {
   if(accountChoice == "account") <span>{makeRadio(true, "account", "account")} {S.?("accountModel")}<br />{makeRadio(false, "noAccount", "account")} {S.?("noAccountModel")}</span>
   else <span>{makeRadio(false, "account", "account")} {S.?("accountModel")}<br />{makeRadio(true, "noAccount", "account")} {S.?("noAccountModel")}</span>
  }
  
  bind("design", xhtml, 
	   "snowflake" -> snowflake(),
	   "accountModel" -> accountModel(),
	   "extraViewLayer" -> SHtml.ajaxCheckbox(extraViewChoice, extraViewOption _),
	   "stableViewLayer" -> SHtml.ajaxCheckbox(stableViewChoice, stableViewOption _),
	   "prefixTFact" -> ajaxText(prefixTFact, text => editTFact(text)),
	   "prefixSFact" -> ajaxText(prefixSFact, text => editSFact(text)),
	   "prefixDim" -> ajaxText(prefixDim, text => editDim(text)),
	   "nameDimMeasure" -> ajaxText(nameDimMeasure, text => editDimMeasure(text)),
	   "namePrimaryKey" -> ajaxText(namePrimaryKey, text => editPrimaryKey(text)),
	   "nameReferences" -> ajaxText(nameReferences, text => editReferences(text)),
	   "defaultKeyType" -> ajaxText(defaultKeyType, text => editKeyType(text)),
	   "defaultMeasureType" -> ajaxText(defaultMeasureType, text => editMeasureType(text)),
	   "defaultAttributeType" -> ajaxText(defaultAttributeType, text => editAttributeType(text)),
	   "additionalAttributes" -> additionalAttributes())
 }
}