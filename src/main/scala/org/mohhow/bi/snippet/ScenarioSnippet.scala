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
import org.mohhow.bi.util.{XMLUIBuilder => MyBuilder}
import java.util.Date

object ScenarioToEdit extends SessionVar[Scenario](null)
object SelectedScenario extends SessionVar[Scenario](null)

object Setup extends SessionVar[Node](null)
object Vision extends SessionVar[NodeSeq](null)
object SectionText extends RequestVar[String](null)
object SelectedSectionNumber extends SessionVar[Long](0)
object Units extends SessionVar[List[(Long, String, String, String)]](null)
object MaximalUnitId extends SessionVar[Long](0)
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
	
 def createScenario() : JsCmd = {
  val newScenario = Scenario.create
  newScenario.dateCreated(new Date)
  ScenarioToEdit(newScenario)
  RedirectTo("/scenarioEdit")
 }
 
 /**
  * open the edit page 
  */
 
 def editScenario() : JsCmd = {
  if(SelectedScenario.is != null){
	  ScenarioToEdit(SelectedScenario.is)
	  RedirectTo("/scenarioEdit")
  }
  else Alert("No scenario selected")
 }
 
 def selectedScenarioName(): String = {
  if(SelectedScenario.is != null) SelectedScenario.is.name
  else "No scenario selected"
 }
 
 /**
  * create one row for the scenario overview table. The selection of the checkbox in the first row determines the selected scenario for the  complete application.
  */
 
 def scenarioRow(scenario: Scenario) = { 
  val scenarioSelection = SelectedScenario.is != null && SelectedScenario.is.id == scenario.id
  <tr><td>{SHtml.ajaxCheckbox(scenarioSelection, selected => chooseScenario (scenario.id, selected))}</td><td>{scenario.name}</td><td>{scenario.owner}</td><td>{org.mohhow.bi.util.Utility.formatDate(scenario.dateCreated)}</td></tr>
 }
 
 def chooseScenario(scenarioId: Long, selected: Boolean): JsCmd = {
  if(selected){
	  SelectedScenario(Scenario.findAll(By(Scenario.id, scenarioId.toLong)).apply(0))
	  val backlogs = ProductBacklog.findAll(By(ProductBacklog.fkScenario, scenarioId));
	  if (!backlogs.isEmpty) ChosenBacklog(backlogs.apply(0))
	  try {
		  Setup((Repository.read("scenario", scenarioId, "setup","setup", -1) \\ "setup").map(Utility.trim).apply(0))
		  Vision((Repository.read("scenario", scenarioId, "vision", "vision", -1) \\ "vision").apply(0))
	  } 
	  catch {
	 	  case ex: Exception => println("Something wrong")
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
  
  if(SelectedScenario.is != null){
	 val sprint = getLatestSprint()
	 
	 <div>
	  <h4>Description</h4>
	  {SelectedScenario.is.description}
	  <br /><br />
	  <h4>Namespace</h4>
	  Prefix: {SelectedScenario.is.prefix} <br />
	  URL: {SelectedScenario.is.url}
	   <br /><br />

	  <h4>Vision</h4>
      <br />
	  	{link("/vision", nothing , <span>Find here the scenario's vision</span>)}
	  <br />
	  <br />
 
	  <h4>Actual Sprint</h4>
      <br />
	  Sprint Objective: {getSprintObjective(sprint)} <br /><br />
	  Sprint Backlog: <br /><br /> {getSprintBacklog(sprint)}<br /><br />
      Sprint Schedule: {getSprintSchedule(sprint)}
	 </div>
  } 
  else <div />
 }
	
 def scenarios(xhtml: NodeSeq) = {
  bind("scenario", xhtml,
   	   "add"     -> ajaxButton("Add Scenario", createScenario _) % ("class" -> "standardButton"),
   	   "edit"    -> ajaxButton("Edit Scenario", editScenario _) % ("class" -> "standardButton"),
   	   "display" -> <span align="right">{selectedScenarioName}</span>,
   	   "allScenarios" -> Scenario.findAll().map(scenarioRow).toSeq,
   	   "selectedDescription" -> selectedDescription())
 }
 		
 /**
  * Scenario setup, units at first
  */
 
 def updateUnits(units: List[(Long, String, String, String)], unitId: Long, selectionKind: String, text : String): List[(Long, String, String, String)] = units match {
	 case List() => List()
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

 def createUnitModel(unit: Node) = unit match {
  case <unit><subject>{subject @ _*}</subject><name>{name @ _*}</name><symbol>{symbol @ _*}</symbol></unit> => {
	  val actualId = MaximalUnitId.is
	  MaximalUnitId(actualId + 1)
	  (actualId, subject.apply(0).toString, name.apply(0).toString, symbol.apply(0).toString)
  }
  
  case _ => null
 }
 
 def allUnits(): NodeSeq = {
  if(Units.is == null) Units((Setup.is \\ "unit").map(createUnitModel).toList)
  Units.is.map(showUnit).toSeq
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
  val actualId = MaximalUnitId.is
  MaximalUnitId(actualId + 1)
  Units(Units.is ::: List((actualId, "<subject>", "<name>", "<symbol>")))
  JsRaw("$('#unitTableRows').append('" + Utility.trim(showUnit((actualId, "subject", "name", "symbol"))).toString +"')")
 }
 
 def removeUnit: JsCmd = {
  val newUnits = updateUnits(Units.is, SelectedUnitId.is, "remove", "")
  Units(newUnits)
  JsRaw("$('#unitTableRows').remove('.emphasized')")
 }
 
 def saveUnits(): JsCmd = {
  def serializeModel(modelItem: (Long, String, String, String)) = <unit><subject>{modelItem._2}</subject><name>{modelItem._3}</name><symbol>{modelItem._4}</symbol></unit>
  val units = MyUtil.flattenNodeSeq(Units.is.map(serializeModel))
  val transform = "units" #> units
  val setup = Setup.is
  Setup(transform(setup).apply(0))
  println("xxx" + Setup.is.toString + "xxx")
  Repository.write("scenario", SelectedScenario.is.id, "setup","setup", -1, Setup.is) 
  Noop
 }
  
 def unitSetup (xhtml: NodeSeq): NodeSeq = {
  bind("unit", xhtml, "rows"  -> allUnits(),
	   "add" -> ajaxButton("+", addUnit _) % ("class" -> "standardButton"),
	   "remove" -> ajaxButton("-", removeUnit _) % ("class" -> "standardButton"),
	   "save" -> ajaxButton("Save", saveUnits _) % ("class" -> "standardButton"))
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
  def changeTextInList(l: List[(Long, String, Long, String)], sectionNumber: Long, text: String)= l match  {
  	case Nil => Nil
  	case (sn, st, snOld, status) :: tail => {
  		if(sn == sectionNumber)  (sn, text, snOld, status) :: tail else l
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
		 else l
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
  Vision(newVision)
  Repository.write("scenario", SelectedScenario.is.id, "vision", "vision", -1, newVision)
  Noop
 }
 
 def visionSetup (xhtml: NodeSeq): NodeSeq = {
  def down() = swapSections("down")
  def up() = swapSections("up")
  
  bind("sections", xhtml, "rows"  -> showAllSectionHeader(),
	   "add" -> ajaxButton("+", addSection _) % ("class" -> "standardButton"),
	   "remove" -> ajaxButton("-", removeSection _) % ("class" -> "standardButton"),
	   "up" -> ajaxButton("Up", up _) % ("class" -> "standardButton"),
	   "down" -> ajaxButton("Down", down _) % ("class" -> "standardButton"),
	   "save" -> ajaxButton("Save", saveSectionSetup _) % ("class" -> "standardButton"))
 }
 
 /**
  * The following methods are used on the vision page itself
  * 
  */
 
 def finishText(text: String): JsCmd = {
	 SectionText(text)
	 Noop
 }
 
 def handleVisionSemantic(elms: List[(String, String)]) = {
	 
 }
 
 def transformSection(section: Node): Node = {
  val sectionNumber = SectionHeaderEdit.is._1	
  val sectionTitle = SectionHeaderEdit.is._2
  val numberOfSection = MyUtil.getNodeText((section \\ "sectionNumber").apply(0))
	 
  if(numberOfSection == sectionNumber) {
	   
	  // side effect: create new elements from Wiki entries
	  
	  handleVisionSemantic(WikiParser.getElements(SectionText.is))
	  
		 <section>
		 	<sectionNumber>{sectionNumber.toString}</sectionNumber>
		 	<title>{sectionTitle.toString}</title>
		 	<editText>{SectionText.is}</editText>
		 	<displayText>{WikiParser.transformWikiText(SectionText.is)}</displayText>
		 </section>
  }
  else section
 }
 
 def saveSection() : JsCmd = {
  val newVision = <vision>{(Vision.is \\ "section").map(transformSection)}</vision>
  Vision(newVision)
  Repository.write("scenario", SelectedScenario.is.id, "vision", "vision", -1, newVision)
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
  val editIt = ajaxButton("Edit", () => edit(sectionNumber, sectionTitle)) % ("class" -> "standardButton") % ("style" -> "float:right")
  val cancelIt = ajaxButton("Cancel", () => cancelEdit(sectionNumber)) % ("class" -> "standardButton") % ("style" -> "float:right")
  val saveIt = ajaxButton("Save", saveSection _) % ("class" -> "standardButton") % ("style" -> "float:right")
  val textArea = ajaxTextarea(MyUtil.getNodeText((section \\ "editText").apply(0)), text => finishText(text)) % ("rows" -> "15") % ("cols" -> "130") % ("class" -> "wikiEditor")
  
  <div class="message" id ={"displayContainer" + sectionNumber}>
    <p class="messageTitle">{MyUtil.getNodeText((section \\ "title").apply(0))}{editIt}</p>
    {MyUtil.getNodeText((section \\ "displayText").apply(0))}
   </div>
   <div class="message editContainer" id ={"editContainer" + sectionNumber}>
    <p class="messageTitle">{MyUtil.getNodeText((section \\ "title").apply(0))}{cancelIt}{saveIt}</p><br />
    {textArea}
   </div>
 }

 def showVisionText (xhtml: NodeSeq) = {
  bind("vision", xhtml, "text"  -> MyUtil.flattenNodeSeq((Vision.is \\ "section").map(createSection).toList))
 }
 
 def design (xhtml: NodeSeq): NodeSeq = {
  def createModelConfiguration() = {
	  val modelNodes = Setup.is \\ "dataModel"
	  MyUtil.flattenNodeSeq(modelNodes.map(MyBuilder.createRadioGroup).toList)
  }
  
  bind("design", xhtml, 
	   "dataModel"  ->createModelConfiguration(),
	   "view" -> <nothing />,
	   "prefix" -> <nothing />)
 }
}