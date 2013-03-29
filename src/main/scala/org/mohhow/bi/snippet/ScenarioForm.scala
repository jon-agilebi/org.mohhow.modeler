package org.mohhow.snippet

import net.liftweb._
import http._
import util._

import mapper._
import util._
import Helpers._

import org.mohhow.model._
import java.util.Date
import org.mohhow.bi.lib.Repository
import org.mohhow.bi.util.{Utility => MyUtil}

object ScenarioForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
 
 def clientShortName() = {
  var result = ""
  if(ScenarioToEdit.is != null) {
	 val clients = Client.findAll(By(Client.id, ScenarioToEdit.is.fkClient))
	 if(!clients.isEmpty) result = clients(0).shortName
  }
  
  result
 }
 
 def isAllowedToCreateScenario(user: User, cl: Client) = {
  val link = ClientToUser.findAll(By(ClientToUser.fkClient, cl.id), By(ClientToUser.fkUser, user.id))
  !link.isEmpty && link.apply(0).canBeOwner.toDouble == 1
 }
 
 def clientList(): List[String] = {
  val user = User.currentUser openOr null
  if(user != null) user.clients.all.filter(cl => isAllowedToCreateScenario(user, cl)).map(cl => cl.shortName.toString).toList
  else Nil
 }
	
 val client = select(S.?("client"), clientShortName, clientList)
 val scenarioName = field(S.?("name"), nvl(ScenarioToEdit.is.name.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")), uniqueScenarioName _)
 val scenarioDescription = textarea(S.?("description"), nvl(ScenarioToEdit.is.description.toString), valMaxLen(1000, S.?("textToLong")))
 val scenarioPrefix = field(S.?("prefix"), nvl(ScenarioToEdit.is.prefix.toString), valMinLen(1, S.?("textToShort")))
 val scenarioUrl = field(S.?("url"), nvl(ScenarioToEdit.is.url.toString), valMinLen(1, S.?("textToShort")))
 
 def uniqueScenarioName(scenarioName: String): List[FieldError] = {
  val user = User.currentUser openOr null
  if(user != null) {
	 val scenarios = Scenario.findAll(ByList(Scenario.fkClient, user.clients.all.map(_.id.toLong).toList))
	 if(scenarios.exists(sc => sc.name == scenarioName && sc.id != ScenarioToEdit.is.id)) S.?("scenarioAlreadyExists") else Nil 
  }
  else Nil
 }
 
 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def finish() {
  val scenario = ScenarioToEdit.is
  val scenarioClient = Client.findAll(By(Client.shortName, client.toString)).apply(0)
 
  scenario.fkClient(scenarioClient).name(scenarioName).description(scenarioDescription).prefix(scenarioPrefix).url(scenarioUrl).owner(User.currentUser).save
	
  if(SelectedScenario.is == null || SelectedScenario.is.id != scenario.id)
  {
	  // We are in the situation that we add a new scenario, hence we also have to create a new backlog and
	  // we also have to add the current user as scenario owner to the role list; furthermore we create a repository
	  // folder for the new scenario
	  
	  // create repository
	  
	  try {
		  Repository.createScenario(scenario.id)
		  
		  // read the default information
		  Setup((Repository.read("scenario", scenario.id, "setup","setup", -1) \\ "setup").apply(0))
		  Vision((Repository.read("scenario", scenario.id, "vision", "vision", -1) \\ "vision").apply(0))
		  Blocks((Repository.read("scenario", scenario.id, "blocks", "blocks", -1) \\ "block"))
		  
		  ScenarioOwner(User.currentUser openOr null)
		  val ownership = ScenarioRole.create
		  ownership.fkUser(User.currentUser openOr null).fkScenario(scenario).role("owner").dateCreated(new Date).save
		  
		  Analysts(Nil)
		  Designer(Nil)
		  ReleaseManager(Nil)
		  
		  val backlog = ProductBacklog.create
		  backlog.fkScenario(scenario).description("Backlog for scenario " + scenario.name).save
		  ChosenBacklog(backlog)
		  RelevantFeature(null)
		  ChosenFeature(null)
		  
		  val usage = Map() ++ (Setup.is \\ "usage" \\ "part").map(node => MyUtil.getSeqHeadText(node) -> ((node \ "@useIt").text == "Y", (node \ "@relation").text, (node \ "@scenarioId").text.toLong)).toList
		  Usage(usage)
		  UsageEdit(usage)
	  }
	  catch {
	 	 case ex: Exception => {
	 		 scenario.delete_!
	 		 S.warning("scenarioCreationFailed", S.?("scenarioCreationFailed"))
	 	 }
	  }
  }
	  SelectedScenario(scenario)  
      S.redirectTo("/index")
  }
}