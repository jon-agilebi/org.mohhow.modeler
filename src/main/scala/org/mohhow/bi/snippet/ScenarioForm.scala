package org.mohhow.snippet

import net.liftweb._
import http._
import util._
import org.mohhow.model._
import java.util.Date
import org.mohhow.bi.lib.Repository

object ScenarioForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
	
 val scenario = ScenarioToEdit.is
 val scenarioName = field("Name", nvl(scenario.name.toString), valMinLen(1, "Scenario name too short"), valMaxLen(100, "Scenario name too long"), uniqueScenarioName _)
 val scenarioDescription = textarea("Description", nvl(scenario.description.toString), valMaxLen(1000, "Description too long"))
 val scenarioPrefix = field("Namespace Prefix", nvl(scenario.prefix.toString), valMinLen(1, "Scenario prefix too short"))
 val scenarioUrl = field("Namespace Url", nvl(scenario.url.toString), valMinLen(1, "Scenario url too short"))
 
 def uniqueScenarioName(martName: String): List[FieldError] = {
  if(Scenario.findAll().exists(_.name == scenarioName)) "Scenario name must be unique" else Nil 
 }
 
 def finish() {
  scenario.name(scenarioName).description(scenarioDescription).prefix(scenarioPrefix).url(scenarioUrl).save
	
  if(SelectedScenario.is == null || SelectedScenario.is.id != scenario.id)
  {
	  // We are in the situation that we add a new mart, hence we also have to create a new backlog and
	  // we also have to add the current user as scenario owner to the role list; furthermore we create a repository
	  // folder for the new scenario
	  
	  // create repository
	  
	  Repository.createScenario(scenario.id)
	  
	  // read the default information
	  Setup((Repository.read("scenario", scenario.id, "setup","setup", -1) \\ "setup").apply(0))
	  Vision((Repository.read("scenario", scenario.id, "vision", "vision", -1) \\ "vision").apply(0))
	  
	  val backlog = ProductBacklog.create
	  backlog.fkScenario(scenario).description("Backlog for scenario " + scenario.name).save
	  ChosenBacklog(backlog)
  }
	  SelectedScenario(scenario)  
      S.redirectTo("/index")
  }
}
