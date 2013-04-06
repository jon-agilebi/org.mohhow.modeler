package org.mohhow.snippet

import net.liftweb._
import http._
import util._
import util.Helpers._
import common._
import mapper._
import org.mohhow.model._
import java.util.Date
import java.util.regex.Pattern
import org.mohhow.bi.lib.WikiParser
import org.mohhow.bi.util.{Utility => MyUtil}

object FeatureForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
 val priorityPattern = Pattern.compile("\\d{1,3}+")	
 
 def parseDescription(question: String): List[FieldError] = {
  if(featureType == S.?("businessQuestion") || featureType == S.?("complianceRequest")) {
   val result = WikiParser.parseBusinessQuestion(question)
   if(result._1) result._2 else Nil
  } 
  else Nil
 }
 
 val featureTypes = List(S.?("businessQuestion"), S.?("complianceRequest"), S.?("userStory"), S.?("constraint"), S.?("noneOfTheAbove"))
 
 val featureName = field(S.?("name"), nvl(RelevantFeature.is.name.toString), valMinLen(1, S.?("textToShort")), valMaxLen(100, S.?("textToLong")))
 val featureType = select(S.?("type"), nvl(RelevantFeature.is.featureType.toString),  featureTypes)
 val featureDescription = textarea(S.?("description"), nvl(RelevantFeature.is.description.toString), valMaxLen(1000, S.?("textToLong")), parseDescription _)
 val featurePriority = field(S.?("priority"), nvl(RelevantFeature.is.priority.toString), valRegex(priorityPattern, S.?("priorityNotInRange"))) 
 val featureStoryPoints = select(S.?("storyPoints"), nvl(RelevantFeature.is.storyPoints.toString), List("1", "2", "3", "5", "8", "13", "21", "Gogol"))
 
 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def finish() {
	 asLong(featurePriority.is) match {
		 case Full(pr) => {
			 val feature = RelevantFeature.is
			 feature.name(featureName).featureType(featureType).description(featureDescription).storyPoints(featureStoryPoints).priority(pr).save
			 saveLinksToProtocolItems()
			 
			 if(featureType.toString == S.?("businessQuestion") || featureType.toString == S.?("complianceRequest")) processQuestion(feature)
			 
			 ChosenFeature(feature)
			 S.redirectTo("/backlog") 
		 }
		 
		 case _ => S.error( S.?("priorityNotInRange"));
	 } 
 }
 
 def saveLinksToProtocolItems() = {
  
  val itemsToBe = ItemsOfFeature.is.toList
  val itemsAsIs = ProtocolToBacklog.findAll(By(ProtocolToBacklog.fkFeature, RelevantFeature.is.id)).map(link => link.fkProtocolItem).toList
  
  for(newId <- itemsToBe.diff(itemsAsIs)) {
	  val newLink = ProtocolToBacklog.create
	  newLink.fkProtocolItem(newId).fkFeature(RelevantFeature.is.id).dateCreated( new Date).save
  }
  
  for(oldId <- itemsAsIs.diff(itemsToBe)) {
	  val oldLink = ProtocolToBacklog.findAll(By(ProtocolToBacklog.fkProtocolItem,oldId), By(ProtocolToBacklog.fkFeature, RelevantFeature.is.id)).apply(0)
	  oldLink.delete_!
  }
  
 }
 
 def processQuestion(feature: Feature) = {
  
  def processMeasure(measureName: String) = {
	val msrs = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is), By(Measure.shortName, measureName))
	
	if(msrs.isEmpty) {
	 val m = Measure.create
	 m.fkScenario(SelectedScenario.is).shortName(measureName).fkFeature(feature).status("candidate").dateCreated(new Date).save

	}
  }
  
  def processRole(roleName: String): UserRole = {
   val roles = UserRole.findAll(By(UserRole.roleName, roleName))
   
   if(roles.isEmpty) {
	 val newRole = UserRole.create
	 newRole.roleName(roleName).dateCreated(new Date).save
	 newRole
   }
   else roles(0)
  }
  
  val content = WikiParser.contentOfQuestion(feature.description.toString)
  val role = processRole(content._1)
  
  if(IsNewFeature.is) {
	  val newSpec = Specification.create
	  newSpec.fkScenario(SelectedScenario.is).fkFeature(feature).name(feature.name).description(feature.description).status("candidate").dateCreated(new Date).save
	  val newLink = SpecificationToRole.create
	  newLink.fkSpecification(newSpec).fkRole(role).dateCreated(new Date).save
	  MyUtil.createFrame(newSpec.id)
  }
  
  val questions = content._2 
  
  for(question <- questions) {

	 for(candidate <- question._2) if(candidate._1 == "measure") processMeasure(candidate._2) 
  }
 }
 
}