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

object FeatureForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
 val priorityPattern = Pattern.compile("\\d{1,3}+")	
 
 val featureName = field("Name", nvl(RelevantFeature.is.name.toString), valMinLen(1, "Feature name too short"), valMaxLen(100, "Feature name too long"))
 val featureType = select("Type", nvl(RelevantFeature.is.featureType.toString), List("User Story", "Business Question", "Compliance Request", "Constraint", "None of the above"))
 val featureDescription = textarea("Description", nvl(RelevantFeature.is.description.toString), valMaxLen(1000, "Description too long"))
 val featurePriority = field("Priority", nvl(RelevantFeature.is.priority.toString), valRegex(priorityPattern, "Priority must be a number less or equal to 999"))
 val featureStoryPoints = select("Story Points", nvl(RelevantFeature.is.featureType.toString), List("1", "3", "5", "8", "13", "21", "Gogol"))
 
 def finish() {
	 asLong(featurePriority.is) match {
		 case Full(pr) => {
			 val feature = RelevantFeature.is
			 feature.name(featureName).featureType(featureType).description(featureDescription).storyPoints(featureStoryPoints).priority(pr).save
			 saveLinksToProtocolItems()
			 
			 ChosenFeature(feature)
			 S.redirectTo("/backlog") 
		 }
		 
		 case _ => S.error("prio not correct");
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
}
