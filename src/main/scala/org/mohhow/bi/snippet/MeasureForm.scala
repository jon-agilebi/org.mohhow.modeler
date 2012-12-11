package org.mohhow.snippet

import net.liftweb._
import http._
import mapper._
import util._
import util.Helpers._
import org.mohhow.model._
import common._
import java.util.Date

import scala.xml._
import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib.WikiParser 
import java.util.regex.Pattern

object MeasureForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
 def checkTerm(term: String): List[FieldError] = if(term != null && term != "" && WikiParser.checkTerm(term)._1)  WikiParser.checkTerm(term)._2 else Nil
 
 val upToThreeDigits = Pattern.compile("\\d{1,3}+")	
 
 val timeUnits = List("seconds", "tMinutes", "hours", "workingDays", "calendarDays", "weeks", "months", "nthDayOfNextMonth", "nthWorkingDayOfNextMonth", "years")
 val timeUnitPresentation = List(S.?("seconds"), S.?("tMinutes"), S.?("hours"), S.?("workingDays"), S.?("calendarDays"), S.?("weeks"), S.?("months"), S.?("nthDayOfNextMonth"), S.?("nthWorkingDayOfNextMonth"), S.?("years"))
 
 def findTimeUnit(presentation: String) = {
  def f(txt: String, firstList: List[String], secondList:List[String]): String = firstList match {
	  case Nil => ""
	  case head :: tail => if(txt == head) secondList.head else f(txt, tail, secondList.tail)
  }
  
  f(presentation, timeUnitPresentation, timeUnits)	 
 }
 
 val subject = select(S.?("subject"), nvl(SelectedMeasure.is.subject), subjectList())
 val shortName = field(S.?("shortName"), nvl(SelectedMeasure.is.shortName), valMinLen(1, S.?("textToShort")), valMaxLen(50, S.?("textToLong")))
 val longName = field(S.?("longName"), nvl(SelectedMeasure.is.longName), valMinLen(1, S.?("textToShort")), valMaxLen(200, S.?("textToLong")))
 val unit = select(S.?("unit"), nvl(SelectedMeasure.is.unit), getUnits())
 val definition = textarea(S.?("definition"), nvl(SelectedMeasure.is.definition), valMaxLen(1000, S.?("textToLong")))
 val formula = textarea(S.?("formula"), MyUtil.prettyTerm(nvl(SelectedMeasure.is.formula.toString), false), checkTerm _, "class" -> "termEditor")
 val mockup = textarea(S.?("mockup"), MyUtil.prettyTerm(nvl(SelectedMeasure.is.mockup.toString), false), checkTerm _, "class" -> "termEditor")
 val requiredActuality = field(S.?("requiredActuality"), nvl(SelectedMeasure.is.requiredActualityValue.toString),valRegex(upToThreeDigits, S.?("threeDigitsNotInRange")))
 val requiredActualityUnit = select(S.?("unit"), nvl(S.?(SelectedMeasure.is.requiredActualityUnit)), timeUnitPresentation)
 val interest = field(S.?("timespanOfInterest"), nvl(SelectedMeasure.is.timespanOfInterestValue.toString),valRegex(upToThreeDigits, S.?("threeDigitsNotInRange")))
 val interestUnit = select(S.?("unit"), nvl(S.?(SelectedMeasure.is.timespanOfInterestUnit)), timeUnitPresentation)
 val storage = field(S.?("requiredStorageTime"), nvl(SelectedMeasure.is.requiredStorageValue.toString),valRegex(upToThreeDigits, S.?("threeDigitsNotInRange")))
 val storageUnit = select("unit", nvl(S.?(SelectedMeasure.is.requiredStorageUnit)), timeUnitPresentation)
 
 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def finish() { 
  val measure = SelectedMeasure.is
  
  asLong(requiredActuality.is) match {
	
  	case Full(ra) => {
  		
  		asLong(interest.is) match {
  			
  			case Full(intrst) => {
  				
  				asLong(storage.is) match {
  					case Full(sto) => {
  						measure.subject(subject).shortName(shortName).longName(longName).status("approved").definition(definition).mockup(WikiParser.makeReplacements(mockup)._1).formula(WikiParser.makeReplacements(formula)._1).unit(unit)
  						measure.requiredActualityValue(ra).requiredActualityUnit(findTimeUnit(requiredActualityUnit))
  						measure.timespanOfInterestValue(intrst).timespanOfInterestUnit(findTimeUnit(interestUnit))
  						measure.requiredStorageValue(sto).requiredStorageUnit(findTimeUnit(storageUnit)).save
  						saveContext(measure)
  						saveRange(measure)
  						S.redirectTo("/catalogue") 
  					}
  					
  					case _ => S.error(S.?("threeDigitsNotInRange"))
  				}	
  			}
  			
  			case _ => S.error(S.?("threeDigitsNotInRange"))
  		}
    }
  
  	case _ => S.error(S.?("threeDigitsNotInRange"))
  } 
 }
 
 def getUnits() = {
  def getText(tag: String, n: Node) = MyUtil.getNodeText((n \\ tag).apply(0))
  val units = (Setup.is \\ "unit").map(n => (getText("subject", n), getText("name", n), getText("symbol", n))).toList
  units.map(_._2)	 
 }
 
 def subjectList(): List[String] = {
  def subjects(node: Node): List[String] = WikiParser.findTaggedText(MyUtil.getNodeText(node), "subject")
  val fromVision = (Vision.is  \\ "editText").flatMap(subjects).toList
  val fromCatalogue = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is)).map(m => m.subject).toList
  (fromVision ::: fromCatalogue).map(_.toString).distinct
 }

 def saveContext(measure: Measure) = {
  def createContext(item: (Long, String), m: Measure) = {
   val m2mv = MeasureToModelVertex.create
   m2mv.fkMeasure(m).fkLevel(item._1).aggregation(item._2).dateCreated(new Date).save
  }
  
  def aggr(levelId: Long) = Context.is.filter(_._1 == levelId).apply(0)._2
  
  val existingContext = MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkMeasure, measure.id))
  val modelLevel = Set() ++ Context.is.map(_._1)
  val existingLevel = Set() ++ existingContext.map(_.fkLevel.toLong)
  // delete existing context elements which are not in the model
  existingContext.filter(m2m => !modelLevel.contains(m2m.fkLevel)).map(m2m => m2m.delete_!)
  
  // create new context elements
  Context.is.filter(c => !existingLevel.contains(c._1)).map(c => createContext(c, measure))
  
  // update context elements
  existingContext.filter(m2m => modelLevel.contains(m2m.fkLevel)).map(m2m => m2m.aggregation(aggr(m2m.fkLevel)).save)
  
 }

 def saveRange(measure: Measure) = {
  def optN(d: Option[Double]): Double = d match {case Some(v) => v; case None => 0}
  def createRange(r: (Option[Double], Option[Double], Option[Double], String)) = {
   val mr = MeasureRange.create
   mr.fkMeasure(measure).lowerBound(optN(r._1)).upperBound(optN(r._2)).rangeValue(optN(r._3)).meaning(r._4).save
  }
  
  MeasureRange.findAll(By(MeasureRange.fkMeasure, measure.id)).map(m => m.delete_!)
  Ranges.is.map(r => createRange(r))
 }
}