package org.mohhow.snippet

import net.liftweb._
import http._
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
 def checkTerm(term: String): List[FieldError] = if(WikiParser.checkTerm(term)._1)  WikiParser.checkTerm(term)._2 else Nil
 
 val upToThreeDigits = Pattern.compile("\\d{1,3}+")	
 val timeUnits = List("Seconds", "Minutes", "Hours", "Working Days", "Calendar Days", "Months", "nth Day of Next Month", "nth Working Day of Next Month", "Years")
 
 
 val measure = SelectedMeasure.is
	
 val shortName = field("Short Name", nvl(measure.shortName), valMinLen(1, "Short name too short"), valMaxLen(50, "Short name too long"))
 val longName = field("Long Name", nvl(measure.longName), valMinLen(1, "Long name cannot be empty"), valMaxLen(200, "Long name too long"))
 val unit = select("Unit", nvl(measure.unit), getUnits())
 val definition = textarea("Definition", nvl(measure.definition), valMaxLen(1000, "Definition too long"))
 val formula = textarea("Formula", nvl(measure.formula), checkTerm _)
 val mockup = textarea("Mockup Formula", nvl(measure.mockup), valMaxLen(1000, "Mockup formula too long"))
 val requiredActuality = field("Required Actuality", nvl(measure.requiredActualityValue.toString),valRegex(upToThreeDigits, "Number must be less or equal to 999"))
 val requiredActualityUnit = select("Unit of Measure for Required Actuality", nvl(measure.requiredActualityUnit), timeUnits)
 val interest = field("Timespan of Interest", nvl(measure.timespanOfInterestValue.toString),valRegex(upToThreeDigits, "Number must be less or equal to 999"))
 val interestUnit = select("Unit of Measure for Timespan of Interest", nvl(measure.timespanOfInterestUnit), timeUnits)
 val storage = field("Required Storage Time", nvl(measure.requiredStorageValue.toString),valRegex(upToThreeDigits, "Number must be less or equal to 999"))
 val storageUnit = select("Unit of Measure for Required Storage Time", nvl(measure.requiredStorageUnit), timeUnits)
 
 def finish() { 
  asLong(requiredActuality.is) match {
	
  	case Full(ra) => {  
	 //measure.shortName(shortName)  //.longName(longName).definition(definition).mockup(mockup).formula(formula).unit(unit).save
	  //measure.requiredActualityValue(ra).requiredActualityUnit(requiredActualityUnit).save
      //S.redirectTo("/catalogue")
	}
  	
  	case _ => S.error("Required Actuality not correct");
  } 
 }
 
 def getUnits() = {
  def getText(tag: String, n: Node) = MyUtil.getNodeText((n \\ tag).apply(0))
  val units = (Setup.is \\ "unit").map(n => (getText("subject", n), getText("name", n), getText("symbol", n))).toList
  units.map(_._2)	 
 }
}