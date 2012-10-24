package org.mohhow.snippet

import org.mohhow._
import model._
import org.mohhow.bi.util._ 
import net.liftweb._
import net.liftweb.common._
import http._
import SHtml._
import S._

import js._
import JsCmds._

import mapper._
import util._
import Helpers._

import scala.xml._
import JE.{JsRaw,Str}
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib.WikiParser
import org.mohhow.bi.lib.JsonUtility

object SelectedBacklogId extends SessionVar[Long](-1)
object SelectedMeasure extends SessionVar[Measure](null)
object SelectedMeasureRange extends SessionVar[MeasureRange](null)
object Context extends SessionVar[List[(Long, String)]](Nil)
object Ranges extends SessionVar[List[(Option[Double], Option[Double], Option[Double], String)]](Nil)
object SelectedStatus extends SessionVar[String]("all")

class MeasureSnippet {

 def addIt() : JsCmd = {
  val measure = Measure.create
  measure.dateCreated(new Date).fkScenario(SelectedScenario.is.id)
  SelectedMeasure(measure)
  RedirectTo("/measure")
 }
 
 def editIt() : JsCmd = if(SelectedMeasure.is != null) RedirectTo("/measure") else Alert(S.?("noMeasureSelection"))
  
 def discardIt() : JsCmd = {
  if(SelectedMeasure.is != null) {
   val m = SelectedMeasure.is
   m.dateDiscarded(new Date).status("deprecated").save
   SelectedMeasure(null)
   RedirectTo("/catalogue")
  } 
  else Alert(S.?("noMeasureSelection"))
 }
 
 def markAsSynonym() : JsCmd = {
  if(SelectedMeasure.is != null) {
	  
	  if(SelectedMeasure.is.status == "synonym") {
	 	  val m = SelectedMeasure.is
	 	  m.status("candidate").save
	 	  SelectedMeasure(m)
	 	  RedirectTo("/catalogue")
	  }
	  else JsRaw("$('#synonymList').show();")
  }
  else Alert(S.?("noMeasureSelection"))
 }
 
 def selectMeasure(id : String) : JsCmd = {
  val m = Measure.findAll(By(Measure.id, id.toLong)).apply(0)
  SelectedMeasure(m)
  RedirectTo("/catalogue")
 } 
 
 def createCatalogueItem(m: Measure): Node = {
  def isZebra(): String = if(SelectedMeasure.is != null && m.id == SelectedMeasure.is.id) "zebraHover" else "" 
  var featureName = ""
  val features = Feature.findAll(By(Feature.id, m.fkFeature))
  if(features.size > 0) featureName = features.apply(0).name 
  
  val actuality = m.requiredActualityValue.toString + " " + S.?(m.requiredActualityUnit)
  val interest = m.timespanOfInterestValue.toString + " " + S.?(m.timespanOfInterestUnit)
  val storage = m.requiredStorageValue.toString + " " + S.?(m.requiredStorageUnit)
  
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('measure')"), selectMeasure _)._2
  val zebra =  isZebra()

  <tr>
	 <td>{m.subject}</td>
  	 <td>{S.?(m.status)}</td>
  	 <td>{m.shortName}</td>
  	 <td>{m.longName}</td>
  	 <td>{m.unit}</td>
	 <td>{actuality}</td>
	 <td>{interest}</td>
	 <td>{storage}</td>
	 <td>{featureName}</td>
	 <td>{MyUtil.formatDate(m.dateCreated)}</td>
  </tr> % ("onclick" -> action) % new UnprefixedAttribute("measure", m.id.toString, Null) % new UnprefixedAttribute("class", zebra, Null)
 }
 
 def createDetailBlock(title: String): Node = {
  
  def rangeToRow(mr: MeasureRange) = <tr><td>{mr.lowerBound.toString}</td><td>{mr.upperBound.toString}</td><td>{mr.rangeValue.toString}</td><td>{mr.meaning}</td></tr>
  
  def toMathMl(term: String) = {
	  if (term == null || term == "") NodeSeq.Empty 
	  else <math xmlns="http://www.w3.org/1998/Math/MathML"><mrow>{MyUtil.tagIt(WikiParser.parseTerm(term))}</mrow></math>
  }
  
  val m = SelectedMeasure.is
  
  if(m != null) {
	  title match {
	 	  case "definition" => <div class="message"><p class="messageTitle">{S.?("definition")}</p>{m.definition}</div>
	 	  case "formula" 	=> <div class="message"><p class="messageTitle">{S.?("formula")}</p>{toMathMl(m.formula)}</div>
	 	  case "mockup" 	=> <div class="message"><p class="messageTitle">{S.?("mockup")}</p>{toMathMl(m.mockup)}</div>
	 	  case "range" 		=> {
	 	 	  
	 	 	 val ranges = MeasureRange.findAll(By(MeasureRange.fkMeasure, m.id), OrderBy(MeasureRange.lowerBound, Ascending))
	 	 	 
	 	 	 if(ranges.isEmpty) <div class="message"><p class="messageTitle">{S.?("ranges")}</p></div>
	 	 	 else {
	 	 		 
	 	 		<div class="message"><p class="messageTitle">{S.?("ranges")}</p>
	 	  			<table class="protocolTable">
						<thead>
							<tr><td>{S.?("minimalValue")}</td><td>{S.?("maximalValue")}</td><td>{S.?("range")}</td><td>{S.?("meaning")}</td></tr>
						 </thead>
						 <tbody>
						 	{ranges.map(rangeToRow)}
						 </tbody>
					</table>
	 	   		</div>
	 	 	 }
	 	  }
	 	  case "context" => 
	 	   
	 	   <div class="message"><p class="messageTitle">{S.?("context")}</p>
	 	   	<table class="protocolTable">
				<thead>
					<tr><td>{S.?("dimension")}</td><td>{S.?("level")}</td><td>{S.?("aggregation")}</td></tr>
				</thead>
				<tbody>
					{getContext(m)}
				</tbody>
			</table>
		  </div>
	  }
  }
  else <nothing />
 }
  
 def getContext(m: Measure): NodeSeq = {
  def contextTr(key: MeasureToModelVertex) = {
   val allLevel = ModelVertex.findAll(By(ModelVertex.id, key.fkLevel))
   
   if(allLevel.isEmpty) <tr><td></td><td></td><td>{S.?(key.aggregation)}</td></tr>
   else {
	   
	val level = allLevel.apply(0)
	val dimensions = ModelVertex.findAll(By(ModelVertex.id, level.referenceId), By(ModelVertex.elementType, "dimension"))
	<tr><td>{if(!dimensions.isEmpty) dimensions(0).elementName else ""}</td><td>{level.elementName}</td><td>{S.?(key.aggregation)}</td></tr>
   }
  }
  
  MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkMeasure, m.id)).map(contextTr).toSeq
 }
 
 def measures (xhtml: NodeSeq): NodeSeq = {
  def selectCatalogueStatus(status: String): JsCmd = {
	SelectedStatus(status)
	if(status == "all") SetHtml("catalogueTableRows", Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id)).map(createCatalogueItem).toSeq)
	else SetHtml("catalogueTableRows", Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id), By(Measure.status, status)).map(createCatalogueItem).toSeq)
  }
  
  def markSynonym(id: String) = {
   val m = SelectedMeasure.is
   m.status("synonym").fkSynonym(id.toLong).save
   RedirectTo("/catalogue")
  }
  
  def synonyms(): List[(String, String)] = {
   val msrs = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id), By(Measure.status, "approved"))
   if(SelectedMeasure.is != null) msrs.filter(_.id != SelectedMeasure.is.id).map(m => (m.id.toString, m.shortName.toString)).toList ::: List(("0", ""))
   else msrs.map(m => (m.id.toString, m.shortName.toString)).toList ::: List(("0", ""))
  }
	
  val stati = List(("all", S.?("all")), ("candidate", S.?("candidate")), ("approved", S.?("approved")), ("synonym", S.?("synonym")), ("deprecated", S.?("deprecated")))
	
  bind("catalogue", xhtml, "rows"    		  -> Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id)).map(createCatalogueItem).toSeq,
						   "add"     		  -> ajaxButton(S.?("add"), addIt _) % ("class" -> "standardButton"),
						   "edit"    		  -> ajaxButton(S.?("edit"), editIt _) % ("class" -> "standardButton"),
						   "choice"  		  -> SHtml.ajaxSelect(stati, Full("all"), s => selectCatalogueStatus(s)),
						   "discard"    	  -> ajaxButton(S.?("discard"), discardIt _) % ("class" -> "standardButton"),
						   "markAsSynonym"    -> ajaxButton(S.?("markAsSynonym"), markAsSynonym _) % ("class" -> "standardButton"),
						   "synonyms"  		  -> SHtml.ajaxSelect(synonyms(), Full("0"), id => markSynonym(id)) % ("id" -> "synonymList"),
						   "definition" 	  -> createDetailBlock("definition"),
						   "context" 		  -> createDetailBlock("context"),
						   "formula" 	      -> createDetailBlock("formula"),
						   "mockup" 		  -> createDetailBlock("mockup"),
						   "range" 			  -> createDetailBlock("range"))
 }
 
 /*
  * The following methods are integrated into the measure editor
  */
 
 def chooseContext(levelId: Long, selected: Boolean): JsCmd = {
  def reduce(id: Long, list: List[(Long, String)]): List[(Long, String)] = list match {
	  case Nil => Nil
	  case (n, aggr) :: tail => if(n == id) reduce(id, tail) else (n, aggr) :: reduce(id, tail)
  }	  
	   
  if(selected) Context((levelId, "none") :: Context.is) else Context(reduce(levelId, Context.is))
  Noop 
 }
 
 def changeAggregation(levelId: Long, str: String): JsCmd = {
  def change(id: Long, text: String, list: List[(Long, String)]): List[(Long, String)] = list match {
	  case Nil => Nil
	  case (n, someAggregation) :: tail => if(n == id)  (n, text) :: tail else (n, someAggregation) :: change(id, text, tail)
  }	
  
  Context(change(levelId, str, Context.is))
  Noop
 }
 
 def editContext(): NodeSeq = {
  def getAggregation(l: List[MeasureToModelVertex]): Box[String] = if(l.isEmpty) Empty else Full(l(0).aggregation.toString)
  val m = SelectedMeasure.is
  val aggregationMethods = List(("noAggregation", S.?("noAggregation")),("sum", S.?("sum")), ("min",S.?("min")), ("max", S.?("max")), ("avg", S.?("avg")), ("count", S.?("count")), ("countDistinct", S.?("countDistinct")))
  
  Context(MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkMeasure, m.id)).map(m2m => (m2m.fkLevel.toLong, m2m.aggregation.toString)).toList)
  
  val dimensions = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.id), By(ModelVertex.elementType, "dimension"), By(ModelVertex.elementKind, "original"), NullRef(ModelVertex.validUntil))
  val allRows = for(d <- dimensions;
	  			    val level = ModelVertex.findAll(By(ModelVertex.referenceId, d.id), By(ModelVertex.elementType, "level"), By(ModelVertex.elementKind, "original"), NullRef(ModelVertex.validUntil));
	  			    val rows = for(l <- level;
	  			                   val ranges = MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkLevel, l.id), By(MeasureToModelVertex.fkMeasure, m.id)).toList
	  			                   ) yield
	  						<tr>
	 	  						<td>{SHtml.ajaxCheckbox(!ranges.isEmpty, selected => chooseContext(l.id, selected))}</td>
	 	  						<td>{d.elementName}</td>	
	 	  						<td>{l.elementName}</td>
	 	  						<td>{SHtml.ajaxSelect(aggregationMethods, getAggregation(ranges), v => changeAggregation (l.id, v))}</td>
	 	  					</tr>
	 	  			) yield MyUtil.flattenNodeSeq(rows)
  
  MyUtil.flattenNodeSeq(allRows)
 }
 
 def orderRange(left: (Option[Double], Option[Double], Option[Double], String), right: (Option[Double], Option[Double], Option[Double], String), expectedOrder: Boolean): Boolean = {
  left._1 match {
  	case Some(leftValue) => right._1 match { 
  		case Some(rightValue) => if(expectedOrder) leftValue < rightValue else leftValue > rightValue
	 	case None => false
	}	
	case None => true
  }
 }
 
 def ltRange(left: (Option[Double], Option[Double], Option[Double], String), right: (Option[Double], Option[Double], Option[Double], String)): Boolean = orderRange(left, right, false)
 def gtRange(left: (Option[Double], Option[Double], Option[Double], String), right: (Option[Double], Option[Double], Option[Double], String)): Boolean = orderRange(left, right, true)
 
 def buildRangeModel(): List[(Option[Double], Option[Double], Option[Double], String)] = {
  def v(mr: MeasureRange) = {
	  if(mr.lowerBound == null && mr.upperBound == null) (None, None, Some(mr.rangeValue.toString.toDouble), mr.meaning.toString)
	  else if (mr.lowerBound == null) (None, Some(mr.upperBound.toString.toDouble), Some(mr.rangeValue.toString.toDouble), mr.meaning.toString)
	  else if (mr.upperBound == null) (Some(mr.lowerBound.toString.toDouble), None, Some(mr.rangeValue.toString.toDouble), mr.meaning.toString)
	  else (Some(mr.lowerBound.toString.toDouble), Some(mr.upperBound.toString.toDouble), Some(mr.rangeValue.toString.toDouble), mr.meaning.toString)
  }
  
  val ranges = MeasureRange.findAll(By(MeasureRange.fkMeasure, SelectedMeasure.is.id)).map(v)
  println("The ranges are " + ranges.toString)
  ranges.sort(gtRange)
 }
 
 def editRange(ranges: List[(Option[Double], Option[Double], Option[Double], String)]): NodeSeq = {
  def updateRange(text: String, index: Int, kind: String) = {
	def updateItem(text: String, item: (Option[Double], Option[Double], Option[Double], String), kind: String) = {
		
		kind match {
    		case "lowerBound" => (Some(text.toDouble), item._2, item._3, item._4)
    		case "upperBound" => (item._1, Some(text.toDouble), item._3, item._4)
    		case "rangeValue" => (item._1, item._2, Some(text.toDouble), item._4)
    		case "meaning" => (item._1, item._2, item._3, text)
		}
		
	}
    
	val newRange = (Ranges.is take index) ::: (updateItem(text, Ranges.is.apply(index), kind) :: (Ranges.is drop (index + 1)))
	Ranges(newRange)
    Noop
  }
  
  def tr(item: (Option[Double], Option[Double], Option[Double], String), index: Int, alsoEditLowerBound: Boolean): Node = {
   def optD(d: Option[Double]) = d match {case Some(v) => v.toString; case None => ""} 
   def lb(item: (Option[Double], Option[Double], Option[Double], String), index: Int, alsoEditLowerBound: Boolean) = {
	   if(alsoEditLowerBound) ajaxText(optD(item._1), text => updateRange(text, index, "lowerBound")) 
	   else <span>{optD(item._1)}</span>
   }
                                                          
  <tr>
	<td>{lb(item, index, alsoEditLowerBound)}</td>
    <td>{ajaxText(optD(item._2), text => updateRange(text, index, "upperBound"))}</td>
    <td>{ajaxText(optD(item._3), text => updateRange(text, index, "rangeValue"))}</td>
    <td>{ajaxText(item._4, text => updateRange(text, index, "meaning"))}</td>
  </tr>
 }
 
  def up(l: List[(Option[Double], Option[Double], Option[Double], String)], index: Int): List[Node] = l match {
	  case Nil => Nil
	  case head :: tail => tr(head, index, false) :: up(tail, index + 1)  
  }
  
  if(ranges.size > 0) MyUtil.flattenNodeSeq(tr(ranges.head, 0, true) :: up(ranges.tail, 1))  else <nothing />	  
 }
 
 def addRange(): JsCmd = {
  def lb(r: List[(Option[Double], Option[Double], Option[Double], String)]) = if(r.isEmpty) None else r.sort(ltRange).apply(0)._2
  
  Ranges(Ranges.is ::: List((lb(Ranges.is), None, None, "")))
  SetHtml("measureRangeTableBody", editRange(Ranges.is))
 }
 
 def removeRange(): JsCmd = {
  Ranges(Ranges.is.sort(ltRange).tail.sort(gtRange))
  SetHtml("measureRangeTableBody", editRange(Ranges.is))
 }
 
 def editMeasures (xhtml: NodeSeq): NodeSeq = {
  bind("edit", xhtml, "context"     -> editContext(),
					  "range"       -> editRange(buildRangeModel()),
					  "addRange"    -> ajaxButton("+", addRange _) % ("class" -> "standardButton"),
					  "removeRange" -> ajaxButton("-", removeRange _) % ("class" -> "standardButton"))
 }
 
 /**
  *  Suggestion lists for autocomplete
  */
 
 def initializeCompletion(): JsCmd = {
  val msrs = JsonUtility.list2Json(Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id)).map(_.shortName.toString))
  val attrs = JsonUtility.list2Json(ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "attribute")).map(_.elementName.toString))
  val cmd = "initialize(" + msrs + ", " + attrs + ");"
  println(cmd)
  JsRaw(cmd)
 }
 
 def completion (xhtml: NodeSeq): NodeSeq = {
  bind("completion", xhtml, "sources"     -> Script(initializeCompletion()))
 }
}