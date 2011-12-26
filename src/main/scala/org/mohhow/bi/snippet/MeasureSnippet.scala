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

object SelectedBacklogId extends SessionVar[Long](-1)
object SelectedMeasure extends SessionVar[Measure](null)
object SelectedMeasureId extends SessionVar[Long](-1)
object SelectedStatus extends SessionVar[String]("all")

class MeasureSnippet {

 def addIt() : JsCmd = {
  val measure = Measure.create
  measure.dateCreated(new Date).fkScenario(SelectedScenario.is.id).status("approved")
  SelectedMeasure(measure)
  RedirectTo("/measure")
 }
 
 def editIt() : JsCmd = {
  if(SelectedMeasure.is != null) RedirectTo("/measure") else Alert("No measure selected")
 }
  
 def discardIt() : JsCmd = {
  if(SelectedMeasure.is != null) {
   val m = SelectedMeasure.is
   m.dateDiscarded(new Date).status("deprecated")
   SelectedMeasure(null)
   RedirectTo("/catalogue")
  } 
  else Alert("No measure selected")
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
  
  val actuality = m.requiredActualityValue.toString + " " + m.requiredActualityUnit
  val interest = m.timespanOfInterestValue.toString + " " + m.timespanOfInterestUnit
  val storage = m.requiredStorageValue.toString + " " + m.requiredStorageUnit
  
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('measure')"), selectMeasure _)._2
  val zebra =  isZebra()

  <tr>
	 <td>{m.domain}</td><td>{m.status}</td><td>{m.shortName}</td><td>{m.longName}</td><td>{m.unit}</td>
	 <td>{actuality}</td><td>{interest}</td><td>{storage}</td><td>{featureName}</td><td>{MyUtil.formatDate(m.dateCreated)}</td>
  </tr> % ("onclick" -> action) % new UnprefixedAttribute("measure", m.id.toString, Null) % new UnprefixedAttribute("class", zebra, Null)
 }
 
 def checkFormula(): JsCmd = {
	 Noop
 }
 
 def createDetailBlock(title: String): Node = {
  
  def rangeToRow(mr: MeasureRange) = <tr><td>{mr.lowerBound.toString}</td><td>{mr.upperBound.toString}</td><td>{mr.rangeValue.toString}</td><td>{mr.meaning}</td></tr>
  def toMathMl(term: String) = if (term == null) Empty else <math xmlns="http://www.w3.org/1998/Math/MathML"><mrow>{WikiParser.parseTerm(term)}</mrow></math>
  val m = SelectedMeasure.is
  
  if(m != null) {
	  title match {
	 	  case "definition" => <div class="message"><p class="messageTitle">Definition</p>
	 	   							{m.definition}
	 	   					   </div>
	 	  case "formula" 	=> <div class="message"><p class="messageTitle">Formula</p>{toMathMl(m.formula)}</div>
	 	  case "mockup" 	=> <div class="message"><p class="messageTitle">Mockup Formula</p>
	 	   							{m.mockup}
	 	   					   </div>
	 	  case "range" 		=> {
	 	 	  
	 	 	 val ranges = MeasureRange.findAll(By(MeasureRange.fkMeasure, m.id))
	 	 	 
	 	 	 if(ranges.isEmpty) <div class="message"><p class="messageTitle">Ranges</p></div>
	 	 	 else {
	 	 		 
	 	 		<div class="message"><p class="messageTitle">Ranges</p>
	 	  			<table>
						<thead>
							<tr>
						  	<td>Minimal Value</td>
						    <td>Maximal Value</td>
							<td>Range Value</td>
							<td>Meaning</td>
						  </tr>
						 </thead>
						 <tbody>
						 	{ranges.map(rangeToRow)}
						 </tbody>
					</table>
	 	   		</div>
	 	 		 
	 	 	 }
	 	 	  
	 	  }
	 	  case "context" => 
	 	   
	 	   <div class="message"><p class="messageTitle">Context</p>
	 	   	<table class="roleTable">
				<thead>
					<tr>
						<td>Dimension</td>
						<td>Level</td>
						<td>Aggregation</td>
					</tr>
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
   val level = ModelVertex.findAll(By(ModelVertex.id, key.id)).apply(0)
   val dimensions = ModelVertex.findAll(By(ModelVertex.id, level.referenceId), By(ModelVertex.elementType, "dimension"))
	  
   <tr><td>{if(!dimensions.isEmpty) dimensions(0).elementName else ""}</td><td>{level.elementName}</td><td>{key.aggregation}</td></tr>
  }
  
  
  MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkMeasure, m.id), By(MeasureToModelVertex.isCurrent, 1)).map(contextTr).toSeq
  
 }
 
 def measures (xhtml: NodeSeq): NodeSeq = {
	def selectCatalogueStatus(status: String): JsCmd = {
		SelectedStatus(status)
		if(status == "all") SetHtml("catalogueTableRows", Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id)).map(createCatalogueItem).toSeq)
		else SetHtml("catalogueTableRows", Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id), By(Measure.status, status)).map(createCatalogueItem).toSeq)
	}
	
	val stati = List(("all", "all"), ("candidate", "Candidates"), ("approved", "approved"), ("synonym", "Synonym"), ("deprecated", "deprecated"))
	
	bind("catalogue", xhtml, "rows"    -> Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id)).map(createCatalogueItem).toSeq,
							 "add"     -> ajaxButton("Add", addIt _) % ("class" -> "standardButton"),
							 "edit"    -> ajaxButton("Edit", editIt _) % ("class" -> "standardButton"),
							 "choice"  -> SHtml.ajaxSelect(stati, Full("all"), s => selectCatalogueStatus(s)),
							 "discard"    -> ajaxButton("Discard", discardIt _) % ("class" -> "standardButton"),
							 "definition" -> createDetailBlock("definition"),
							 "context" -> createDetailBlock("context"),
							 "formula" -> createDetailBlock("formula"),
							 "mockup" -> createDetailBlock("mockup"),
							 "range" -> createDetailBlock("range"))
 }
 
 
 //{m.formula}{ajaxButton("Check Formula", checkFormula _) % ("class" -> "standardButton")} <br />
 //	 	   							<span id="checkFormulaResult">Auch die beste Formel hat ihre Schwaechen.</span>
 
 def editMeasures (xhtml: NodeSeq): NodeSeq = {
	
	bind("edit", xhtml, "context"    -> editContext(),
						"range"     -> <nothing /> )
 }
 
 
 def chooseContext(levelId: Long, selected: Boolean): JsCmd = {
  if(selected){
	  // level id und additivitaet hinzufuegen

  }
  else {
	  // nach level suchen und loeschen
  }
  Noop 
 }
 
 def changeAggregation(str: String): JsCmd = {
	 Noop
 }
 
 def editContext(): NodeSeq = {
  val m = SelectedMeasure.is
  val dimensions = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.id), By(ModelVertex.elementType, "dimension"))
  val contextSelection = true;
  val aggregationMethods = List(("nothing", "No Aggregation"),("sum", "Sum"), ("min","Min"), ("max", "Max"), ("avg", "Average"))
	
  val allRows = for(d <- dimensions;
	  			    val level = ModelVertex.findAll(By(ModelVertex.referenceId, d.id), By(ModelVertex.elementType, "level"));
	  			    val rows = for(l <- level) yield
	  						<tr>
	 	  						<td>{SHtml.ajaxCheckbox(contextSelection, selected => chooseContext (l.id, selected))}</td>
	 	  						<td>{d.elementName}</td>	
	 	  						<td>{l.elementName}</td>
	 	  						<td>{SHtml.ajaxSelect(aggregationMethods, Empty, v => changeAggregation (v))}</td>
	 	  					</tr>
	 	  			) yield MyUtil.flattenNodeSeq(rows)
  
  MyUtil.flattenNodeSeq(allRows)
 }
}