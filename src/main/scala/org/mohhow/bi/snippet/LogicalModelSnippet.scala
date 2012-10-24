package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import http._
import SHtml._
import S._

import js._
import js.jquery._
import JsCmds._

import mapper._
import util._
import Helpers._

import scala.xml._
import JE.{JsRaw,Str}
import java.util.Date
import org.mohhow.bi.lib.Repository
import js.jquery.JqJsCmds._
import org.mohhow.bi.util.{Utility => MyUtil}
import scala.collection.mutable.HashMap
import org.mohhow.bi.lib.WikiParser

object SelectedModelItem extends SessionVar[ModelVertex](null)

class LogicalModelSnippet {
	
 def serializeVertex(v: ModelVertex): Node = {
   <v vertexId={v.id.toString} modelId='-1'>
  	<elementType>{v.elementType}</elementType>
  	<elementName>{v.elementName}</elementName>
  	<x>{v.x}</x>
  	<y>{v.y}</y>
    <detail>{v.elementDetail}</detail>
  	<scale>{v.scale}</scale>
    <usage>0</usage>
  	<status>sync</status>
   </v> 
 }
 
 def serializeEdge(e: ModelEdge): Node = {
  <e edgeId={e.id.toString} modelId='-1'>
  	<h>{e.head}</h>
  	<t>{e.tail}</t>
  	<status>sync</status>
  </e>
 }
 
 /**
  * reads all vertex and edge elements of a given reference id, in other words all elements of one drawing, from 
  * the database, serializes them as XML node of the web page and calls the Javascript command which draws the SVG
  */
 
 def draw(referenceId: Long): JsCmd = {
  val vertices = MyUtil.flattenNodeSeq(ModelVertex.findAll(By(ModelVertex.referenceId, referenceId), By(ModelVertex.isCurrent, 1)).map(serializeVertex).toList)
  val edges = MyUtil.flattenNodeSeq(ModelEdge.findAll(By(ModelEdge.referenceId, referenceId), By(ModelEdge.isCurrent, 1)).map(serializeEdge).toList)
  val command1 = SetHtml("logicalModelVertices", vertices)
  val command2 = SetHtml("logicalModelEdges", edges)
  CmdPair(CmdPair(command1, command2), JsRaw("drawModel(true);"))
 }
	
 def selectModelItem(id: String): JsCmd = {
  println("ich werde immer noch gestartet")
  val m = ModelVertex.findAll(By(ModelVertex.id, id.toLong)).apply(0)
  SelectedModelItem(m)
  CmdPair(draw(m.id), JsRaw("$('.listItem').removeClass('zebraHover');$(\".listItem[referenceId='" + id + "']\").addClass('zebraHover');"))
 }
 
 def transformItem(m: ModelVertex) = {
   val action = SHtml.ajaxCall(JsRaw("$(this).attr('referenceId')"), selectModelItem _)._2
   <li class='listItem'>{m.elementName}</li> % ("onclick" -> action) % new UnprefixedAttribute("referenceId", m.id.toString, Null)
 }
 
 def getSelectionItems(elementType: String): NodeSeq = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), 
		 																   By(ModelVertex.elementKind, "original"),
		 															       By(ModelVertex.elementType, elementType)).map(transformItem).toSeq
 
 def uniqueElementName(name: String, elementType: String): Boolean = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id),
		 																		By(ModelVertex.elementKind, "original"),
		 																		By(ModelVertex.elementType, elementType), By(ModelVertex.elementName, name)).isEmpty
 
 def createElement(elementName: String, elementType: String): JsCmd = {
  if(uniqueElementName(elementName, elementType)) {
   if(elementName.length > 0 && elementName.length <= 50) {
		val d = ModelVertex.create
		d.fkScenario(SelectedScenario.is).validFrom(new Date).isCurrent(1).elementType(elementType).elementName(elementName).elementKind("original").x(500).y(200).scale(1).save
		 
		// after saving, the element id can be used as reference for this diagram
		d.referenceId(d.id).save
		SelectedModelItem(d)
		CmdPair(CmdPair(Unblock, draw(d.referenceId)), updateItemList(elementType))
   }
   else Alert(S.?("textDoesNotFit"))
  }
  else Alert(S.?("noUniqueNameTypeCombination"))
 }
 
 def updateItemList(elementType: String): JsCmd = JsCmds.SetHtml(elementType + "List", getSelectionItems(elementType))
 
 def cancelCreateElement(): JsCmd = Unblock

 def createElement(xhtml: NodeSeq): NodeSeq = {
  bind("create", xhtml, "dim" -> SHtml.ajaxText("", text => createElement(text, "dimension")) % ("size" -> "30") % ("maxlength", "50"),
		  				"cube" -> SHtml.ajaxText("", text => createElement(text, "cube")) % ("size" -> "30") % ("maxlength", "50"),
		                "cancel" -> ajaxButton(S.?("cancel"), cancelCreateElement _) % ("class" -> "standardButton"))
 }
 
 def addElement(elementType: String) : JsCmd = {
  def choice(elmType: String) = {if(elmType == "dimension") <create:dim /> else <create:cube />} 
	 
  ModalDialog(<div class="modalMessage">
		  			<lift:LogicalModelSnippet.createElement>
		  				<label>{S.?("enterName")}</label>{choice(elementType)}<br /><br />
                		<create:cancel/>
                	</lift:LogicalModelSnippet.createElement>
              </div>)
 }
 
 def editAdapt() : JsCmd = {
  if(SelectedModelItem.is == null) Alert(S.?("noModelSelection")) else {
 
	  var command = "$('#logicDisplay').fadeOut(); $('#logicDisplayMenu').hide(); $('#logicEditMenu').show();"
	  command = command + "drawModel(false);"
	  
	  if(SelectedModelItem.is.elementType == "dimension") command = command + "createPalette('dimension');"
	  else command = command + "createPalette('cube');"
  
	  JsRaw(command)
  }
 }

 def cancel() : JsCmd = CmdPair(JsRaw("$('#logicDisplay').fadeIn(); $('#logicDisplayMenu').show(); $('#logicEditMenu').hide(); editPaper.clear(); drawModel(true);"), RedirectTo("/adapt")) 
 def addDimension = addElement("dimension")
 def addCube = addElement("cube")
 
 def makeSuggestions() = {
  def attrOfQuestion(q: List[(String, List[(String, String)])]) = List.flatten(q.map(c => c._2.filter(_._1 == "context").map(_._2)))
  
  val backlogs = ProductBacklog.findAll(By(ProductBacklog.fkScenario, SelectedScenario.is.id)) 
  val features = List.flatten(backlogs.map(b => Feature.findAll(By(Feature.fkPb, b.id), By(Feature.featureType, S.?("businessQuestion")))))
  println(features.map(_.name).toString)
  val attributeList = List.flatten(features.map(f => attrOfQuestion(WikiParser.contentOfQuestion(f.description)._2))).distinct.sort(_ < _)
  println("The attribute list is " + attributeList)
  val usedAttributes = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "attribute")).map(_.elementKind.toString).distinct
  val formated = attributeList.map(a => if(usedAttributes exists (_ == a)) <span class="used">{a}</span> else <span class="unused">{a}</span>)
  if(formated.isEmpty) <span /> else MyUtil.flattenNodeSeq(List.flatten(formated.tail.map(item => List(item, <span>, </span>))) ::: List(formated.head))
 }
 
 def model (xhtml: NodeSeq): NodeSeq = {
  bind("adapt", xhtml, "cubes" -> getSelectionItems("cube"),
		               "dimensions" -> getSelectionItems("dimension"),
		               "addDimension" -> ajaxButton(S.?("addDimension"), addDimension _) % ("class" -> "standardButton"),
		               "addCube" -> ajaxButton(S.?("addCube"), addCube _) % ("class" -> "standardButton"),
		               "computeCube" -> ajaxButton(S.?("computeCubes"), computeCube _) % ("class" -> "standardButton"),
		               "edit"  -> ajaxButton(S.?("edit"), editAdapt _) % ("class" -> "standardButton"),
		               "cancel"  -> ajaxButton(S.?("cancel"), cancel _) % ("class" -> "standardButton"),
		               "suggestions" -> makeSuggestions(),
		               "save"  -> <button class='standardButton'>{S.?("save")}</button> % ("onclick" -> SHtml.ajaxCall(JsRaw("$('#logicalModelContainer').html()"), saveLogicalModel _)._2)) 
 }
	
 def computeCube() : JsCmd = {
  
  def copyVertex(v: ModelVertex, referenceId: Long) = {
   val newVertex = ModelVertex.create
   if(v != null) {
	   newVertex.fkScenario(SelectedScenario.is).referenceId(referenceId).elementType(v.elementType).elementName(v.elementName).elementDetail(v.elementDetail).scale(1).validFrom(new Date).isCurrent(1).elementKind("copy").save
   }
   else {
	   newVertex.fkScenario(SelectedScenario.is).referenceId(referenceId).elementType("undefined").elementName("undefined").scale(1).validFrom(new Date).isCurrent(1).elementKind("copy").save
   }
   newVertex
  }
  
  def findLevel(m: Measure) = MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkMeasure, m.id)).map(m2v => m2v.fkLevel.toLong).toList.sort(_ < _)
  
  def findDimension(levelId: Long) = {
   val level = ModelVertex.findAll(By(ModelVertex.id, levelId))
   if(level.isEmpty) null else {
	   val dims = ModelVertex.findAll(By(ModelVertex.id, level(0).referenceId))
	   if(dims.isEmpty) null else dims(0)
   }
  }
  
  def isConnected(v1: Long, v2: Long) = !ModelEdge.findAll(By(ModelEdge.head, v1), By(ModelEdge.tail, v2)).isEmpty || !ModelEdge.findAll(By(ModelEdge.head, v2), By(ModelEdge.tail, v1)).isEmpty
 
  def findLevelVertices(c: ModelVertex) = {
	val copies = ModelVertex.findAll(By(ModelVertex.referenceId, c.id), By(ModelVertex.elementType, "level"))  //findClosure(c.id, List(c.id))
	List.flatten(copies.map(l => ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "level"), 
			                            By(ModelVertex.elementName, l.elementName), By(ModelVertex.elementKind, "original")))).map(v => v.id.toLong)
  }
  
  def countMeasures(cube: ModelVertex) = Measure.findAll(By(Measure.fkCube, cube.id), By(Measure.status, "approved")).size
  
  def createCube(level: List[Long], measures: List[Measure]) = {
   val cubeNames = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "cube")).map(v => v.elementName.toString).toList
   val newCube = ModelVertex.create
   newCube.fkScenario(SelectedScenario.is).elementType("cube").elementKind("original").elementName(MyUtil.makeUnique("C", cubeNames, 0)).elementDetail("automaticGeneration").x(440).y(285).scale(1).validFrom(new Date).isCurrent(1).save
   newCube.referenceId(newCube.id).save
   
   var i = 0
   
   for(l <- level) {
	   val dimensionCopy = copyVertex(findDimension(l), newCube.id)
	   dimensionCopy.x(Math.floor(500 + 100 * Math.cos(2*i*Math.Pi/level.size)).toLong).y(Math.floor(325 + 100 * Math.sin(2*i*Math.Pi/level.size)).toLong).save
	   val levelCopy = copyVertex(ModelVertex.findAll(By(ModelVertex.id, l)).apply(0), newCube.id)
	   levelCopy.x(Math.floor(500 + 300 * Math.cos(2*i*Math.Pi/level.size)).toLong).y(Math.floor(325 + 300 * Math.sin(2*i*Math.Pi/level.size)).toLong).save
	   val cubeToDimension = ModelEdge.create
	   cubeToDimension.fkScenario(SelectedScenario.is).referenceId(newCube.id).head(newCube).tail(dimensionCopy).validFrom(new Date).isCurrent(1).save
	   val dimensionToLevel = ModelEdge.create
	   dimensionToLevel.fkScenario(SelectedScenario.is).referenceId(newCube.id).head(dimensionCopy).tail(levelCopy).validFrom(new Date).isCurrent(1).save
	   i = i + 1
   }
   
   newCube
  }
  
  def findCube(level: List[Long], measures: List[Measure], cubes: List[(ModelVertex, List[Long])]): ModelVertex = {
   val allMatches = cubes.filter(c => c._2.diff(level).isEmpty && level.diff(c._2).isEmpty).sort((c1, c2) => measures.filter(m => m.fkCube  == c1._1.id).size <	measures.filter(m => m.fkCube  == c2._1.id).size)
   if(allMatches.isEmpty) createCube(level, measures) else allMatches(0)._1
  }
  
  def measuresOfAGroup(group: (String,List[Long])) = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id)).filter(m => m.subject == group._1 && findLevel(m) == group._2).toList
   
  val msrs = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id), By(Measure.status, "approved")).filter(m => m.formula == null || m.formula.length == 0)
  val groups = msrs.map(m => (m.subject.toString, findLevel(m))).distinct.toList
  println("The groups are " + groups.toString)
  val cubes = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "cube")).map(c => (c, findLevelVertices(c))).toList
  println("The cubes are " + cubes.toString)
  
  for(group <- groups) {
   val msrs = measuresOfAGroup(group)
   println("The measures of group " + group.toString + " are " + msrs.toString)
   val cubeCandidate = findCube(group._2, msrs, cubes)
   for(measure <- msrs) if(measure.fkCube != cubeCandidate.id) measure.fkCube(cubeCandidate.id).save
  }
  
  for(cube <- cubes.map(_._1)) if(countMeasures(cube) == 0) cube.elementKind("deprecated").save
  
  RedirectTo("/adapt")
 }
 
 def getNodeText(node: Node): String = node match {
  case Elem(_, _, _, _, Text(myText)) => myText
  case _ => ""
 }
 
 
 def saveLogicalModel(modelString: String) : JsCmd = {
  val vertexIdTranslation = new HashMap[String, Long]
  var command = "";
  
  def getReferenceType(refId: Long): String = {
   val mv = ModelVertex.findAll(By(ModelVertex.id, refId))
   if(mv.isEmpty) "unknown" else mv.apply(0).elementType  
  }
  
  def getVertex(vertexId: String): ModelVertex = {
	  if(vertexId.startsWith("m")) {
	 	  val newVertex = ModelVertex.create
	 	  newVertex.validFrom(new Date).isCurrent(1).save
	 	  newVertex
	  }
	  else ModelVertex.findAll(By(ModelVertex.id, vertexId.toLong)).apply(0)
  }
  def getEdge(edgeId: String): ModelEdge = {
	  if(edgeId.startsWith("m")) {
		  val newEdge = ModelEdge.create
		  newEdge.validFrom(new Date).isCurrent(1).save
		  newEdge
	  }
	  else ModelEdge.findAll(By(ModelEdge.id, edgeId.toLong)).apply(0)
  }
  
  def nvl(text: String) = if(text == null) "" else text
  
  val model = XML.loadString(modelString)
  val vertices = model \\ "v"
	
  for(vertex <- vertices) {
		val status = getNodeText((vertex \\ "status").apply(0))
		
		if(status != "sync") {
			val vertexId = (vertex \ "@vertexId").text
			val elementType = getNodeText((vertex \\ "elementType").apply(0))
			val elementName = getNodeText((vertex \\ "elementName").apply(0))
			val x = getNodeText((vertex \\ "x").apply(0))
			val y = getNodeText((vertex \\ "y").apply(0))
			val scale = getNodeText((vertex \\ "scale").apply(0))
			val detail = getNodeText((vertex \\ "detail").apply(0))
			val mv = getVertex(vertexId);
			
			if(status == "removed" && !vertexId.startsWith("m")) {
				mv.isCurrent(0).validUntil(new Date).save
				command = command + "$('#logicalModelVertices v[vertexId=" + vertexId + "]').remove();"
			}
			else {
				try {
					mv.elementType(elementType).elementName(nvl(elementName)).elementDetail(nvl(detail)).x(x.toLong).y(y.toLong).scale(scale.toLong).isCurrent(1).referenceId(SelectedModelItem.is.id).fkScenario(SelectedScenario.is)
					if((elementType == "level" || elementType == "hierarchy") && getReferenceType(SelectedModelItem.is.id) == "dimension") mv.elementKind("original")
					
					if(elementType == "attribute") mv.elementKind(getNodeText((vertex \\ "usage").apply(0)))
					
					mv.save
					
					if(vertexId.startsWith("m")) vertexIdTranslation += vertexId -> mv.id.toLong
				}
				catch{
					 case ex: Exception => println(ex.toString);
				}
			}
		
		}
  }
  
  val edges = model \\ "e"
  
  def findVertexId(text: String): Long = if (text.startsWith("m")) vertexIdTranslation(text) else text.toLong
    
  for(edge <- edges) {
    	val status = getNodeText((edge \\ "status").apply(0))
		
		if(status != "sync") {
			val edgeId = (edge \ "@edgeId").text;
			val head = findVertexId(getNodeText((edge \\ "h").apply(0)))
			val tail = findVertexId(getNodeText((edge \\ "t").apply(0)))
			
			val me = getEdge(edgeId);
			
			if(status == "removed"){
				me.isCurrent(0).validUntil(new Date).save
				command = command + "$('#logicalModelEdges e[edgeId=" + edgeId + "]').remove();"
			}
			else me.head(head).tail(tail).referenceId(SelectedModelItem.is.id).fkScenario(SelectedScenario.is).isCurrent(1).save
		}
  }
  
  command = command + "$('#logicalModelVertices v status').empty();$('#logicalModelVertices v status').append('sync');"
  command = command + "$('#logicDisplay').fadeIn(); $('#logicDisplayMenu').show(); $('#logicEditMenu').hide();editPaper.clear();drawModel(true);" 
  CmdPair(JsRaw(command), RedirectTo("/adapt")) 
 }	
}