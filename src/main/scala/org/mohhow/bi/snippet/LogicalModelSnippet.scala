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
 
 def draw(referenceId: Long): JsCmd = {
  val vertices = MyUtil.flattenNodeSeq(ModelVertex.findAll(By(ModelVertex.referenceId, referenceId)).map(serializeVertex).toList)
  val edges = MyUtil.flattenNodeSeq(ModelEdge.findAll(By(ModelEdge.referenceId, referenceId)).map(serializeEdge).toList)
  val command1 = SetHtml("logicalModelVertices", vertices)
  val command2 = SetHtml("logicalModelEdges", edges)
  CmdPair(CmdPair(command1, command2), JsRaw("drawModel(true);"))
 }
	
 def selectModelItem(id: String): JsCmd = {
  val m = ModelVertex.findAll(By(ModelVertex.id, id.toLong)).apply(0)
  SelectedModelItem(m)
  CmdPair(draw(m.id), JsRaw("$('.listItem').removeClass('zebraHover');$(this).addClass('zebraHover');"))
 }
	
 def transformItem(m: ModelVertex) = {
   val action = SHtml.ajaxCall(JsRaw("$(this).attr('referenceId')"), selectModelItem _)._2
   <li class='listItem'>{m.elementName}</li> % ("onclick" -> action) % new UnprefixedAttribute("referenceId", m.id.toString, Null)
 }
 
 def getSelectionItems(elementType: String): NodeSeq = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), 
		 																		By(ModelVertex.elementType, elementType)).map(transformItem).toSeq
 
 def uniqueElementName(name: String, elementType: String): Boolean = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), 
		 																		By(ModelVertex.elementType, elementType), By(ModelVertex.elementName, name)).isEmpty
 
 def createElement(elementName: String, elementType: String): JsCmd = {
	 if(uniqueElementName(elementName, elementType)) {
		 val d = ModelVertex.create
		 d.fkScenario(SelectedScenario.is).validFrom(new Date).isCurrent(1).elementType(elementType).elementName(elementName).x(340).y(305).scale(1).save
		 
		 // after saving, the element id can be used as reference for this diagram
		 d.referenceId(d.id).save
		 SelectedModelItem(d)
		 CmdPair(Unblock, CmdPair(draw(d.referenceId), updateItemList("dimension"))) 
	 }
	 else { Alert("An element of this type with this name already exists.") } 
 }
 
 
 
 def updateItemList(elementType: String): JsCmd = JsCmds.SetHtml(elementType + "List", getSelectionItems(elementType))
 
 def cancelCreateElement(): JsCmd = {
	 Unblock
 }
 
 def createElement(xhtml: NodeSeq): NodeSeq = {
  bind("create", xhtml, "dim" -> SHtml.ajaxText("", text => createElement(text, "dimension")) % ("size" -> "30") % ("maxlength", "50"),
		  				"cube" -> SHtml.ajaxText("", text => createElement(text, "cube")) % ("size" -> "30") % ("maxlength", "50"),
		                "cancel" -> ajaxButton("Cancel", cancelCreateElement _) % ("class" -> "standardButton"))
 }
 
 def addElement(elementType: String) : JsCmd = {
  def choice(elmType: String) = {if(elmType == "dimension") <create:dim /> else <create:cube />} 
	 
  ModalDialog(<div class="modalMessage">
		  			<lift:LogicalModelSnippet.createElement>
		  				<label>Enter name</label>{choice(elementType)}<br /><br />
                		<create:cancel/>
                	</lift:LogicalModelSnippet.createElement>
              </div>)
 }
 
 def editAdapt() : JsCmd = {
  if(SelectedModelItem.is == null) Alert("Please choose a dimension or cube at first") else {
 
	  var command = "$('#logicDisplay').fadeOut(); $('#logicDisplayMenu').hide(); $('#logicEditMenu').show();"
	  command = command + "drawModel(false);"
	  
	  if(SelectedModelItem.is.elementType == "dimension") {
  		command = command + "createPalette('dimension');"
	  }
	  else {
		command = command + "createPalette('cube');"
	  }
  
	  JsRaw(command)
  }
 }

 def cancel() : JsCmd = JsRaw("$('#logicDisplay').fadeIn(); $('#logicDisplayMenu').show(); $('#logicEditMenu').hide(); editPaper.clear(); drawModel(true);")
 def addDimension = addElement("dimension")
 def addCube = addElement("cube")
 
 def model (xhtml: NodeSeq): NodeSeq = {
  bind("adapt", xhtml, "cubes" -> getSelectionItems("cube"),
		               "dimensions" -> getSelectionItems("dimension"),
		               "addDimension" -> ajaxButton("Add Dimension", addDimension _) % ("class" -> "standardButton"),
		               "addCube" -> ajaxButton("Add Cube", addCube _) % ("class" -> "standardButton"),
		               "computeCube" -> ajaxButton("Compute Cubes", computeCube _) % ("class" -> "standardButton"),
		               "edit"  -> ajaxButton("Edit", editAdapt _) % ("class" -> "standardButton"),
		               "cancel"  -> ajaxButton("Cancel", cancel _) % ("class" -> "standardButton"),
		               "save"  -> <button class='standardButton'>Save</button> % ("onclick" -> SHtml.ajaxCall(JsRaw("$('#logicalModelContainer').html()"), saveLogicalModel _)._2)) 
 }
	
 def computeCube() : JsCmd = {
    Noop
 }
 
 def getNodeText(node: Node): String = node match {
  case Elem(_, _, _, _, Text(myText)) => myText
  case _ => ""
 }
 
 
 def saveLogicalModel(modelString: String) : JsCmd = {
   
  var command = "";
  def getVertex(vertexId: String): ModelVertex = if(vertexId == "") ModelVertex.create else ModelVertex.findAll(By(ModelVertex.id, vertexId.toLong)).apply(0)
  def getEdge(edgeId: String): ModelEdge = if(edgeId == "") ModelEdge.create else ModelEdge.findAll(By(ModelEdge.id, edgeId.toLong)).apply(0)
  
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
			
			if(status == "removed" && vertexId != "") {
				if(mv.delete_!) command = command + "$('#logicalModelVertices v[vertexId=" + vertexId + "]').remove();"
			}
			else mv.elementType(elementType).elementName(elementName).elementDetail(detail).x(x.toLong).y(y.toLong).scale(scale.toLong).validFrom(new Date).referenceId(SelectedModelItem.is.id).fkScenario(SelectedScenario.is).save
		}
  }
  
  val edges = model \\ "e"
  
  def findVertexId(text: String) = {
   if (text.startsWith("m")) {
    vertices.map(v =>((v \ "@vertexId").text, (v \ "@modelId").text)).filter(_._1 == text.substring(1)).apply(0)._2
   }
   else text
  }
    
  for(edge <- edges) {
    	val status = getNodeText((edge \\ "status").apply(0))
		
		if(status != "sync") {
			val edgeId = (edge \ "@edgeId").text;
			val head = findVertexId(getNodeText((edge \\ "h").apply(0)))
			val tail = findVertexId(getNodeText((edge \\ "t").apply(0)))
			
			val me = getEdge(edgeId);
			
			if(status == "removed"){
				if(me.delete_!) command = command + "$('#logicalModelEdges e[edgeId=" + edgeId + "]').remove();"
			}
			else me.head(head.toLong).tail(tail.toLong).referenceId(SelectedModelItem.is.id).fkScenario(SelectedScenario.is).save
		}
  }
  
  command = command + "$('#logicalModelVertices v status').empty();$('#logicalModelVertices v status').append('sync')"
  JsRaw(command) 
 }	
}