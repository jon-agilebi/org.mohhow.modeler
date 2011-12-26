package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import net.liftweb.common._
import http._
import SHtml._
import S._

import js._
import JsCmds._
import JE.{JsRaw,Str}

import mapper._
import util._
import Helpers._

import scala.xml._
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}

object SelectedSpecification extends SessionVar[Specification](null)
object SelectedBlock extends SessionVar[Block](null)
object SelectedSpecificationStatus extends SessionVar[String](null)

class MockupSnippet {
	
 def selectBlock(id : String) : JsCmd = {
  val bl = Block.findAll(By(Block.id, id.toLong)).apply(0)
  SelectedBlock(bl)
  Noop
 } 
 
 def selectSpecification(id : String) : JsCmd = {
  val sp = Specification.findAll(By(Specification.id, id.toLong)).apply(0)
  SelectedSpecification(sp)
  JsCmds.SetHtml("specifications", createSpec(sp))
 } 
	
 def createSpecificationItem(sp: Specification) = {
  def emphasize(spec: Specification) = if(SelectedSpecification.is != null && SelectedSpecification.is.id == spec.id) "emphasizable emphasized" else "emphasizable"
  def createBlockItem(b: Block) = {
	  val blAction = SHtml.ajaxCall(JsRaw("$(this).attr('blockTreeId')"), selectBlock _)._2
	  val item = <span class="emphasizable">{b.name}</span> % ("onclick" -> blAction) % new UnprefixedAttribute("blockTreeId", b.id.toString, Null)
	  
	  <li class="treeItem"><span class="leaf">+</span>{item}</li> 
  }
  
  val spAction = SHtml.ajaxCall(JsRaw("$(this).attr('specTreeId')"), selectSpecification _)._2
  val blocks = Block.findAll(By(Block.fkSpecification, sp.id))
  val spItem = <span>{sp.name}</span> % ("onclick" -> spAction) % new UnprefixedAttribute("specTreeId", sp.id.toString, Null) % new UnprefixedAttribute("class", emphasize(sp), Null)
  
  if(!blocks.isEmpty) {
	  <li class="treeItem">
	  	<span class="handle closed">+</span>{spItem}
	  	<ul>{blocks.map(createBlockItem).toSeq}</ul>
	  </li> 
  }
  else <li class="treeItem"><span class="leaf">+</span>{spItem}</li>   
  
 }
	
 def createSpecificationTree(): NodeSeq = {
  def getSpecifications() = {
	  
	  if(SelectedSpecificationStatus.is == null || SelectedSpecificationStatus.is == "all") {
	 	  Specification.findAll(By(Specification.fkScenario, SelectedScenario.is), OrderBy(Specification.name, Ascending))
	  }
	  else { 
	 	  Specification.findAll(By(Specification.fkScenario, SelectedScenario.is), By(Specification.status, SelectedSpecificationStatus.is), OrderBy(Specification.name, Ascending))
	  }
  }
  
  val specs = getSpecifications()
 
  if(!specs.isEmpty && SelectedSpecification.is == null) SelectedSpecification(specs.apply(0))
  createSpec(SelectedSpecification.is)
  
  <ul>{specs.map(createSpecificationItem)}</ul>
 }
 
 def createSpecificationTreeContainer(): NodeSeq = {
	 <div>
		 <p class="messageTitle">Status {statusChoice()}</p>
		 {createSpecificationTree()}
	 </div>
 }
 
 def createBlock(spId: String, block : Block) : Node = {
 
  <div class="messageBlock block">
	<table class = "blockTable" >
	  <col width="34%" />
	  <col width="66%" />
	  <tr><td>{block.name}</td><td class="dropMeasure">Measures<ul></ul></td></tr>
	  <tr><td rowspan="2">{link("/block", () => Noop, <span>Presentation Type</span>)}</td><td>Dimensions</td></tr>
	  <tr><td>Filter</td></tr>
	</table>	 
  </div> % new UnprefixedAttribute("class", "messageBlock block blockOf" + spId, Null)
 }
 
 def createSpec(spec : Specification) : Node = {
  def editIt = editSpec(spec)	 
  val edit = ajaxButton("Edit", editIt _) % ("class" -> "standardButton") % ("style" -> "float:right")
  
  <div class="message">
  	<p class="messageTitle">
  		{spec.name}{edit}<br />
  		<span>Implementation Type: {spec.implementationType}</span><span style="float:right">Status: {spec.status}</span><br />
        Description <br />
        <span>{spec.description}</span>
  	</p>
  	{Block.findAll(By(Block.fkSpecification, spec.id)).map( b => createBlock(spec.id.toString, b)).toSeq}
  </div>  % new UnprefixedAttribute("id", "specBlock" + spec.id.toString, Null)
 }
 
 def selectStatus(specId: Long, status: String) : JsCmd = {
   val spec = Specification.findAll(By(Specification.id, specId)).apply(0)
   spec.status(status).save
   Noop
 }
 
 def statusChoice(): NodeSeq = {
  def selectStatus(status: String) : JsCmd = {
   SelectedSpecificationStatus(status)
   RedirectTo("specification")
  }
  
  val stati = List(("all", "All"), ("candidate", "Candidates"), ("approved", "approved"), ("deprecated", "deprecated"))
  SHtml.ajaxSelect(stati, Box(SelectedSpecificationStatus.is), st => selectStatus(st))
 }
 
 def createEditSpec(spec : Specification) : Node = {
  
  def selectImplementation(specId: Long, implementationType: String) : JsCmd = {
   val spec = Specification.findAll(By(Specification.id, specId)).apply(0)
   spec.implementationType(implementationType).save
   editSpec(spec)
  } 
  
  def finishIt(): JsCmd = {
	  val cmd1 = JsCmds.SetHtml("specifications", createSpec(SelectedSpecification.is))
	  val cmd2 = JsCmds.SetHtml("specificationTree", createSpecificationTreeContainer())
	  CmdPair(cmd1, cmd2)
  }
  
  def showLinkToDesign(): Node = {
   if(spec.implementationType == "scorecard")  link("/design", () => Noop, <span>Scorecard Design</span>) % new UnprefixedAttribute("id", "linkToDesign", Null)
   else <span />
  }
  
  val stati = List(("candidate", "Candidate"), ("approved", "approved"), ("deprecated", "deprecated"))
  val selectStati = SHtml.ajaxSelect(stati, Box(spec.status), v => selectStatus(spec.id, v))
  val impls = List(("scorecard", "Agile Scorecard"), ("rest", "Rest Service"))
  val nameInput = SHtml.ajaxText(spec.name, text => saveSpecHeader(spec.id, "name", text)) % new UnprefixedAttribute("size", "50", Null) % new UnprefixedAttribute("maxlength", "50", Null) 
  val linkToDesign = showLinkToDesign()
  val selection = SHtml.ajaxSelect(impls, Box(spec.implementationType), v => selectImplementation(spec.id, v))
  val addBlock = ajaxButton("add block", () => createDetail("specBlock" + spec.id.toString, spec.id.toString)) % ("class" -> "standardButton") % ("style" -> "float:right")
  val finish = ajaxButton("Finish", finishIt _) % ("class" -> "standardButton") % ("style" -> "float:right")
  
  <div class="message">
  	<p class="messageTitle">{nameInput} {addBlock}{finish}<br /><label>Implementation Type</label>{selection} {linkToDesign}<span style="float:right">Status: {selectStati}</span></p>
  	{SHtml.ajaxTextarea(spec.description, text => saveSpecHeader(spec.id, "description", text)) % new UnprefixedAttribute("size", "50", Null) % new UnprefixedAttribute("maxlength", "50", Null) }
  	{Block.findAll(By(Block.fkSpecification, spec.id)).map( b => createBlock(spec.id.toString, b)).toSeq}
  </div>  % new UnprefixedAttribute("id", "specBlock" + spec.id.toString, Null)
 }
 
 def addSpec() : JsCmd = {
  val newSpec = Specification.create
  newSpec.name("<new specification>").description("<description>").fkScenario(SelectedScenario.is).dateCreated(new Date).save
  val displaySpecCommand = JsCmds.SetHtml("specifications", createSpec(newSpec))
  val updateTreeCommand = JsCmds.SetHtml("specificationTree", createSpecificationTree())
  CmdPair(displaySpecCommand, updateTreeCommand)
 }
 
 
 def createDetail(containerId : String, spId : String) : JsCmd = {
  val newBlock = Block.create
  newBlock.name("<block>").fkSpecification(spId.toLong)
  newBlock.save
  val newBlockContainer = Utility.trim(createBlock(spId, newBlock)).toString
  val command = "$('#" + containerId + "').append('" + newBlockContainer + "')"
  JsRaw(command)
 }
 
 def editSpec(sp: Specification): JsCmd = {
  val trees = <div>
	  			<h3>Dimensions</h3>
	  			{dimensionTree()}
  				<h3>Measures</h3>
  				{measureTree()}
  			  </div>
  val specCommand = JsCmds.SetHtml("specifications", createEditSpec(sp))
  val treeCommand = JsCmds.SetHtml("specificationTree", trees)
  CmdPair(specCommand, treeCommand)
 }
 
 def saveSpecHeader(specId: Long, selectionKind: String, text : String) : JsCmd = {
  val spec = Specification.findAll(By(Specification.id, specId)).apply(0)
  
  selectionKind match {
	 	  case "name" => spec.name(text)
	 	  case "description" => spec.description(text)
  }
	 
  spec.save	   
  JsCmds.SetHtml("specificationTree", createSpecificationTree())
 }
 
 def copyBlock(): JsCmd = {
  if(SelectedBlock.is != null && SelectedSpecification.is != null) {
   val bl = SelectedBlock.is
   val sp = SelectedSpecification.is
		
   val newBl = Block.create
   newBl.fkSpecification(sp.id).name(bl.name).representationType(bl.representationType).validFrom(new Date).isCurrent(1).save
   JsCmds.SetHtml("specificationTree", createSpecificationTree()) 
  }
  else Alert("Please choose the block to be copied and the specification which shal contain the block!")
 }
 
 def removeBlockOrSpec(isBlock: Boolean): JsCmd = {
  if(isBlock && SelectedBlock.is != null) Block.findAll(By(Block.id, SelectedBlock.is.id)).map(b => b.delete_!)
  if(!isBlock && SelectedSpecification.is != null) Specification.findAll(By(Specification.id, SelectedSpecification.is.id)).map(s => s.delete_!)
  RedirectTo("/specification")
 }
 
 def createSpecification(): NodeSeq = if(SelectedSpecification.is != null) createSpec(SelectedSpecification.is) else <nothing />
	 
 def specification (xhtml: NodeSeq): NodeSeq = {
  def removeBlock() = removeBlockOrSpec(true)
  def removeSpecification() = removeBlockOrSpec(false)
   
  bind("spec", xhtml, "addSpecification"    -> ajaxButton("Add Specification", addSpec _) % ("class" -> "standardButton"),
		  			  "removeSpecification"    -> ajaxButton("Remove Specification", removeSpecification _) % ("class" -> "standardButton"),
		  			  "copyBlock"    -> ajaxButton("Copy Block", copyBlock _) % ("class" -> "standardButton"),
		  			  "removeBlock"    -> ajaxButton("Remove Block", removeBlock _) % ("class" -> "standardButton"),
		  			  "tree" -> createSpecificationTree(),
		  			  "specification" -> createSpecification(),
		  			  "statusChoice" -> statusChoice())
 }
 
 def measureTree(): NodeSeq = {
  def measureLeaf(m: Measure) = <li class="treeItem"><span class="leaf">+</span><span class="emphasizable dragMeasure">{m.shortName}</span></li>
  val measures = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id), OrderBy(Measure.domain, Ascending))
  val domains = measures.groupBy{_.domain}.map{_._2.head}.map(m => m.domain)
  
  val treeList = for(domain <- domains) yield {
	   val measuresOfDomain = measures.filter(_.domain == domain)
	  <li class="treeItem"><span class="handle closed">+</span><span class="emphasizable">{domain}</span><ul>{measuresOfDomain.map(measureLeaf).toSeq}</ul></li>
  }

  MyUtil.flattenNodeSeq(treeList.toList)
 }
	
 def createDimensionTreeItem(v: ModelVertex, level: String, modelElements: List[ModelVertex]): NodeSeq = {
  val goDown = Map("dimension" -> "hierarchy", "hierarchy" -> "level", "level" -> "attribute")
  def isConnected(h: ModelVertex, t: ModelVertex) = !ModelEdge.findAll(By(ModelEdge.head, h.id), By(ModelEdge.tail, t.id)).isEmpty
  def isChild(vertex: ModelVertex) = isConnected(v, vertex) && vertex.elementType == goDown(level)
  def createChildren(v: ModelVertex) = createDimensionTreeItem(v, goDown(level), modelElements)
  
  if(level == "attribute") {
	  
	  <li class="treeItem dragAttribute"><span class="leaf">+</span><span class="emphasizable">{v.elementName}</span></li> 
  }
  else {
	  val children = modelElements.filter(isChild _)
		 
	  if(children.length > 0) <li class="treeItem"><span class="handle closed">+</span><span class="emphasizable">{v.elementName}</span><ul>{children.map(createChildren).toSeq}</ul></li>
	  else <li class="treeItem"><span class="leaf">+</span><span class="emphasizable">{v.elementName}</span></li>
  }
 }

 def dimensionTree(): NodeSeq = {
   val dimensions = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "dimension"))  
   
   val treeList = for (dimension <- dimensions) yield {
	   val modelElements = ModelVertex.findAll(By(ModelVertex.referenceId, dimension.id))
	   createDimensionTreeItem(dimension, "dimension", modelElements)
   }
   
  	MyUtil.flattenNodeSeq(treeList.toList)
 }
 
 /**
  * Scorecard Display
  */
  
 def rotate(): JsCmd = {
  JsRaw("rotateScorecard()");
 }
 
 val lineStyle = List(("Width", "Width", 2), ("Color", "Color", 6), ("DashStyle", "Dash Style", -1))
 val blockStyle = lineStyle ::: List(("Margin", "Margin", 2), ("Padding", "Padding", 2), ("RoundTheEdge", "Edge Radius", 2), ("Background", "Background", 6))
 val fontStyle = List(("Font", "Font", -1), ("Points", "Points",  2), ("Color", "Color", 6), ("Show", "Show", -1))
 val chartStyle = lineStyle ::: List(("Padding", "Padding", 2), ("Background", "Background", 6))
 
 val scorecardEdit = List(("border", "Border", blockStyle), ("block", "Block ", blockStyle), ("line", "Line ", lineStyle), ("axis", "Axis ", lineStyle), ("grid", "Grid ", lineStyle), ("chart", "Chart ", chartStyle))
 val textEdit = List(("scorecardTitle", "Scorecard Title"), ("blockTitle", "Block Title"), ("standardText", "Standard Text"), ("labelText", "Label Text"), ("axisText", "Axis Text"))

 def createEditBlock(topic: String, topicTitle: String, components: List[(String, String, Int)], isFont: Boolean): Node = {
  val layout = (Setup.is \\ "scorecard").apply(0)
  
  def findValue(path: String): String = if(!(layout \\ path).isEmpty) MyUtil.getNodeText((layout \\ path).apply(0)) else ""
  def findFontValue(path: String, kind: String) = kind match {
	  case "Font" => path.split(";").apply(0)
	  case "Points" => path.split(";").apply(1)
	  case "Color" => path.split(";").apply(2) + ";" + path.split(";").apply(3) + ";" + path.split(";").apply(4)
	  case _ => path.split(";").apply(5)
  }
  
  def createRow(componentName: String, componentTitle: String, componentSize: Int, isFont: Boolean): Node =  {
	  
   if(isFont) {
	   
	   <tr>
	   	<td><label>{componentTitle}</label></td>
	    <td><input type="text" id={topic + componentName + "Input" } value={findFontValue(findValue(topic), componentName)} size = {componentSize.toString} maxSize = {componentSize.toString} /></td>
	   </tr>
   }
   else {
	   val path = topic + componentName
	   
	   <tr>
	   	<td><label>{componentTitle}</label></td>
	    <td><input type="text" id={topic + componentName + "Input" } value={findValue(path)} size = {componentSize.toString} maxSize = {componentSize.toString} /></td>
	   </tr>
	   
   }
  }
  
  <div>
  <li class='listItem editHeader'>{topicTitle}</li>
  <li class='listItem editBlock mohhowFormLight'>
  	<table>
  		{components.map(x => createRow(x._1,x._2, x._3, isFont)).toSeq}
  	</table>
  </li>
  </div>
 }
 
 def showScorecardConfiguration(): Node = {
	<div>
	 <ul>
		{scorecardEdit.map(x => createEditBlock(x._1, x._2, x._3, false)).toSeq} 	
     </ul>
	 <ul>
        {textEdit.map(t => createEditBlock(t._1, t._2, fontStyle, true)).toSeq}
     </ul>
    </div>
 }

 def scorecard (xhtml: NodeSeq): NodeSeq = {
  bind("scorecard", xhtml, "configuration" -> showScorecardConfiguration(), 
		                   "turn" -> ajaxButton("Rotate", rotate _) % ("class" -> "standardButton"),
		                   "save" -> ajaxButton("Save", rotate _) % ("class" -> "standardButton"))
 }
 
 /**
  * Scorecard Design
  * 
  */
 
 def cancelDesign(): JsCmd = RedirectTo("/specification")
 
 def rotateDesign(): JsCmd = {
  JsRaw("rotateDesign()");
 }
 
 def design (xhtml: NodeSeq): NodeSeq = {
  bind("scorecard", xhtml, "cancel" -> ajaxButton("Cancel", cancelDesign _) % ("class" -> "standardButton"), 
		                   "turn" -> ajaxButton("Rotate", rotateDesign _) % ("class" -> "standardButton"),
		                   "save" -> ajaxButton("Save", cancelDesign _) % ("class" -> "standardButton"))
 }
}