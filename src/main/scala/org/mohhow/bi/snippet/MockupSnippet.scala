package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.json.DefaultFormats

import http._
import SHtml._
import S._

import js._
import JsCmds._
import JE.{JsRaw,Str,Call}

import mapper._
import util._
import Helpers._

import scala.xml._
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib.Repository
import org.mohhow.bi.lib.WikiParser
import org.mohhow.bi.lib.ModelUtility
import org.mohhow.bi.lib.JsonUtility
import scala.collection.mutable

object SelectedSpecification extends SessionVar[Specification](null)
object SelectedBlock extends SessionVar[Block](null)
object SelectedSpecificationStatus extends SessionVar[String](null)
object Comparisons extends SessionVar[NodeSeq](null)
object PresentationTypes extends SessionVar[NodeSeq](null)
object SelectedBlockInformation extends SessionVar[Node](null)
object DesignBlockInformation extends SessionVar[Node](null)

class MockupSnippet {
	
  /**
   *  methods to create tree with specification and block information
   */
	
 def selectBlock(id : String) : JsCmd = {
  val bl = Block.findAll(By(Block.id, id.toLong)).apply(0)
  SelectedBlock(bl)
  Noop
 } 
 
 def selectSpecification(id : String) : JsCmd = {
  val sp = Specification.findAll(By(Specification.id, id.toLong)).apply(0)
  SelectedSpecification(sp)
  
  val blocks = Block.findAll(By(Block.fkSpecification, sp.id))
  val blockXml = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block").filter(b => blocks.exists(aBlock => aBlock.id.toString == (b \ "@blockId").text))
  
  val blocksAsJson =  prepareInit(blockXml)
  CmdPair(JsCmds.SetHtml("specifications", createSpec(sp)), JsRaw("initializeBlockInformation(\"" + blockXml.toString.replaceAll("\"", "'").replaceAll("\n", "")  + "\");"))
 } 
	
 def createSpecificationItem(sp: Specification) = {
  def emphasize(spec: Specification) = if(SelectedSpecification.is != null && SelectedSpecification.is.id == spec.id) "emphasizableSpec emphasized" else "emphasizableSpec"
  
  def createBlockItem(b: Block) = {
   val blAction = SHtml.ajaxCall(JsRaw("$(this).attr('blockTreeId')"), selectBlock _)._2
   val item = <span class="emphasizableBlock">{b.name}</span> % ("onclick" -> blAction) % new UnprefixedAttribute("blockTreeId", b.id.toString, Null)
	  
   <li class="treeItem"><span class="leaf">__</span>{item}</li> 
  }
  
  val spAction = SHtml.ajaxCall(JsRaw("$(this).attr('specTreeId')"), selectSpecification _)._2
  val blocks = Block.findAll(By(Block.fkSpecification, sp.id))
  val spItem = <span>{sp.name}</span> % ("onclick" -> spAction) % new UnprefixedAttribute("specTreeId", sp.id.toString, Null) % new UnprefixedAttribute("class", emphasize(sp), Null)
  
  if(!blocks.isEmpty) {
	  <li class="treeItem">
	  	<span class="handle closed">__</span>
	    {spItem}
	  	<ul>{blocks.map(createBlockItem).toSeq}</ul>
	  </li> 
  }
  else <li class="treeItem"><span class="leaf">__</span>{spItem}</li>   
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
 
  if(!specs.isEmpty && SelectedSpecification.is == null) {
	  SelectedSpecification(specs.apply(0))
	  createSpec(SelectedSpecification.is)
  }
  
  specs.map(createSpecificationItem)
 }
 
 def statusChoice(): NodeSeq = {
  def selectStatus(status: String) : JsCmd = {
   SelectedSpecificationStatus(status)
   SelectedSpecification(null)
   RedirectTo("specification")
  }
  
  val stati = List(("all", S.?("all")), ("candidate", S.?("candidate")), ("approved", S.?("approved")), ("deprecated", S.?("deprecated")))
  SHtml.ajaxSelect(stati, Box(SelectedSpecificationStatus.is), st => selectStatus(st))
 }
 
 /**
  * methods required for specification creation
  */
 
 def createFrame(scorecardId: Long) = {
  val portrait = <fr orientation="portrait"></fr> % new UnprefixedAttribute("scorecardId", scorecardId.toString, Null)
  val landscape = <fr orientation="landscape"></fr> % new UnprefixedAttribute("scorecardId", scorecardId.toString, Null)
  val otherFrames = (Repository.read("scenario", SelectedScenario.is.id, "frames", "frames", -1) \\ "fr").toList
  Repository.write("scenario", SelectedScenario.is.id, "frames", "frames", -1, <frames>{MyUtil.flattenNodeSeq(portrait :: landscape :: otherFrames)}</frames>)
 }
 
 def addSpec() : JsCmd = {
  val newSpec = Specification.create
  newSpec.name("<new specification>").description("<description>").status("approved").implementationType("generic").fkScenario(SelectedScenario.is).dateCreated(new Date).save
  createFrame(newSpec.id)
  val displaySpecCommand = JsCmds.SetHtml("specifications", createSpec(newSpec))
  val updateTreeCommand = JsCmds.SetHtml("specificationTree", createSpecificationTree())
  CmdPair(displaySpecCommand, updateTreeCommand)
 }
 
 /**
  * Remove blocks or specifications, copy blocks
  */
 
  def serializeBlock(blockId: Long) = {
	 val b = <block blockId={blockId.toString}>
	 			<presentationType></presentationType>
	 			<presentationDetail></presentationDetail>
                <title></title>
	 			<structure></structure>
             </block>
	 
	 val otherBlocks = Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block"
	 
	 val nb = MyUtil.flattenNodeSeq(List(b, otherBlocks))
	 Repository.write("scenario", SelectedScenario.is.id, "blocks", "blocks", -1, <blocks>{nb}</blocks>)
 }
 
 def copyBlock(): JsCmd = {
  if(SelectedBlock.is != null && SelectedSpecification.is != null) {
   val bl = SelectedBlock.is
   val sp = SelectedSpecification.is
		
   val newBl = Block.create
   newBl.fkSpecification(sp.id).name(bl.name).dateCreated(new Date).save
   serializeBlock(newBl.id)
   JsCmds.SetHtml("specificationTree", createSpecificationTree()) 
  }
  else Alert(S.?("noBlockSpecificationChoice"))
 }
 
 def addBlock(): JsCmd = {
  if(SelectedSpecification.is != null) {
   val newBl = Block.create
   newBl.fkSpecification(SelectedSpecification.is).name("<block>").dateCreated(new Date).save
   serializeBlock(newBl.id)
   JsCmds.SetHtml("specificationTree", createSpecificationTree()) 
  }
  else Alert(S.?("noSpecificationChoice"))
 }
 
 def removeBlockOrSpec(isBlock: Boolean): JsCmd = {
  if(isBlock && SelectedBlock.is != null) {
	  val allBlocks = Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block"
	  val allBlocksWithoutSelected = allBlocks.filter(block => (block \ "@blockId").text != SelectedBlock.is.id.toString).toSeq
	  Repository.write("scenario", SelectedScenario.is.id, "blocks", "blocks", -1, <blocks>{allBlocksWithoutSelected}</blocks>)
	  Block.findAll(By(Block.id, SelectedBlock.is.id)).map(b => b.delete_!)
	  SelectedBlock(null)
  }
  
  if(!isBlock && SelectedSpecification.is != null) {
	  val allFrames = (Repository.read("scenario", SelectedScenario.is.id, "frames", "frames", -1) \\ "fr").toList
	  val allFramesWithoutSelected = allFrames.filter(frame => (frame \ "@scorecardId").text != SelectedSpecification.is.id.toString).toSeq
	  Repository.write("scenario", SelectedScenario.is.id, "frames", "frames", -1, <frames>{allFramesWithoutSelected}</frames>)
	  Specification.findAll(By(Specification.id, SelectedSpecification.is.id)).map(s => s.delete_!)
	  SelectedSpecification(null)
  }
  
  RedirectTo("/specification")
 }
 
 def findSerialization(xml: NodeSeq, b: Block): Node = {
  val selection = xml.filter(n => (n \\ "@blockId").text == b.id.toString)
  if(selection.isEmpty) <block /> else selection(0)
 }
 
 def selectBlock(b: Block) = {
  SelectedBlock(b)
  if(Comparisons.is == null) Comparisons(Repository.read("configuration", 0, "metadata", "model_metadata", -1) \\ "comparison")
  if(PresentationTypes.is == null) PresentationTypes(Repository.read("configuration", 0, "metadata", "model_metadata", -1) \ "presentationTypes" \ "presentationType")
  val blocksFromXml = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block").filter(bl => (bl \\ "@blockId").text == b.id.toString)
  if(!blocksFromXml.isEmpty) SelectedBlockInformation(blocksFromXml(0)) else SelectedBlockInformation(<block blockId="-1"></block>)
  println(" selected block information is " + SelectedBlockInformation.is.toString)
 }
 
 def measureListItem(m: Node): Node = <li>{MyUtil.getSeqHeadText(m)}</li>
 def saveBlockHeader(b:Block, text : String): JsCmd = {b.name(text).save; Noop }
 
 def measureRow(block: Block, mode: String, msrs: NodeSeq) = mode match {   
  case "display" => <tr><td>{block.name}</td><td>{S.?("measures")}<ul style="list-style-position:inside">{msrs.map(measureListItem).toSeq}</ul></td></tr>	    	     
  case "edit" => {
	 	  
	 val blockNameInput = SHtml.ajaxText(block.name, text => saveBlockHeader(block, text)) % new UnprefixedAttribute("size", "50", Null) % new UnprefixedAttribute("maxlength", "50", Null) 
	 
	 <tr>
	 	<td>{blockNameInput}</td>
	 	<td class="dropMeasure">{S.?("measures")}
	 	    <button class='standardButton upButton measureRelevant' style='float: right'>{S.?("up")}</button>
            <button class='standardButton downButton measureRelevant' style='float: right'>{S.?("down")}</button>
            <button class='standardButton removeButton measureRelevant' style='float: right'>{S.?("remove")}</button> 
	 		<ul style="list-style-position:inside">{msrs.map(measureListItem).toSeq}</ul>
	 	</td>
	 </tr> % new UnprefixedAttribute("blockId", block.id.toString, Null) 
  }
 }
 
 def attributeListItem(m: Node): Node = <li>{MyUtil.getSeqHeadText(m \\ "name")}</li>
 
 def readPresentationType(b: Block) = {
    val blocksFromXml = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block").filter(bl => (bl \\ "@blockId").text == b.id.toString)
    if(blocksFromXml.isEmpty) S.?("presentationType") 
    else {
    	val desc = S.?(MyUtil.getSeqHeadText(blocksFromXml(0) \\ "presentationDetail")) + " " + S.?(MyUtil.getSeqHeadText(blocksFromXml(0) \\ "presentationType"))
    	if(desc != null && desc.length > 1) desc else S.?("presentationType")
    }
 }
  
 def attributeRow(block: Block, mode: String, attrs: NodeSeq) = mode match {
   	 
   case "display" => {
	 <tr>
	   <td rowspan="2">{link("/block", () => selectBlock(block), <span>{readPresentationType(block)}</span>)}</td>
	   <td>{S.?("attributes")}<ul style="list-style-position:inside">{attrs.map(attributeListItem).toSeq}</ul></td>
     </tr>	    	  
   }
	   
   case "edit" => {
	 	  
	 <tr>
	 	<td rowspan="2">{link("/block", () => selectBlock(block), <span>{readPresentationType(block)}</span>)}</td>
	 	<td class="dropAttribute">{S.?("attributes")}
	 		<button class='standardButton upButton attributeRelevant' style='float: right'>{S.?("up")}</button>
	 		<button class='standardButton downButton attributeRelevant' style='float: right'>{S.?("down")}</button>
	 		<button class='standardButton removeButton attributeRelevant' style='float: right'>{S.?("remove")}</button>
	 		<ul style="list-style-position:inside">{attrs.map(measureListItem).toSeq}</ul>
	 	</td>
	 </tr> % new UnprefixedAttribute("blockId", block.id.toString, Null) 
   }
 }
 
 def refineFilterText(text: String): String = {
   def findIt(name: String ):String = {
	  val attrs = ModelVertex.findAll(By(ModelVertex.elementName, name), By(ModelVertex.elementType, "attribute"), By(ModelVertex.fkScenario, SelectedScenario.is.id))
	  if(!attrs.isEmpty) "<d" + attrs(0).id.toString + ">"
	  else {
		  val msrs = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id), By(Measure.shortName, name))
		  if(msrs.isEmpty) "" else "<m" + msrs(0).id.toString + ">"
	  }
   }
   
   val firstMatch = """\*.+\*""".r findFirstIn text
   
   firstMatch match {
	   case None => text
	   case Some(m) => {
	  	   val reference = findIt(m.substring(1, m.length - 1))
	  	   if(reference.length > 0) refineFilterText("""\*.+\*""".r replaceFirstIn(text, reference)) else text   
	   }
   }
 }
  
 def saveFilter(text: String, blockId: String): JsCmd = {
  if(text != null && text.length > 0) {
	  val parseResult = WikiParser.checkFilter(refineFilterText(text))
	  parseResult match {
			case (true, resultText) => Alert(resultText)
			case (false, resultText) => {
				val cmd = "changeBlockInformation(" + blockId + ", 'editFilter', \"" + resultText + "\", null);"
				JsRaw(cmd)
		}
	  }
  } else Noop
 }
  
 def filterRow(block: Block, mode: String, filterText: String) = mode match {
	  
   case "display" => {
	   <tr><td>{S.?("filter")}<br/><span>{MyUtil.prettyTerm(filterText, true)}</span></td></tr>
   }
	   
   case "edit" => {
	   val textArea = ajaxTextarea(MyUtil.prettyTerm(filterText, true), text => saveFilter(text, block.id.toString)) % ("rows" -> "4") % ("cols" -> "40") % ("class" -> "dropInFilter")
	   <tr><td>{S.?("filter")}<br />{textArea}</td></tr>
   }
 }
 
 def createSpec(spec : Specification) : Node = {
  
  def editIt = editSpec(spec)	 
  val edit = ajaxButton(S.?("edit"), editIt _) % ("class" -> "standardButton") % ("style" -> "float:right")
  val serializedBlocks = Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block"
   
  <div class="message">
  	<p class="messageTitle">
  		{spec.name}{edit}<br />
  		<span>{S.?("implementationType")}: {S.?(spec.implementationType)}</span><span style="float:right">{S.?("status")}: {S.?(spec.status)}</span><br />
        {S.?("description")} <br />
        <span>{spec.description}</span>
  	</p>
  	{Block.findAll(By(Block.fkSpecification, spec.id)).map(b => createBlock(spec.id.toString, b, findSerialization(serializedBlocks, b), "display")).toSeq}
  </div>  % new UnprefixedAttribute("id", "specBlock" + spec.id.toString, Null)
 }
 
 def createSpecification(): NodeSeq = if(SelectedSpecification.is != null) createSpec(SelectedSpecification.is) else NodeSeq.Empty
 
 def specification (xhtml: NodeSeq): NodeSeq = {
  def removeBlock() = removeBlockOrSpec(true)
  def removeSpecification() = removeBlockOrSpec(false)
   
  bind("spec", xhtml, "addSpecification"    -> ajaxButton(S.?("addSpecification"), addSpec _) % ("class" -> "standardButton"),
		  			  "removeSpecification"    -> ajaxButton(S.?("removeSpecification"), removeSpecification _) % ("class" -> "standardButton"),
		  			  "addBlock"    -> ajaxButton(S.?("addBlock"), addBlock _) % ("class" -> "standardButton"),
		  			  "copyBlock"    -> ajaxButton(S.?("copyBlock"), copyBlock _) % ("class" -> "standardButton"),
		  			  "removeBlock"    -> ajaxButton(S.?("removeBlock"), removeBlock _) % ("class" -> "standardButton"),
		  			  "tree" -> createSpecificationTree(),
		  			  "specification" -> createSpecification(),
		  			  "statusChoice" -> statusChoice())
 }
 
 def createBlock(spId: String, block : Block, serialization: Node, mode: String) : Node = {
  
  val measures = serialization \\ "measure"
  val attributes = serialization \\ "attribute"
  val filterText = MyUtil.getSeqHeadText(serialization \\ "filter")
  
  <div class="messageBlock block">
	<table class = "blockTable" >
	  <col width="34%" />
	  <col width="66%" />
	  {measureRow(block, mode, measures)}
	  {attributeRow(block, mode, attributes)}
	  {filterRow(block, mode, filterText)}
	</table>	 
  </div> % new UnprefixedAttribute("class", "messageBlock block blockOf" + spId, Null)
 }
 
 def selectStatus(specId: Long, status: String) : JsCmd = {
   val spec = Specification.findAll(By(Specification.id, specId)).apply(0)
   spec.status(status).save
   Noop
 }
 
 def createEditSpec(spec: Specification): Node = {
  
  def selectImplementation(specId: Long, implementationType: String) : JsCmd = {
   val spec = Specification.findAll(By(Specification.id, specId)).apply(0)
   spec.implementationType(implementationType).save
   editSpec(spec)
  } 
  
  case class AttributeContent(name: String, order: String)
  case class BlockContent(blockId: String, measures: List[String], attributes: List[AttributeContent], filter: String)
  
  def serializeStructure(blockString: String): List[(String, Node)] = {
   def asAttribute(ac: AttributeContent) = ac match {case AttributeContent(n, o) => <attribute><name>{n}</name><order>{o}</order></attribute>}
   
   def toStructure(bc: BlockContent) = bc match {
	case BlockContent(blockId, msrs, attrs, filter) => {
		(blockId, <structure><measures>{msrs.map(m => <measure>{m}</measure>)}</measures><attributes>{attrs.map(asAttribute)}</attributes><filter>{filter}</filter></structure>)
	}
   }
	  
   implicit val formats = net.liftweb.json.DefaultFormats
   
   try {
	   val blocks  = parse(blockString)	
	   println("ok blocks is " + blocks)
	   val blockContent = blocks.extract[List[BlockContent]]
	   println("ok blockContent is " + blockContent)
	   blockContent.map(toStructure).toList
   }
   catch {
	   case ex: Exception => {
	  	   println(ex.toString)
	  	   Nil
	   }
	   
   }
  }
  
  def finishIt(blockString: String): JsCmd = {
	 
   def changeTitle(b: Node, l: List[Block]) = {
	   val lReduced = l.filter(_.id == ((b \ "@blockId").text).toLong)
	   if(lReduced.isEmpty) b
	   else {
	  	   val transformTitle = "title" #> <title>{lReduced(0).name.toString}</title>
	  	   transformTitle(b).apply(0)
	   }
   }
   
   def changeStructure(b: Node, l: List[(String, Node)]) = {
	val lReduced = l.filter(_._1 == (b \ "@blockId").text)
	   if(lReduced.isEmpty) b
	   else {
	  	   val transformStructure = "structure" #> lReduced(0)._2
	  	   transformStructure(b).apply(0)
	   }
   }
   
   var blocksWithNewStructure:List[(String, Node)] = Nil
	  
   if(blockString != null && blockString.length > 0) blocksWithNewStructure  = serializeStructure(blockString)
   println("I finish " + blocksWithNewStructure.toString)
   val blocks = Block.findAll(By(Block.fkSpecification, SelectedSpecification.is.id)).toList
   val serializedBlocks = Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block"
   val result = serializedBlocks.map(b => changeTitle(b, blocks)).map(b => changeStructure(b, blocksWithNewStructure)).toSeq
   Repository.write("scenario", SelectedScenario.is.id, "blocks", "blocks", -1, <blocks>{result}</blocks>) 
   
   CmdPair(RedirectTo("/specification"), JsCmds.SetHtml("specifications", createSpec(SelectedSpecification.is)))
  }
  
  def prepareBlocks(): JsCmd = {
   val blockIds = Block.findAll(By(Block.fkSpecification, SelectedSpecification.is.id)).map(_.id.toString).toList
   val allBlocks = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block")
   val blocks = allBlocks.filter(bl => blockIds exists(id => id == (bl \ "@blockId").text)) //.filter(bl => (bl \ "@presentationType").text != null && (bl \ "@presentationType").text.length > 0)
   val frames = (Repository.read("scenario", SelectedScenario.is.id, "frames", "frames", -1) \\ "fr").filter(fr => (fr \ "@scorecardId").text ==  SelectedSpecification.is.id.toString)
   val succIds = blocks.map(b => ((b \ "@blockId").text, (b \ "@successorId").text)).filter(b => b._2 != null && b._2.length > 0)
   val successors = <successors>{succIds.map(id => <successor blockId={id._1} successorId={id._2}/>)}</successors>
   DesignBlockInformation(<design>{blocks}{frames}{successors}</design>)
   Noop
  }
  
  def showLinkToDesign(): Node = {
   if(spec.implementationType == "scorecard")  link("/design", () => prepareBlocks(), <span>Scorecard Design</span>) % new UnprefixedAttribute("id", "linkToDesign", Null)
   else <span />
  }
  
  val serializedBlocks = Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block"
  val stati = List(("candidate", S.?("candidate")), ("approved", S.?("approved")), ("deprecated", S.?("deprecated")))
  val selectStati = SHtml.ajaxSelect(stati, Box(spec.status), v => selectStatus(spec.id, v))
  val impls = List(("generic",S.?("generic")), ("scorecard", S.?("scorecard")), ("rest", S.?("rest")))
  val nameInput = SHtml.ajaxText(spec.name, text => saveSpecHeader(spec.id, "name", text)) % new UnprefixedAttribute("size", "50", Null) % new UnprefixedAttribute("maxlength", "50", Null) 
  val linkToDesign = showLinkToDesign()
  val selection = SHtml.ajaxSelect(impls, Box(spec.implementationType), v => selectImplementation(spec.id, v))
  val finish = <button class='standardButton'>{S.?("finish")}</button> % ("onclick" -> SHtml.ajaxCall(JsRaw("JSON.stringify($('#dropResults').data('blockInformation'))"), finishIt _)._2) 
  
  <div class="message">
  	<p class="messageTitle">{nameInput}{finish}<br /><label>{S.?("implementationType")}</label>{selection} {linkToDesign}<span style="float:right">Status: {selectStati}</span></p><br />
  	{SHtml.ajaxTextarea(spec.description, text => saveSpecHeader(spec.id, "description", text)) % ("rows" -> "10") % ("cols" -> "80")}
  	{Block.findAll(By(Block.fkSpecification, spec.id)).map( b => createBlock(spec.id.toString, b, findSerialization(serializedBlocks, b), "edit")).toSeq}
  </div>  % new UnprefixedAttribute("id", "specBlock" + spec.id.toString, Null)
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
  val dragMeasureCommand = JsRaw("$('.dragMeasure').draggable({helper: 'clone', opacity: 0.8});")
  val dropMeasureCommand = JsRaw("$('.dropMeasure').droppable({drop: function(event, ui) {$(this).children('ul').append(\"<li class='emphasizableMeasure'>\" + ui.draggable.text() + '</li>');changeBlockInformation($(this).parent().attr('blockId'), 'addMeasure', ui.draggable.text(), null)}});")
  val dragAttributeCommand = JsRaw("$('.dragAttribute').draggable({helper: 'clone', opacity: 0.8});")
  val dropAttributeCommand = JsRaw("$('.dropAttribute').droppable({drop: function(event, ui) {$(this).children('ul').append(\"<li class='emphasizableAttribute'>\" + ui.draggable.text() + \"<button orderFor='\" + ui.draggable.text() + \"' class='standardButton orderAttribute' style='float:right'>no order</button></li>\");changeBlockInformation($(this).parent().attr('blockId'), 'addAttribute', ui.draggable.text(), null)}});")
  val dropInFilterCommand = JsRaw("$('.dropInFilter').droppable({drop: function(event, ui) {appendValue($(this), '*' + ui.draggable.text() + '*');}});")
  val hideCommand = JsRaw("$('.tmpClosed').each(function(i) {$(this).removeClass('tmpClosed');$(this).hide();});")
  
  CmdPair(CmdPair(CmdPair(CmdPair(CmdPair(CmdPair(CmdPair(specCommand, treeCommand), dragMeasureCommand), dropMeasureCommand), dragAttributeCommand), dropAttributeCommand), dropInFilterCommand), hideCommand)
 }
 
 def saveSpecHeader(specId: Long, selectionKind: String, text : String) : JsCmd = {
  val spec = Specification.findAll(By(Specification.id, specId)).apply(0)
  
  selectionKind match { 
	 	  case "name" => spec.name(text)
	 	  case "description" => spec.description(text)
  }
	 
  spec.save	   
  Noop
 }
 	 
 def measureTree(): NodeSeq = {
  def measureLeaf(m: Measure) = <li class="treeItem"><span class="leaf">__</span><span class="emphasizable dragMeasure">{m.shortName}</span></li>
  val measures = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id), By(Measure.status, "approved"), OrderBy(Measure.subject, Ascending))
  val domains = measures.groupBy{_.subject}.map{_._2.head}.map(m => m.subject)
  
  val treeList = for(domain <- domains) yield {
	   val measuresOfDomain = measures.filter(_.subject == domain)
	  <li class="treeItem"><span class="handle closed">__</span><span class="emphasizable">{domain}</span><ul class="tmpClosed">{measuresOfDomain.map(measureLeaf).toSeq}</ul></li>
  }

  MyUtil.flattenNodeSeq(treeList.toList)
 }
	
 def createDimensionTreeItem(v: ModelVertex, level: String, modelElements: List[ModelVertex], hierarchies: List[List[(ModelVertex, Int)]]): NodeSeq = {
  val goDown = Map("dimension" -> "hierarchy", "hierarchy" -> "level", "level" -> "attribute")
  def createChildren(v: ModelVertex) = createDimensionTreeItem(v, goDown(level), modelElements, hierarchies)
  def findChildren(v: ModelVertex, l: List[ModelVertex], level: String, hierarchies: List[List[(ModelVertex, Int)]]): List[ModelVertex] = {
	  if(level == "dimension") List.flatten(hierarchies.map(chain => chain.filter(_._2 == 0).map(_._1)))
	  else if(level == "hierarchy") hierarchies.filter(chain => chain.filter(_._2 == 0).map(_._1).apply(0) == v).apply(0).filter(_._2 > 0).map(_._1)
	  else l.filter(l => ModelUtility.isConnected(l.id, v.id) && l.elementType == goDown(level))
  }
  
  if(level == "attribute") {
	  <li class="treeItem"><span class="leaf">__</span><span class="emphasizable dragAttribute">{v.elementName}</span></li> 
  }
  else {
	  val children = findChildren(v, modelElements, level, hierarchies) 
	  if(children.length > 0) <li class="treeItem"><span class="handle closed">__</span><span class="emphasizable">{v.elementName}</span><ul class="tmpClosed">{children.map(createChildren).toSeq}</ul></li>
	  else <li class="treeItem"><span class="leaf">__</span><span class="emphasizable">{v.elementName}</span></li>
  }
 }

 def dimensionTree(): NodeSeq = {
   val dimensions = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "dimension"), By(ModelVertex.elementKind, "original"), OrderBy(ModelVertex.elementName, Ascending))  
   
   val treeList = for (dimension <- dimensions) yield {
	   val modelElements = ModelVertex.findAll(By(ModelVertex.referenceId, dimension.id), OrderBy(ModelVertex.elementName, Ascending))
	   val hierarchies = ModelUtility.hierarchies(dimension.id)
	   createDimensionTreeItem(dimension, "dimension", modelElements, hierarchies)
   }
   
  	MyUtil.flattenNodeSeq(treeList.toList)
 }
 
 def prepareInit(blocks: NodeSeq): String = {
  def pAttr(attr: Node) = MyUtil.getSeqHeadText(attr \ "name") + ":" + MyUtil.getSeqHeadText(attr \ "order")
  def prAttr(attrs: NodeSeq) = if(attrs.isEmpty) "" else pAttr(attrs(0))
  def pBlock(bl: Node): String = (bl \ "@blockId").text + ";" + MyUtil.makeSeparatedList((bl \\ "measure").map(m => MyUtil.getNodeText(m)).toList,",") + ";" +
		                            MyUtil.makeSeparatedList((bl \\ "attribute").map(pAttr).toList, ",") + ";" + MyUtil.getSeqHeadText(bl \\ "filter")
		 
  JsonUtility.list2Json(blocks.map(pBlock).toList)	                                     
 }
 
 /**
  * Scorecard Design
  * 
  */
 
 def cancelDesign(): JsCmd = RedirectTo("/specification")
 
 def saveDesign(designData: String) = {
  def replaceFrame(frame: Node, referenceFrame: Node) = {
	  if((frame \\ "@scorecardId").text == (referenceFrame \\ "@scorecardId").text 
	 	 && (frame \\ "@orientation").text == (referenceFrame \\ "@orientation").text) referenceFrame else frame
  }
  
  def addSuccessor(block: Node, successors: NodeSeq) = {  
	  val someSuccessor = successors.filter(succ => (block \ "@blockId").text == (succ \ "@blockId").text)
	  if(someSuccessor.isEmpty) block
	  else {
	 	  val transform = "[successorId]" #>  (someSuccessor(0) \ "@successorId").text
	 	  transform(block)
	  }
  }
  
  val xml = XML.loadString(designData)
  val frames = xml \\ "fr"
  val successorIds = xml \\ "successor"
  
  if(frames.size == 2) {
	  val framesSoFar = Repository.read("scenario", SelectedScenario.is.id, "frames", "frames", -1) \\ "fr"
	  val framesFromNowOn = MyUtil.flattenNodeSeq(framesSoFar.toList.map(fr => replaceFrame(fr, frames.apply(0))).map(fr => replaceFrame(fr, frames.apply(1))))
	  Repository.write("scenario", SelectedScenario.is.id, "frames", "frames", -1, <frames>{framesFromNowOn}</frames>) 
  }
  
  val blocks = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block").map(b => addSuccessor(b, successorIds)).toSeq
  Repository.write("scenario", SelectedScenario.is.id, "blocks", "blocks", -1, <blocks>{blocks}</blocks>)
  
  RedirectTo("/specification")
 }
 
 def rotateDesign(): JsCmd = {
  JsRaw("rotateDesign()");
 }
 
 def clean(): JsCmd = {
  JsRaw("clean()");
 }
 
 def design (xhtml: NodeSeq): NodeSeq = {
  bind("scorecard", xhtml, "cancel" -> ajaxButton(S.?("cancel"), cancelDesign _) % ("class" -> "standardButton"), 
		                   "turn" -> ajaxButton(S.?("rotate"), rotateDesign _) % ("class" -> "standardButton"),
		                   "save" -> <button class='standardButton'>{S.?("save")}</button> % ("onclick" -> SHtml.ajaxCall(JsRaw("$('#designBlockInformation').html()"), saveDesign _)._2),
		                   "clean" -> ajaxButton(S.?("clean"), clean _) % ("class" -> "standardButton"),
		                   "data" -> DesignBlockInformation.is)
 }
 
 /**
  * Choice of presentation type
  */
 
 def ruleBody(rule: Node) = {
   <td>
   	<div presentationType={MyUtil.getNodeText((rule \\ "presentationType").apply(0))} presentationDetail={MyUtil.getNodeText((rule \\ "presentationDetail").apply(0))} class="presentationThumbnail"  id = {"rule" + MyUtil.getNodeText((rule \\ "ruleId").apply(0))}/>
   </td>
 }
  
 def ruleHeader(rule: Node) = <td>{MyUtil.getNodeText((rule \\ "ruleDescription").apply(0))}</td>
  
 def toTable(comp: Node) = {
   def addMissingCell(cells: NodeSeq) = if(cells.size == 1) MyUtil.flattenNodeSeq(cells.toList ::: List(<td></td>)) else cells
   val compName = MyUtil.getNodeText((comp \\ "comparisonName").apply(0))
   val compDesc = MyUtil.getNodeText((comp \\ "description").apply(0))
   
   val rules = (comp \\ "rule").map(node => ruleBody(node)).toSeq
   val header = (comp \\ "rule").map(node => ruleHeader(node)).toSeq
   
   <table class="protocolTable">
   	<col width="80" text-align="left"/>
	<col width="270" text-align="left"/>
    <col width="85" text-align="left"/>
    <col width="85" text-align="left"/>
    <thead>
   		<tr><td>{S.?("rule")}</td><td></td>{addMissingCell(header)}</tr>
    </thead>
    <tbody>
    	<tr><td>{compName}</td><td>{compDesc}</td>{addMissingCell(rules)}</tr>
    </tbody>
   </table>
 }	
 
 def allComparisons(): Node = {
  val comparisons = Comparisons.is.filter(comp => (comp \ "@group").text == "comparison")
  <div>{comparisons.map(toTable).toSeq}</div>
 }
 
 def tableAndIndicator() = {

   <table class="protocolTable">
    <col width="85" text-align="left"/>
    <col width="85" text-align="left"/>
    <thead>
   		<tr><td>Kind</td><td></td></tr>
    </thead>
    <tbody>
    	<tr><td>Table</td><td class="presentationThumbnail"  id="tableThumbnail" presentationType="table" presentationDetail="plain"></td></tr>
	    <tr><td>Status Indicator</td><td class="presentationThumbnail" id="statusIndicatorThumbnail" presentationType="statusIndicator" presentationDetail="circle"></td></tr>
	    <tr><td>Trend Indicator</td><td class="presentationThumbnail" id="trendIndicatorThumbmail" presentationType="trendIndicator" presentationDetail="arrow"></td></tr>
	 	<tr><td>Plain Presentation</td><td class="presentationThumbnail" id="plainThumbmail" presentationType="plain" presentationDetail="plain"></td></tr>
    </tbody>
   </table>
 }
 
 def presentationDetails() = {
	 
  def detailRow(presentationType: String, presentationDetail: String, description: String) = {
	  val cell = <td class="detailThumbnail" selected="n"></td> % new UnprefixedAttribute("id", "detailThumb" + presentationType + presentationDetail, Null) % new UnprefixedAttribute("presentationType", presentationType, Null) % new UnprefixedAttribute("presentationDetail", presentationDetail, Null)
	  <tr class="detailThumbnailRow"><td>{description}</td>{cell}</tr> % new UnprefixedAttribute("presentationType", presentationType, Null) % new UnprefixedAttribute("presentationDetail", presentationDetail, Null) 
  }
  
  def txt(n: Node, tag: String) = MyUtil.getSeqHeadText(n \\ tag)
  
  val details = Comparisons.is \\ "presentationTypes" \\ "presentationType"
	  
   <table class="protocolTable">
    <col width="85" text-align="left"/>
    <col width="85" text-align="left"/>
    <thead>
   		<tr><td>usage</td><td></td></tr>
    </thead>
    <tbody>
    	{PresentationTypes.is.map(pt => detailRow(txt(pt, "kind"), txt(pt, "detail"), txt(pt, "usage"))).toSeq}
    </tbody>
   </table>
 }
 
 def cancelBlockChoice(): JsCmd = RedirectTo("/specification")
 
 def saveBlockChoice(xml: String): JsCmd = {
  def replaceBlock(block: Node, reference: Node) = {
	  if((block \ "@blockId").text == (reference \ "@blockId").text) {
	 	  val str =  block \\ "structure"
	 	  val structure = if(str.isEmpty) <structure /> else str.apply(0)
	 	  val transform = "structure" #> structure
	 	  transform(reference) 
	  }
	   else block
  }
  println(xml)
  val block = XML.loadString(xml)
  val blocks = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block").map(b => replaceBlock(b, block)).toSeq
  Repository.write("scenario", SelectedScenario.is.id, "blocks", "blocks", -1, <blocks>{blocks}</blocks>)
  RedirectTo("/specification")
 }
 
 def additionalAttributeRow(addAttr: (String, String), presentationType: String, presentationDetail: String) = {
	 
	 <tr class='additionalAttributeRow' >
	   	<td><label>{addAttr._1}</label></td>
	    <td><input type="text" class="blockAttributeInput" inputFor={addAttr._1} /></td>
	 </tr> % new UnprefixedAttribute("presentationType", presentationType, Null) % new UnprefixedAttribute("presentationDetail", presentationDetail, Null)
 }
    
 def additionalAttributes(pt: Node) = {
	 val presentationType = MyUtil.getSeqHeadText(pt \ "kind")
	 val presentationDetail = MyUtil.getSeqHeadText(pt \ "detail")
	 val attributeNames = (pt \ "additionalAttributes" \ "attributeName").map(x => MyUtil.getNodeText(x)).toList
	 val attributePattern = (pt \ "additionalAttributes" \ "attributePattern").map(x => MyUtil.getNodeText(x)).toList
	 val addAttrs = attributeNames zip attributePattern
	 
	 <div class="mohhowFormLight additionalAttributeForm"><table>{addAttrs.map(attr => additionalAttributeRow(attr, presentationType, presentationDetail))}</table></div> % new UnprefixedAttribute("presentationType", presentationType, Null) % new UnprefixedAttribute("presentationDetail", presentationDetail, Null)
 }
 
 def blockSpecification (xhtml: NodeSeq): NodeSeq = {
  bind("block", xhtml, "cancel" -> ajaxButton(S.?("cancel"), cancelBlockChoice _) % ("class" -> "standardButton"), 
		               "initialization" -> <div id="readBlockFirstTime">{SelectedBlockInformation.is}</div>,
		               "comparison" -> allComparisons(),
		               "tableIndicator" -> tableAndIndicator(),
		               "presentationDetails" -> presentationDetails(),
		               "additionalAttributes" -> PresentationTypes.is.map(additionalAttributes),
		               "save" -> <button class='standardButton'>{S.?("save")}</button> % ("onclick" -> SHtml.ajaxCall(JsRaw("$('#serializedBlock').data('serializedBlock')"), saveBlockChoice _)._2))
 }
}