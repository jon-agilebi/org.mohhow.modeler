package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import net.liftweb.ldap._
import http._
import SHtml._
import S._
import common._

import js._
import JsCmds._
import JE.{JsRaw,Str}

import mapper._
import util._
import Helpers._

import scala.collection.mutable._

import scala.xml._
import org.mohhow.bi.lib.Repository;
import org.mohhow.bi.lib.ModelUtility;
import org.mohhow.bi.lib.BIService;
import org.mohhow.bi.lib.ConnectionInformation;
import org.mohhow.bi.lib.PDFUtility
import org.mohhow.bi.lib.WikiParser
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}

import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import net.liftweb.mapper._
import _root_.java.sql.{Connection, DriverManager}

object SelectedRelease extends SessionVar[Release](null)
object SelectedSystem extends SessionVar[String](null)
object SelectedAlias extends SessionVar[String](null)
object ReleaseForDeployment extends SessionVar[Release](null)
object TestDataNode extends SessionVar[Node](null)
object TestDataModel extends SessionVar[List[(PTable, ModelVertex)]](Nil)

class ReleaseSnippet {
	
 def selectRelease(id : Long) = {
  val release = Release.findAll(By(Release.id, id)).apply(0)
  SelectedRelease(release)
 } 
	
 def createListItem(release : Release) = {
  val name = S.?("release") + " " + release.prettyNumber
  val myId = release.id.is
  val item = link("/release", () => selectRelease(myId), <span>{name}</span>)
  <li class='listItem'>{item}</li>
 }
 
 def showStatus(): NodeSeq = {
   val release = SelectedRelease.is

   if(release == null) <nothing />
   else {
	   <div>
	   	<label>{S.?("releaseKind")}: </label>{release.kind} <br/>
	   	<label>{S.?("status")}: </label>{release.status} <br/>
	   	<label>{S.?("begin")}: </label>{MyUtil.formatDate(release.begin)} <br/>
	   	<label>{S.?("scheduledEnd")}: </label>{MyUtil.formatDate(release.scheduledEnd)} <br/>
	   	<label>{S.?("announcedEnd")}: </label>{MyUtil.formatDate(release.announcementDate)} <br/>
	   	<label>{S.?("freezeDate")}: </label>{MyUtil.formatDate(release.end)} <br/>
	   </div>
   }
 }
 
 def showArtefact(text: String) = Alert(Repository.getArtefactAsString(SelectedRelease.is.id, text))
 
 def artefacts(): NodeSeq = {
  
  def ddls(r: Release) = {
   val ddlsOfRelease = Repository.getArtefactList(r.id, List("sql"), false)
   val action = SHtml.ajaxCall(JsRaw("$(this).text()"), showArtefact _)._2
   if(ddlsOfRelease.isEmpty) NodeSeq.Empty 
   else <ul style="list-style-position:inside">{ddlsOfRelease.map(ddl => <li>{ddl}</li> % ("onclick" -> action))}</ul>
  }

  def metadata(r: Release, kind: String) = {
   val artefact = Repository.getArtefactList(r.id, List(kind), false)
   val action = SHtml.ajaxCall(JsRaw("$(this).text()"), showArtefact _)._2
   val docLink = "/documentation/" + r.id.toString
   if(artefact.isEmpty) NodeSeq.Empty 
   else if(kind == "pdf") <a href={docLink}>documentation.pdf</a>
   else <span>{artefact}</span> % ("onclick" -> action)
  }
   
  val release = SelectedRelease.is

  if(release == null) <nothing />
  else {
	   <div>
	     <h4>{S.?("documentation")}</h4>
	   		<br />
	  			{metadata(release, "pdf")}
	  		<br /><br />
	   	 <h4>{S.?("metadata")}</h4>
	   		<br />
	  			{metadata(release, "xml")}
	  		<br /><br />
	   	<h4>DDL</h4>
	  		<br />
	  			{ddls(release)}
	  		<br />
	   </div>
  }
 }
 
 def deltaDDL(r: Release) = {
  val deltasOfRelease = Repository.getArtefactList(r.id, List("sql"), true)
  val action = SHtml.ajaxCall(JsRaw("$(this).text()"), showArtefact _)._2
  if(deltasOfRelease.isEmpty) NodeSeq.Empty 
  else <ul style="list-style-position:inside">{deltasOfRelease.map(ddl => <li>{ddl}</li> % ("onclick" -> action))}</ul>
 }
 
 def delta(): NodeSeq = {
   val release = SelectedRelease.is

   if(release == null) <nothing />
   else {
	   <div>
	   	<br />
	   	<h4>DDL</h4>
	   		<br />
	   			{deltaDDL(release)}
	  		<br />
	   </div>
   }
 }
 
 def showRelease(release: Release): Node = {
	 <div>
		 <h4>Release {release.prettyNumber}</h4>
	   	<label>{S.?("releaseKind")}: </label>{release.kind} <br/>
	   	<label>{S.?("status")}: </label>{release.status} <br/>
	   	<label>{S.?("begin")}: </label>{MyUtil.formatDate(release.begin)} <br/>
	   	<label>{S.?("scheduledEnd")}: </label>{MyUtil.formatDate(release.scheduledEnd)} <br/>
	   	<label>{S.?("end")}: </label>{MyUtil.formatDate(release.end)} <br/><br />
	 </div>
 }
 
 def overview (xhtml: NodeSeq): NodeSeq = {
  bind("release", xhtml, "all"  -> Release.findAll(By(Release.fkScenario, SelectedScenario.is.id), OrderBy(Release.scheduledEnd, Ascending)).map(showRelease).toSeq)
 }
 
 def createNewRelease(): JsCmd = {
  val newRelease = Release.create
  newRelease.creationDate(new Date).begin(new Date).fkScenario(SelectedScenario.is)
  SelectedRelease(newRelease)
  RedirectTo("/releaseEdit")
 }
 
 def changeReleaseStatus(targetStatus: String): JsCmd = {
  val release = SelectedRelease.is
  if(release != null) {
	  if(targetStatus == "announced") {
	 	  if(release.status != "active"){
	 	 	  Alert(S.?("announcementRestriction"))
	 	  }
	 	  else {
	 		  release.status("announced").announcementDate(new Date).save
	 		  RedirectTo("/release")
	 	  }
	  }
	  else if(targetStatus == "freezed")
	  {
	 	  if(release.status != "announced"){
	 	 	  Alert(S.?("freezeRestriction"))
	 	  }
	 	  else {
	 	 	  release.status("freezed").freezeDate(new Date).save
	 	 	  RedirectTo("/release")
	 	  }
	  }
	  else Noop
  }
  else Alert(S.?("noReleaseChosen"))
 }
 
 def freeze = changeReleaseStatus("freezed")
 def announce = changeReleaseStatus("announced")
 
 /**
  * all methods for release creation start here
  */
 
 def commaList(l:List[String]): String = MyUtil.makeSeparatedList(l,",\n")
 
 def createTypeDetail(length: Long, precision: Long) = {
  if(length > 0 && precision > 0) "(" + length.toString + ", " + precision.toString + ")"
  else if (length > 0)  "(" + length.toString + ")"
  else ""
 }
  
 def notNullable(flag: Long) = if(flag > 0) " NOT NULL" else ""
	  
 def ref(refId: Long) = {
   if(refId > 0) {
	   val attr = PAttribute.findAll(By(PAttribute.id, refId)).apply(0)
	   val table = PTable.findAll(By(PTable.id, attr.fkPTable)).apply(0)
	   " REFERENCES(" + table.name + "." + attr.name + ")" 
   }
   else ""
 }
 
 def prefix(isPrimaryConstraint: Boolean) = if(isPrimaryConstraint)  "PK_" else " UQ_"
  
 def keyConstraint(table: PTable, isPrimaryConstraint: Boolean) = { 
  def attrList(isPrimaryConstraint: Boolean) = {
	   if(isPrimaryConstraint) PAttribute.findAll(By(PAttribute.fkPTable, table.id)).filter(_.isPrimaryKey.toLong == 1).map(a => a.name.toString)
	   else PAttribute.findAll(By(PAttribute.fkPTable, table.id)).filter(_.isPartOfUniqueKey.toLong == 1).map(a => a.name.toString)
  }
   
  def keyword(isPrimaryConstraint: Boolean) = if(isPrimaryConstraint)  " PRIMARY KEY (" else " UNIQUE ("
 
  val partsOfConstraint = attrList(isPrimaryConstraint)
  if(partsOfConstraint.isEmpty) "" 
  else {
	  val constraintList = commaList(partsOfConstraint)
	  
	  "ALTER TABLE " + table.name + " ADD (CONSTRAINT " + prefix(isPrimaryConstraint) + table.name + keyword(isPrimaryConstraint) + constraintList + "));"
  }
 }
  
 def createLine(attr: PAttribute): String = {
   attr.name + " " + attr.dataType + createTypeDetail(attr.length.toLong, attr.scale.toLong) + notNullable(attr.isNotNullable.toLong) + ref(attr.reference)
 }
 
 def createAlter(table: PTable, actualRelease: Release, previousRelease: Release) = {
  def valid(attr: PAttribute, r: Release): Boolean = {
	 MyUtil.dateAsNumber(attr.validFrom) <= MyUtil.dateAsNumber(r.end) && MyUtil.dateAsNumber(attr.validUntil) > MyUtil.dateAsNumber(r.end)
  }
	 
  val allAttributes = PAttribute.findAll(By(PAttribute.fkPTable, table.id))
  val changedAttributes = allAttributes.filter(attr => attr.isCurrent == 1 && MyUtil.dateAsNumber(attr.validFrom) > MyUtil.dateAsNumber(previousRelease.end))
  val droppedAttributes = allAttributes.filter(attr => attr.isCurrent == 0 && MyUtil.dateAsNumber(attr.validUntil) > MyUtil.dateAsNumber(previousRelease.end))
  val newOrChangedAttributes = changedAttributes.partition(attr => !droppedAttributes.map(_.name).exists(_ == attr.name))
  val attributesToDrop = droppedAttributes.filter(attr => !newOrChangedAttributes._1.map(_.name).exists(_ == attr.name))
	 
  val currentUq = allAttributes.filter(attr => attr.isPartOfUniqueKey == 1 && attr.isCurrent == 1).map(_.name.toString).sort(_ < _)
  val previousUq = allAttributes.filter(attr => attr.isPartOfUniqueKey == 1 && valid(attr, previousRelease)).map(_.name.toString).sort(_ < _)

  val currentPk = allAttributes.filter(attr => attr.isPrimaryKey == 1 && attr.isCurrent == 1).map(_.name.toString).sort(_ < _)
  val previousPk = allAttributes.filter(attr => attr.isPrimaryKey == 1 && valid(attr, previousRelease)).map(_.name.toString).sort(_ < _)
	
  val addColumns = if(newOrChangedAttributes._1.isEmpty) "" else {
	  "ALTER TABLE " + table.name + " ADD (" + MyUtil.makeSeparatedList(newOrChangedAttributes._1.map(createLine),",\n") + ")";
  }
  
  val dropColumns = if(attributesToDrop.isEmpty) "" else {
	  MyUtil.makeSeparatedList(attributesToDrop.map(attr => "ALTER TABLE " + table.name + " DROP COLUMN (" +  attr.name + ");"), "\n")
  }
  
  val alterColumns = if(newOrChangedAttributes._2.isEmpty) "" else {
	  MyUtil.makeSeparatedList(newOrChangedAttributes._2.map(attr => "ALTER TABLE " + table.name + " ALTER COLUMN (" +  createLine(attr) + ");"), "\n")
  }
  
  val alterPk = if(currentPk == previousPk)  "" else {
	  "ALTER TABLE " + table.name + " DROP CONSTRAINT " + prefix(true) + table.name + ";\n" + keyConstraint(table, true)
  }
  
  val alterUq = if(currentUq == previousUq)  "" else {
	  "ALTER TABLE " + table.name + " DROP CONSTRAINT " + prefix(false) + table.name + ";\n" + keyConstraint(table, false)
  }
  
  val allCommands = addColumns + dropColumns + alterColumns + alterPk + alterUq
  if(allCommands.trim.length > 0) Repository.writeDDL(SelectedRelease.is.id, "alter_" + table.name, allCommands.trim, true)
 }
 
 def createDrops(actualRelease: Release, previousRelease: Option[Release]) = previousRelease match {
	 case None => { } // nothing to drop
	 case Some(r) => {
		val toDrop = PTable.findAll(By(PTable.isCurrent, 0)).filter(t => MyUtil.dateAsNumber(t.validUntil) > MyUtil.dateAsNumber(r.end) && MyUtil.dateAsNumber(t.validFrom) <= MyUtil.dateAsNumber(r.end))
		if(!toDrop.isEmpty) {
			val dropCommands = MyUtil.makeSeparatedList(toDrop.map(t => "DROP TABLE " + t.name + ";").toList, "\n")
			Repository.writeDDL(SelectedRelease.is.id, "drop_tables", dropCommands, true)
		}
	 }
 }
 
 def createDDL(table: PTable, extraViewLayer: Boolean, stableViewLayer: Boolean, previousRelease: Option[Release]) = {
  
  val attributeList = PAttribute.findAll(By(PAttribute.fkPTable, table.id), By(PAttribute.isCurrent, 1))
  val rows = commaList(attributeList.map(createLine))
  val completeDDL = "CREATE TABLE " + table.name + "(" + rows + ");\n\n" + keyConstraint(table, true) + "\n\n" + keyConstraint(table, false)
  val commaAttributes = commaList(attributeList.map(_.name.toString))
  val simpleView = "CREATE OR REPLACE VIEW V_" + table.name + "(" + commaAttributes + ") AS SELECT (" + commaAttributes + " FROM " + table.name + ");" 
  
  Repository.writeDDL(SelectedRelease.is.id, table.name, completeDDL, false)
  
  previousRelease match {
	  case None => {
	 	  Repository.writeDDL(SelectedRelease.is.id, table.name, completeDDL, true)
	 	  if(extraViewLayer) {
	 	 	  Repository.writeDDL(SelectedRelease.is.id, "V_" + table.name, simpleView, true)
	 	 	  Repository.writeDDL(SelectedRelease.is.id, table.name, simpleView, false)
	 	  }
	  }
	  case Some(r) => {
	 	  if(MyUtil.dateAsNumber(table.validFrom) > MyUtil.dateAsNumber(r.end)) Repository.writeDDL(SelectedRelease.is.id, table.name, completeDDL, true)
	 	  else createAlter(table, SelectedRelease.is, r) 
	 	  
	 	  if(extraViewLayer) {
	 	 	if(stableViewLayer) {
	 	 	  Repository.writeDDL(SelectedRelease.is.id, "V_" + table.name, simpleView, true)
	 	 	  Repository.writeDDL(SelectedRelease.is.id, "V_" + table.name, simpleView, false)
	 	 	}
	 	 	else {
	 	 	  Repository.writeDDL(SelectedRelease.is.id, "V_" + table.name, simpleView, true)
	 	 	  Repository.writeDDL(SelectedRelease.is.id, "V_" + table.name, simpleView, false) 
	 	 	}
	 	  }
	  }
  }
 }
 
 // SELECT Statement generation for each block
 
 def findAttribute(name: String): ModelVertex = {
  val attrs = ModelVertex.findAll(By(ModelVertex.elementType, "attribute"), By(ModelVertex.elementName, name), By(ModelVertex.fkScenario, SelectedScenario.is.id))	  
  if(attrs.isEmpty) null else attrs(0)
 }
 
 def createSelects(bl: Node, isAccountModel: Boolean): Node = {
  def findBlockType(presentationType: String): String = presentationType match {
	  case "indicator" => "zero"
	  case "table" => "grid"
	  case "text" => "text"
	  case "total" => "text"
	  case _ => "one"
  }
  
  def findMeasure(name: String): Option[Measure] = {
   val msrs = Measure.findAll(By(Measure.shortName, name), By(Measure.fkScenario, SelectedScenario.is.id))
   if(msrs.isEmpty) None else Some(msrs.apply(0))
  }
  
  def findMeasureIfExists(text: Option[String]): Option[Measure] = text match {
	  case None => None
	  case Some(id) => {
	 	val msrs = Measure.findAll(By(Measure.id, id.toLong))
	 	if(msrs.isEmpty) None else Some(msrs(0))
	  }
  }
  
  def niceMeasures(l : List[Option[Measure]]): List[Measure] = l match {
	  case Nil => Nil
	  case h :: tail => {
	 	  h match {
	 	 	  case Some(m) => m :: niceMeasures(tail)
	 	 	  case _ => niceMeasures(tail)
	 	  }
	  }
  }
  
  def getRelevantMeasures(maybeMeasure: Option[Measure]): List[Measure] = maybeMeasure match {
	  case None => Nil
	  case Some(m) => {
	 	  if(m.formula == null || m.formula.toString.length == 0) List(m)
	 	  else {
	 	 	  val pattern = """<m\d+>""".r
	 	 	  val digits = """\d+""".r
	 	 	  val candidates = (pattern findAllIn m.formula.toString).map(x => findMeasureIfExists(digits findFirstIn x))
	 	 	  niceMeasures(candidates.toList)
	 	  }
	  }
  }
  
  def translateFilter(filterText: String): String = {
   val genericPattern = """<[m|d]\d+>""".r
   val digits = """\d+""".r
   
   genericPattern findFirstIn filterText match {
	   case None => filterText
	   case Some(generic) => {
	  	   
	  	  digits findFirstIn generic match {
	  	  	  	case Some(anId) => {
	  	  	  		
	  	  	  		val isAttribute = anId.substring(1,2) == "d"
	  	  	  	  	findPhysics(anId.toLong, isAttribute) match {
	  	  	  	  	  	  	   case (Some(field), Some(table)) => translateFilter(genericPattern replaceFirstIn(filterText, table + "." + field))
	  	  	  	  	  	  	   case _ => filterText
	  	  	  	  	}
	  	  	  	}
	  	  	  	  	     
	  	  	  	case _ => filterText  
	  	  }
	   }
	     
	   case _ => filterText    	   
   }
  }
  
  def findPhysics(id: Long, isAttribute: Boolean) = {
   def findAttribute(id: Long, isAttribute: Boolean) = {
	   if(isAttribute) {
		   	val attrs = PAttribute.findAll(By(PAttribute.fkModelAttribute, id))
		   	if(!attrs.isEmpty) Some(attrs(0)) else None
	   }
	   else {
	  	   	val msrs = PAttribute.findAll(By(PAttribute.fkMeasure, id))
		   	if(!msrs.isEmpty) Some(msrs(0)) else None
	   }
   }
   
   val maybeAttr = findAttribute(id, isAttribute)
   maybeAttr match {
	   case Some(attr) => (maybeAttr, Some(PTable.findAll(By(PTable.id, attr.fkPTable)).apply(0)))
	   case None => (None, None)
   }
  }
  
  def setOrder(orderCode: String): String = orderCode match {
	  case "0" => ""
	  case "1" => "ASCENDING"
	  case _ => "DESCENDING"
  }
  
  def sql(fact: Option[PTable], msrs: List[Measure], attrs: List[(ModelVertex, String)], filter: String): Node = {
   def tName(t: Option[PTable]): String = t match {
	   case None => ""
	   case Some(pt) => pt.name.toString
   }
   
   def aName(a: Option[PAttribute]): String = a match {
	   case None => ""
	   case Some(attr) => attr.name.toString
   }
   
   def anAttribute(a: Option[PAttribute]): PAttribute = a match {
	   case None => null
	   case Some(attr) => attr
   }
   
   def aTable(t: Option[PTable]): PTable = t match {
	   case None => null
	   case Some(pt) => pt
   }
   
   val attrXML = attrs.map(attr => <column><kind>attribute</kind><measureId>-1</measureId><type></type></column>).toSeq  
   val msrsOfFT = msrs.filter(m => findPhysics(m.id, false)._2  == fact)
   val msrXML = msrsOfFT.map(m => <column><kind>measure</kind><measureId>{m.id.toString}</measureId><type></type></column>).toSeq
   val augmentedAttrs = attrs.map(a => (a._1, findPhysics(a._1.id, true), a._2)).map(item => (anAttribute(item._2._1), tName(item._2._2), aName(item._2._1), item._3))
   val augmentedMsrs = msrs.map(m => (m, findPhysics(m.id, false))).map(item => (item._1, tName(item._2._2), aName(item._2._1)))
	  
   <select>
		  {attrXML}
		  {msrXML}
		  <sql></sql>
   </select>
  }
  //{ModelUtility.createSelect(augmentedAttrs, augmentedMsrs, aTable(fact), filter)}</sql> 
  def aMeasure(m: Option[Measure]): Measure = m match {
	   case None => null
	   case Some(msr) => msr
  }
  
  def refineFormula(formula: String): String = {
	  
	  def m(mr: MeasureRange, isMeaning: Boolean): String = if(isMeaning) mr.meaning else mr.rangeValue.toString
	  
	  if(formula.startsWith("meaning") || formula.startsWith("range") || formula.startsWith("plain")) {
	 	  
	 	 val pattern = """\d+""".r  
		 val firstMatch = pattern findFirstIn formula
		 
		 firstMatch match {
		 	case None => formula
		 	case Some(digits) => {
		 		
		 		if(formula.startsWith("meaning") || formula.startsWith("range")) {
		 				
		 			val ranges = MeasureRange.findAll(By(MeasureRange.fkMeasure, digits.toLong), OrderBy(MeasureRange.lowerBound, Ascending))
		 			val rangeString = ("" /: ranges.map(mr => mr.lowerBound.toString + "_" + mr.upperBound.toString + "_" + m(mr, formula.startsWith("meaning")))) (_ + ";" + _)
		 			formula.substring(1, formula.length - 1) + ", " + rangeString + ")"
		 		}
		 		else {
		 			val msrs = Measure.findAll(By(Measure.id,digits.toLong))
		 			if(msrs.isEmpty) formula else formula.substring(1, formula.length - 1) + ", " + msrs(0).unit + ")"
		 		}
		 	}
	 	 }  
	  }
	  else formula
  }
  
  val blockId = (bl \ "@blockId").text
  val blockType = MyUtil.getSeqHeadText(bl \ "presentationType") 
  val filter = translateFilter("""&gt;""".r replaceAllIn("""&lt;""".r replaceAllIn(MyUtil.getSeqHeadText(bl \ "filter"), "<"), ">"))
  val attrs = (bl \\ "attribute").map(a => (findAttribute(MyUtil.getSeqHeadText(a \ "name")), setOrder(MyUtil.getSeqHeadText(a \ "order")))).filter(_._1 != null)
  val msrs = (bl \\ "measure").map(m => findMeasure(MyUtil.getNodeText(m)))
  val relevantMeasures = List.flatten(msrs.toList.map(getRelevantMeasures)).toList
  val factTables = relevantMeasures.map(msr => findPhysics(msr.id, false)).map(_._2).distinct
  
  val attrXML = attrs.map(attr => <attribute><id>{attr._1.id.toString}</id><order>{attr._2}</order><emphasize></emphasize></attribute>).toSeq
  val msrXML = relevantMeasures.map(msr => <measure><id>{msr.id.toString}</id><formula>{refineFormula(msr.formula.toString.trim)}</formula></measure>).toSeq
  val sqlXML = factTables.map(fact => sql(fact, msrs.map(aMeasure).toList, attrs.toList, filter))
  val grid = if(blockType == "grid") {
	  val check = checkGrid(MyUtil.getSeqHeadText(bl \ "horizontalSpan"), MyUtil.getSeqHeadText(bl \ "verticalSpan"), attrs.map(_._1).toList)
	  <grid><horizontalSpan>{check._2}</horizontalSpan><verticalSpan>{check._2}</verticalSpan></grid>
  }
  else NodeSeq.Empty
  
  <block id={blockId}>
		<blockType>{blockType}</blockType>
        {grid}
        <structure>
			{attrXML}
            {msrXML}
        </structure>
		<selects>
            {sqlXML}
		</selects>
  </block> 
 }
 
 def checkGrid(horizontalName: String, verticalName: String, attributes: List[ModelVertex]): (Boolean, String, String) = {
  def findAttribute(name: String) = {
	  ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "attribute"), By(ModelVertex.elementName, name)).toList
  }
  
  def eval(l: List[ModelVertex]) = if(l.size == 0) "noAttribute" else if (l.size > 1) "attributeNotUnique" else ""
  def serialize(l: List[ModelVertex]) = MyUtil.makeSeparatedList(l.map(_.id.toString), ";")
	 
  val horizontal =  findAttribute(horizontalName)
  val vertical = findAttribute(verticalName)
  
  if(horizontal.size != 1 || vertical.size != 1) (false, eval(horizontal), eval(vertical))
  else {
	  val horizontalAttrs = ModelUtility.attributesAbove(horizontal(0))
	  val verticalAttrs = ModelUtility.attributesAbove(vertical(0))
	  val hMatch = attributes.intersect(horizontalAttrs)
	  val vMatch = attributes.intersect(verticalAttrs)
	  
	  if(attributes.diff(hMatch ::: vMatch).size > 0) (false, "attributeNotInHierarchy", "attributeNotInHierarchy")
	  else (true, serialize(hMatch), serialize(vMatch))
  }
 }
 
 def checkAllGrids(): List[String] = {
  def f(a: Node) = findAttribute(MyUtil.getSeqHeadText(a \ "name"))
	  
  val blocks = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block").filter(b => MyUtil.getSeqHeadText(b \\ "presentationType") == "grid")
  val check = blocks.map(bl => checkGrid(MyUtil.getSeqHeadText(bl \ "horizontalSpan"), MyUtil.getSeqHeadText(bl \ "verticalSpan"), (bl \\ "attribute").map(f).filter(_ != null).toList)).toList
  check.filter(c => !c._1).map(c => c._2 + " " + c._3)
 }
 
 def displayGrid(errors: List[String]) = <ul style="list-style-position:inside">{errors.map(e => <li>{e}</li>).toSeq}</ul>

 def createMetadata(isAccountModel: Boolean) = {
  val blocks = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block").filter(b => MyUtil.getSeqHeadText(b \\ "presentationType") != null && MyUtil.getSeqHeadText(b \\ "presentationType").length > 0)
  val frames = (Repository.read("scenario", SelectedScenario.is.id, "frames", "frames", -1) \\ "fr").filter(fr => (fr \ "*").size > 0)

  val metadata = <metadata>
	  				<frames>{frames}</frames>
  					<layout>{Setup.is \\ "layout"}</layout>
	  				<blocks>{blocks}</blocks>
	  				<backend>{blocks.map(bl => createSelects(bl, isAccountModel))}</backend>
	  		     </metadata>  % new UnprefixedAttribute("scenarioId", SelectedScenario.is.id.toString, Null)
  
  Repository.write("release", 0, null, "metadata", SelectedRelease.is.id, metadata)
 }
 
 def documentAsXml() = {
  
  def meetingAsXml(m: Meeting) = {
	def itemXml(item: ProtocolItem) = <item><number>{item.itemNumber.toString}</number><classification>{item.classification}</classification><text>{item.itemText}</text></item>
	  
	val minutes = Minutes.findAll(By(Minutes.fkMeeting, m.id), By(Minutes.status, "published"), OrderBy(Minutes.version, Descending))  
	val minutesXml = if(minutes.isEmpty) <minutes /> 
				 else {
					 
					 val items = ProtocolItem.findAll(By(ProtocolItem.fkMinutes, minutes(0).id), OrderBy(ProtocolItem.itemNumber, Ascending))
					 
					 <minutes>
					 	<version>{minutes(0).version}</version><datePublished>{MyUtil.formatDate(minutes(0).datePublished, S.?("dateFormat"))}</datePublished>
					 	<items>{items.map(itemXml).toSeq}</items>
					 </minutes>
				 }
	
	<meeting>
		<category>{S.?("category") + ": " + m.category}</category>
		<topic>{S.?("topic") + ": " + m.topic}</topic>
		<date>{S.?("meetingDate") + ": " + MyUtil.formatDate(m.meetingBegin, S.?("dateFormat")) + ", " + MyUtil.timeInDay(m.meetingBegin) + " - " + MyUtil.timeInDay(m.meetingEnd)}</date>
        {minutesXml}
	</meeting> 
  }
  
  def getContext(m: Measure): NodeSeq = {
   def contextTr(key: MeasureToModelVertex) = {
    val allLevel = ModelVertex.findAll(By(ModelVertex.id, key.fkLevel))
   
    if(allLevel.isEmpty) <ctx><dim></dim><level></level><aggr>{S.?(key.aggregation)}</aggr></ctx>
    else {
	   val level = allLevel.apply(0)
	   val dimensions = ModelVertex.findAll(By(ModelVertex.id, level.referenceId), By(ModelVertex.elementType, "dimension"))
	   val dim = if(!dimensions.isEmpty) dimensions(0).elementName else ""
	   <ctx><dim>{dim}</dim><level>{level.elementName}</level><aggr>{S.?(key.aggregation)}</aggr></ctx>
    }
   }

   MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkMeasure, m.id)).map(contextTr).toSeq
  }
  
  def serializeMeasure(m: Measure) = {
	 
   val ranges = MeasureRange.findAll(By(MeasureRange.fkMeasure, m.id), OrderBy(MeasureRange.lowerBound, Ascending))
   val rangeList = ranges.map(r => <range><lb>{r.lowerBound.toString}</lb><ub>{r.upperBound.toString}</ub><value>{r.rangeValue.toString}</value><meaning>{r.meaning}</meaning></range>).toSeq	
   
   <measure>
      <shortName>{m.shortName}</shortName>
      <longNameLabel>{S.?("longName")}</longNameLabel>
      <longName>{m.longName}</longName>
      <definitionLabel>{S.?("definition")}</definitionLabel>
      <definition>{m.definition}</definition>
      <formulaLabel>{S.?("formula")}</formulaLabel>
      <formula>{m.formula}</formula>
      <unitLabel>{S.?("unit")}</unitLabel>
      <unit>{m.unit}</unit>
      <lifecycle>{S.?("lifecycle")}</lifecycle>
      <actualityLabel>{S.?("requiredActuality")}</actualityLabel>
      <actuality>{m.requiredActualityValue.toString + " " + S.?(m.requiredActualityUnit)}</actuality>
      <timeOfInterestLabel>{S.?("timespanOfInterest")}</timeOfInterestLabel>
      <timeOfInterest>{m.timespanOfInterestValue.toString + " " + S.?(m.timespanOfInterestUnit)}</timeOfInterest>
      <storageTime>{m.requiredStorageValue.toString + " " + S.?(m.requiredStorageUnit)}</storageTime>
      <storageTimeLabel>{S.?("requiredStorageTime")}</storageTimeLabel>
      <contextTitle>{S.?("context")}</contextTitle>
      <context>{getContext(m)}</context>
      <rangesTitle>{S.?("ranges")}</rangesTitle>
      <ranges>{rangeList}</ranges>
      <dimensionTitle>{S.?("dimension")}</dimensionTitle>
      <levelTitle>{S.?("level")}</levelTitle>
      <aggregationTitle>{S.?("aggregation")}</aggregationTitle>
      <lbTitle>{S.?("lowerBound")}</lbTitle>
      <ubTitle>{S.?("upperBound")}</ubTitle>
      <rvTitle>{S.?("rangeValue")}</rvTitle>
      <meaningTitle>{S.?("meaning")}</meaningTitle>
   </measure>
  }
  
  def measuresAsXml() = {
   def measuresOfSubjects(subject: String, l: List[Measure]) = l.filter(_.subject == subject) 
   val msrs = Measure.findAll(By(Measure.fkScenario, SelectedScenario.is.id), By(Measure.status, "approved")).toList
   val subjects = msrs.map(_.subject.toString).sort(_ < _).distinct.toList
   val subjectList = subjects.map(s => <subject><name>{s}</name><measures>{measuresOfSubjects(s,msrs).map(serializeMeasure).toSeq}</measures></subject>)
   
   <subjects>{subjectList}</subjects>
  }
  
  def visionSection(section: Node) = <section><title>{MyUtil.getSeqHeadText(section \\ "title")}</title><displayText>{MyUtil.tagIt(MyUtil.getSeqHeadText(section \\ "displayText"))}</displayText></section>
  
  def featuresAsXml() = {
   def fn(f: Feature, fs: List[Feature]): String = {
	   if(f.parentFeature == 0) f.featureNumber.toString
	   else {
	  	   val parent = fs.filter(_.id == f.parentFeature)
	  	   if(parent.isEmpty) "9999" else fn(parent(0), fs) + "." + f.featureNumber.toString
	   }
   }
   
   def findDescription(f: Feature) = {
	   if(f.description != null && f.description.length > 0 && (f.featureType == S.?("businessQuestion") || f.featureType == S.?("complianceRequest"))) MyUtil.tagIt(WikiParser.parseBusinessQuestion(f.description)._2)
	   else f.description 
   }
   
   def toXml(item: (String, Feature)) = {
	   <feature>
	   		<featureId>{item._1}</featureId>
	   		<featureName>{item._2.name}</featureName>
	   		<featureTypeLabel>{S.?("featureType")}</featureTypeLabel>
	   		<featureType>{item._2.featureType}</featureType>
	   		<storyPointsLabel>{S.?("storyPoints")}</storyPointsLabel>
	   		<storyPoints>{item._2.storyPoints.toString}</storyPoints>
	   		<priorityLabel>{S.?("priority")}</priorityLabel>
            <priority>{item._2.priority.toString}</priority>
	   		<descriptionLabel>{S.?("description")}</descriptionLabel>
	   		<featureDescription>{findDescription(item._2)}</featureDescription>
	   </feature>
   }
   
   val features = Feature.findAll(By(Feature.fkPb, ChosenBacklog.is.id)).toList
   val featureList = (features.map(f => fn(f, features)) zip features).sort(_._1 < _._1)
   featureList.map(toXml).toSeq
  }
  
  def logicToXml(): Node = {
   def serializeVertex(d: ModelVertex, isDimension: Boolean) = {
	val svg = Repository.read("scenario", SelectedScenario.is.id, "svg", d.elementName.toString, 0) 
	if(isDimension) <dimension><name>{d.elementName}</name><description>{d.elementDescription}</description><diagram>{svg}</diagram></dimension>
	else <cube><name>{d.elementName}</name><description>{d.elementDescription}</description><diagram>{svg}</diagram></cube>
	
   }
   
   val candidates = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementKind, "original"))
   val dimOrCube = candidates.filter(c => c.elementType == "dimension" || c.elementType == "cube").partition(_.elementType == "dimension")
   val dimensions = dimOrCube._1.sort(_.elementName.toString < _.elementName.toString)
   val cubes = dimOrCube._2.sort(_.elementName.toString < _.elementName.toString)
   
   <logic><dimensions>{dimensions.map(d => serializeVertex(d, true)).toSeq}</dimensions><cubes>{cubes.map(c => serializeVertex(c, false)).toSeq}</cubes></logic>
  }
  
  val creationDate = MyUtil.formatDate(new Date, S.?("dateFormat"))
  val prettyR = S.?("release") + " " + SelectedRelease.is.prettyNumber
  
  val scenario = <scenario>
  					<name>{SelectedScenario.is.name}</name>
  					<description>{SelectedScenario.is.description}</description>
  					<release>{prettyR}</release>
  					<subTitle>{S.?("documentationSubtitle")}</subTitle>
  					<creationDate>{creationDate}</creationDate>
  				</scenario>
  					
  val vision = (Repository.read("scenario", SelectedScenario.is.id, "vision", "vision", -1) \\ "vision").apply(0)
  val purgedVision = <vision>{(vision \\ "section").map(visionSection).toSeq}</vision>
  
  val aboutText = S.?("documentationIntroduction").replaceAll("xxscenarioxx", SelectedScenario.is.name).replaceAll("xxcreationDatexx", creationDate).replaceAll("xxreleasexx", prettyR)
  
  <documentation>
  	<header>{scenario}</header>
    <chapter>
    	<chapterTitle>{S.?("aboutDocument")}</chapterTitle>
    	<about><document>{aboutText}</document><scenarioHeader>{S.?("aboutScenario")}</scenarioHeader></about>
    </chapter>
    <chapter>
    	<chapterTitle>{S.?("vision")}</chapterTitle>
    	<content>{purgedVision}</content>
    </chapter>
     <chapter>
    	<chapterTitle>{S.?("backlog")}</chapterTitle>
    	<backlog>{featuresAsXml()}</backlog>
    </chapter>
    <chapter>
    	<chapterTitle>{S.?("logicalModel")}</chapterTitle>
    	{logicToXml()}
    </chapter>
    <chapter>
    	<chapterTitle>{S.?("catalogue")}</chapterTitle>
    	<content>{measuresAsXml()}</content>
    </chapter>
    <chapter>
    	<chapterTitle>{S.?("minutes")}</chapterTitle>
    	<content><meetings>{Meeting.findAll(By(Meeting.fkScenario, SelectedScenario.is), OrderBy(Meeting.meetingBegin, Ascending)).map(meetingAsXml).toSeq}</meetings></content>
    </chapter>
  </documentation>
 }
 
 def createDocumentation() = {
  val rId = SelectedRelease.is.id
  Repository.write("configuration", 0, "metadata", "documentation" + rId.toString, -1, documentAsXml())
  val source = Repository.readAsFile("configuration", 0, "metadata", "documentation" + rId.toString, -1)
  val pdf = Repository.emptyDocumentation(rId)
  PDFUtility.transform(source, pdf)
 }
 
 def generateRelease(): JsCmd = {
  def previousRelease(r: Release): Option[Release] = {
	  val predecessors = Release.findAll(By(Release.fkScenario, r.fkScenario)).filter(otherRelease => otherRelease.id < r.id).sort(_.id < _.id)
	  if(predecessors.isEmpty) None else Some(predecessors(0))
  }
	 
  if(SelectedRelease.is != null) {
	  
	val checks = ModelUtility.checkAllGraphs(SelectedScenario.is.id)
	val checkGrid = checkAllGrids()
	
	if(checks.isEmpty && checkGrid.isEmpty) {
		val setup = Repository.read("scenario", SelectedScenario.is.id, "setup","setup", -1) \\ "setup"
		
		val extraViewLayer = MyUtil.getSeqHeadText(setup \\ "extraViewLayer") == "Y"
		val stableViewLayer = MyUtil.getSeqHeadText(setup \\ "stableViewLayer") == "Y"
		val accountModel = 	MyUtil.getSeqHeadText(setup \\ "accountModel") == "Y"
		val previous = previousRelease(SelectedRelease.is) 
		
		Repository.emptyRelease(SelectedRelease.is.id.toLong)
		PTable.findAll(By(PTable.fkScenario, SelectedScenario.is.id), By(PTable.isCurrent, 1)).map(table => createDDL(table, extraViewLayer, stableViewLayer, previous))
		createDrops(SelectedRelease.is, previous)
		createMetadata(accountModel)
		createDocumentation()
		Alert(S.?("artefactsCreated"))
	}
	else SetHtml("resultsOfScenarioCheck", <div>{ModelUtility.presentGraphResult(checks)}{displayGrid(checkGrid)}</div>)
  }
  else Alert(S.?("noReleaseChosen"))
 }

 def menu (xhtml: NodeSeq): NodeSeq = {
  bind("menu", xhtml, "create"  -> ajaxButton(S.?("createNewRelease"), createNewRelease _) % ("class" -> "standardButton"),
		              "announce" -> ajaxButton(S.?("announceReleaseFreeze"), announce _) % ("class" -> "standardButton"),
		              "freeze" -> ajaxButton(S.?("freeze"), freeze _) % ("class" -> "standardButton"),
		              "generate" -> ajaxButton(S.?("generate"), generateRelease _) % ("class" -> "standardButton"),
		              "show" -> Release.findAll(By(Release.fkScenario, SelectedScenario.is.id)).map(createListItem),
		              "showStatus" -> showStatus(),
		              "artefacts" -> artefacts(),
		              "delta" -> delta())
 }
 
 /**
  * deployment comes next
  */
 
 def environmentsOfSystem(): NodeSeq = {
  def selectAlias(alias : String) : JsCmd = {
    SelectedAlias(alias)
    JsRaw("$('.aliasList').removeClass('emphasized'); $(\".aliasList[alias='" + alias + "']\").addClass('emphasized');")
  }
	 
  def store(alias: Node) = {
	val aliasName = MyUtil.getNodeText(alias) 
    val action = SHtml.ajaxCall(JsRaw("$(this).attr('alias')"), selectAlias _)._2
    <li class ="aliasList">{aliasName}</li> % ("onclick" -> action) % new UnprefixedAttribute("alias", aliasName, Null)
  }
  
  def deployedRelease(release: Node) = {
   val releaseId = (release \ "@releaseId").text.toLong
   val r = Release.findAll(By(Release.id, releaseId))
   if(r.isEmpty) NodeSeq.Empty
   else {
	   <li>
	   	<span>{S.?("release") + " " + r(0).prettyNumber}</span><br />
	   	<span>{S.?("dataStore") + " " + MyUtil.getNodeText(release)}</span>
	   </li>
   }
  }
  
  def createEnvironmentDescription(ns: NodeSeq, environment: String) = {
   if(ns.isEmpty) NodeSeq.Empty
   else { 
	   val releaseNodes =  ns(0) \\ "deployment" \ "release"
	   val releaseIds = releaseNodes.map(n => (n \ "@releaseId").text.toLong).toList
	   val stores = (ns(0) \\ "dataStores" \\ "alias").map(store)
	   
	  <div class="message">
   		<p class="messageTitle">{S.?(environment)}</p>
	   	 <h4>{S.?("releases")}</h4><br /><br />
   		 <ul>{releaseNodes.map(deployedRelease)}</ul><br />
         <h4>{S.?("dataStores")}</h4><br />
         <ul>{stores}</ul>
   	  </div>
   }
  }
  
  if(SelectedSystem.is == null) NodeSeq.Empty
  else {
   val nodesOfSystem = MyUtil.flattenNodeSeq((Repository.read("configuration", -1, "nodes", "nodes",0).map(Utility.trim) \\ "node").filter(n => MyUtil.getSeqHeadText(n \\ "system") == SelectedSystem.is).toList)
   MyUtil.flattenNodeSeq(List("DEV", "TEST", "ACCT", "PROD").map(env => createEnvironmentDescription(nodesOfSystem.filter(n => MyUtil.getSeqHeadText(n \\ "environment") == env), env)))
  }
 }
 
 def userRow(dn: String, checksum: String, fileName: String, blocks: String) ={
	 <user>
		<id>{dn}</id>
		<checksum>{checksum}</checksum>
		<metadata>{fileName}</metadata>
		<blocks>{blocks}</blocks>
	</user>
 }
 
 def envOrder(env: String): Int = env match {
	 case "DEV" => 0
	 case "TEST" => 1
	 case "ACCT" => 2
	 case _ => 3
 }
 
 def serializeBlockIds(blockIds: List[Long]) = ("" /: blockIds.map(b => "b" + b.toString)) (_ + _)
 
 def createUserMetadata(specs: List[Specification], allMetadata: Node): (Node, String, String) = {
  def makeScorecardNode(spec: Specification, allMetadata: Node) = {
	  
	  val layout = (allMetadata \\ "metadata").filter(m => (m \ "@scenarioId").text  == spec.fkScenario.toString).apply(0) \\ "scorecard" \ "*"
	  val frames = (allMetadata \\ "frame").filter(fr => (fr \ "@scorecardId").text == spec.id.toString)
	
	  <scorecard><title>{spec.name}</title>{layout}{frames}</scorecard> % new UnprefixedAttribute("id", spec.id.toString, Null)
  }
  
  def makeBlockNodes(scenarioId: Long, blockIds: List[Long]) = {
	  val blocks = Repository.read("scenario", scenarioId, "blocks", "blocks", -1) \\ "block" 
	  blocks.filter(b => blockIds.exists(_ == (b \\ "@id").text.toLong)).toSeq
  }
  
  val scorecards = specs.map(sp => makeScorecardNode(sp, allMetadata)).toSeq
  val blockIds = specs.map(sp => Block.findAll(By(Block.fkSpecification, sp.id)).map(b => (sp.fkScenario.toLong, b.id.toLong)).toList).flatten.distinct
  val scenarioIds = blockIds.map(_._1).distinct
  val blocks = scenarioIds.map(scId => makeBlockNodes(scId, blockIds.filter(_._1 == scId).map(_._2).toList)).toSeq
  
  val root = <scorecards>{scorecards}{blocks}</scorecards>
  val checksum = root.hashCode.toString
  
  (root % new UnprefixedAttribute("checksum", checksum, Null), checksum, serializeBlockIds(blockIds.map(_._2).toList))
 }
 
 def releasesOfNode(n: Node):List[Release] = {
  val releaseIds =   (n \\ "deployment" \ "release").map(r => (r \ "@releaseId").text).toList
  List.flatten(releaseIds.map(id => Release.findAll(By(Release.id, id.toLong))))
 }
 
 def deploy(isInitial: Boolean): JsCmd = {
	  
	 if(ReleaseForDeployment.is != null & SelectedSystem.is != null) {
	 	  
	 	  val nodes = (Repository.read("configuration", -1, "nodes", "nodes",0) \\ "node").filter(n => MyUtil.getSeqHeadText(n \\ "system") == SelectedSystem.is)
	 	  val nodesWithReleases = nodes.map(n => (n, releasesOfNode(n), envOrder(MyUtil.getSeqHeadText(n \\ "environment")))).toList.sort(_._3 < _._3)
	 	  val nodesWithTargetRelease = nodesWithReleases.filter(_._2 exists(_ == ReleaseForDeployment.is))
	 	  
	 	  if(!nodesWithTargetRelease.isEmpty && isInitial) Alert(S.?("alreadyDeployed"))
	 	  else if(nodesWithTargetRelease.size == nodesWithReleases.size) Alert(S.?("alreadyDeployedTransported"))
	 	  else {
	 	 	  
	 	 	  if(isInitial && SelectedAlias.is == null) Alert(S.?("noDataStoreAliasSelected"))
	 	 	  else { 
	 	 	 	  val node = nodesWithReleases.apply(nodesWithTargetRelease.size)
	 	 	 	  deployOnEnvironment(node._1, ReleaseForDeployment.is, node._2)
	 	 	  }
	 	  } 
	 }
	 else  Alert(S.?("noReleaseOrNoSystem"))
 }
 
 def dropTablesOfRelease(r: Release, n: Node, aliasName: String) = {
  val tableNames = Repository.getArtefactList(r.id, List("sql"), false).map(fileName => fileName.substring(0, fileName.indexOf(".")))
  val store = (n \\ "store").filter(st => MyUtil.getSeqHeadText(st \ "alias") == aliasName)
  if(!store.isEmpty) tableNames.map(name => DB.use(BIService.storeMap(aliasName)._1) { conn => DB.exec(conn, "DROP TABLE " + name + ";") {rs => "success"}})
 }
 
 def createTablesOfRelease(r: Release, n: Node, aliasName: String) = {
  val fileNames = Repository.getArtefactList(r.id, List("sql"), false)
  val store = (n \\ "store").filter(st => MyUtil.getSeqHeadText(st \ "alias") == aliasName)
  if(!store.isEmpty) fileNames.map(name => DB.use(BIService.storeMap(aliasName)._1) { conn => DB.exec(conn, Repository.getArtefactAsString(r.id, name)) {rs => "success"}})
 }
 
 def userGroups(specs: List[Specification]): List[(List[Specification], List[(Provider, String)])] = {
  def member(rtg: RoleToGroup): List[(Provider, String)] = {
	if(rtg.fkProvider != null) {
		val provider = Provider.findAll(By(Provider.id, rtg.fkProvider)).apply(0)
		val conf = MyUtil.createConfiguration(provider)
		val myLdap = new LDAPVendor
		myLdap.configure(conf)
		val dns = MyUtil.attr2List(myLdap.attributesFromDn(rtg.dn + "," + provider.base), provider.memberAttribute.toString, provider.displayAttribute.toString)
		dns.map(item => (provider, item)).toList
	}
	else {
		val allUser = List.flatten(ScenarioRole.findAll(By(ScenarioRole.fkScenario, rtg.fkScenario)).map(sr => User.findAll(By(User.id, sr.fkUser))))
		allUser.map(u => (null, u.email.toString)).toList
	}
  }
  
  val withRoles = specs.map(sp => (sp, SpecificationToRole.findAll(By(SpecificationToRole.fkSpecification, sp.id)).map(_.getUserRole).toList))
  val withGroups = withRoles.map(item => (item, List.flatten(item._2.map(ur => RoleToGroup.findAll(By(RoleToGroup.fkRole, ur.id))))))
  val withDns = withGroups.map(item => (item, List.flatten(item._2.map(member))))
  val specsDns = withDns.map(item => (item._1._1._1, item._2)).toList
  val allGuys = List.flatten(specsDns.map(_._2)).distinct
  val guysAndSpecs = allGuys.map(guy => (guy, specsDns.filter(item => item._2 exists(_ == item)).map(_._1).distinct.sort(_.id < _.id)))
  val allGroups = guysAndSpecs.map(_._2).distinct
  allGroups.map(gr => (gr, guysAndSpecs.filter(_._2 == gr).map(_._1).distinct.toList))
 }
 
 def ajaxification(l: List[(String, Node)], counter: Int, total: Int, user: User): String = l match {
	 case Nil => ""
	 case List(last) => "$('#deploymentProgress').html('<span>" + S.?("deploymentComplete")+ "</span>');"
	 case first :: second :: tail => {
		 val message = "<span>" + counter.toString + " " + S.?("of") + " " + total.toString + " " + S.?("filesDeployed") + "</span>"
		 val success = "function(resp){$('#deploymentProgress').html('" + message + "');" + ajaxification(second :: tail, counter + 1, total, user) + "}"
		 "$.ajax({url: '" + first._1 + "', data: '" + first._2.toString + "',  username: '" +  user.email + "', password: '" + user.password + "', success:" + success + "});"
	 }
 }
 
 def updateDeploymentOnNode(node: Node, oldRelease: Release, newRelease: Release, systemName: String): Node = {
  val releases = (node \\ "deployment" \ "release").map(r => ((r \ "@releaseId").text, MyUtil.getNodeText(r)))
  val newReleases = if(oldRelease != null) (newRelease.id.toString, systemName) :: releases.filter(_._1 != oldRelease.id.toString).toList else (newRelease.id.toString, systemName) :: releases.toList
  val transform = "deployment" #> <deployment>{newReleases.map(r => <release releaseId={r._1}>{r._2}</release>).toSeq}</deployment>
  transform(node).apply(0)
 }
 
 def replaceNodes(item: Node, node: Node, oldRelease: Release, newRelease: Release, systemName: String): Node = {
  if((item \ "@technicalName").text == (node \ "@technicalName").text) updateDeploymentOnNode(node: Node, oldRelease: Release, newRelease: Release, systemName: String)
  else item
 }
 
 def deployOnEnvironment(n: Node, targetRelease: Release, releasesSoFar: List[Release]): JsCmd = {
  def txt(node: Node, tag: String) = MyUtil.getSeqHeadText(node \\ tag)
	 
  val releasesAfterDeployment = targetRelease :: releasesSoFar.filter(_.fkScenario != targetRelease.fkScenario)	 
  val deploymentItems = new ListBuffer[(String, String, String, Node)] // contains at the end all information for deployment service
  
  // get the store information
  
  val stores = (n \\ "dataStores")
  
  if(stores.isEmpty) Alert(S.?("malformedStoreConfiguration"))
  else {
	  val storeInformation = ("stores", stores(0).hashCode.toString, "generic", stores(0))
	  deploymentItems += storeInformation
  }
  
  val allMetadata = <anything>{MyUtil.flattenNodeSeq(releasesAfterDeployment.map(r => Repository.getMetadataOfRelease(r.id) \\ "metadata").toList)}</anything>
  val scorecardIds = (allMetadata \\ "fr").map(fr => (fr \ "@scorecardId").text.toLong).toList
  
  val specs = List.flatten(scorecardIds.map(id => Specification.findAll(By(Specification.id, id))))
   	 	  
  // create the metadata
	 	 	  
  val blocks = allMetadata \\ "backend" \ "block"
  val blockXml = <blocks>{blocks}</blocks>
  val blockInformation = ("blocks", blockXml.hashCode.toString, "generic", blockXml)
  deploymentItems += blockInformation	
  
  val oldRelease = releasesSoFar.filter(r => r.fkScenario == targetRelease.fkScenario)
  
  if(!oldRelease.isEmpty) dropTablesOfRelease(oldRelease(0), n, SelectedAlias.is)
  
  createTablesOfRelease(targetRelease, n, SelectedAlias.is)
  
  val allUserGroups = userGroups(specs)
  val indexedList = allUserGroups.indices zip allUserGroups
  
  val userItems = new ListBuffer[(String, String, String, String)] 
  
  for(index <- indexedList) {
	  val userMetaData = createUserMetadata(index._2._1, allMetadata) 
	  val fileName = "m" + index._1.toString
	  val userGroupInformation = (fileName, userMetaData._2, "individual", userMetaData._1)
	  deploymentItems += userGroupInformation
	  
	  for(aUser <- index._2._2) {
	 	  val aUserItem = (aUser._2, userMetaData._2, fileName, userMetaData._3)
	 	  userItems += aUserItem
	  }
  }
  
  val userNode = <node>{userItems.map(item => userRow(item._1, item._2, item._3, item._4))}</node>
  val userItem = ("user", userNode.hashCode.toString, "individual", userNode)
  deploymentItems += userItem
  
  // serialize the information in deployment items
  
  val deploymentInformation = <items>{deploymentItems.map(item => <item><name>{item._1}</name><checksum>{item._2}</checksum></item>)}</items>
  
  // prepare the ajax requests 
  
  val url = "http://" + MyUtil.getSeqHeadText(n \\ "host") + ":" + MyUtil.getSeqHeadText(n \\ "port")
  val user = User.currentUser openOr null
  val configurationCommand = "$.ajaxSetup({type: 'POST', dataType: 'xml', contentType: 'text/xml', username: '" +  user.email + "', password: '" + user.password + "'});"
  
  val requestInformation = (url + "/deployment/start.xml", deploymentInformation) :: deploymentItems.map(item => (url + "/deployment/transfer/" + item._3 + "/" +  item._1 + ".xml", item._4)).toList
  val cmd = configurationCommand + ajaxification(requestInformation, 0, requestInformation.size, user)
  
  // node transformation
  
  val allNodes = Repository.read("configuration", -1, "nodes", "nodes",0).map(Utility.trim) \\ "node"
  val old = if(!oldRelease.isEmpty) oldRelease(0) else null
  val transformedNodes = <nodes>{ allNodes.map(item => replaceNodes(item, n, old, targetRelease, SelectedAlias.is))}</nodes>
  
  Repository.write("configuration", -1, "nodes", "nodes", 0, transformedNodes)
  Nodes(transformedNodes)
  println(cmd)
  JsRaw(cmd)
 }
 
 def allSystems(): NodeSeq = {
  def selectSystem(systemName : String) : JsCmd = {
   SelectedSystem(systemName)
   SetHtml("environmentContainer", environmentsOfSystem())
  }
  
  val nodes = Repository.read("configuration", -1, "nodes", "nodes",0).map(Utility.trim) \\ "node" \ "system"
  val action = SHtml.ajaxCall(JsRaw("$(this).text()"), selectSystem _)._2
  nodes.map(MyUtil.getNodeText).distinct.map(text => <li class='listItem systemItem'>{text}</li> % ("onclick" -> action)).toSeq
 }
 
 def selectReleaseForDeployment(releaseId : String) = {
  val release = Release.findAll(By(Release.id, releaseId.toLong)).apply(0)
  ReleaseForDeployment(release)
  Noop
 } 
 
 def createReleaseItem(release : Release) = {
  val name = S.?("release") + " " + release.prettyNumber
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('releaseId')"), selectReleaseForDeployment _)._2
 
  <li class='treeItem releaseItem emphasizable'>
  	<span class="leaf">__</span><span class="emphasizable">{name}</span>
  </li> % ("onclick" -> action) % new UnprefixedAttribute("releaseId", release.id.toString, Null)
 }
 
 def getScenarioWithReleases(sc: Scenario): Node = {
  val releases = Release.findAll(By(Release.fkScenario, sc.id))
  if(releases.isEmpty) <li class='treeItem'><span class="leaf">__</span><span>{sc.name}</span></li> 
  else <li class='treeItem'><span class="handle closed">__</span><span>{sc.name}</span><ul>{releases.map(createReleaseItem).toSeq}</ul></li>
 }
 
 def allReleases(): NodeSeq = {
  if (User.loggedIn_?) {
   val user = User.currentUser openOr null
   if(user != null) {
	   Scenario.findAll(ByList(Scenario.fkClient, user.clients.all.map(_.id.toLong).toList)).map(getScenarioWithReleases).toSeq
   }
   else NodeSeq.Empty 	  
  }
  else NodeSeq.Empty
 }
 
 def getPTable(tableName: String, r: Release): PTable = {
  val ps = PTable.findAll(By(PTable.fkScenario, r.fkScenario), By(PTable.name, tableName))
  val psInTime = ps //.filter(pt => MyUtil.dateAsNumber(pt.validFrom) <= MyUtil.dateAsNumber(r.freezeDate) && (pt.validUntil == null || MyUtil.dateAsNumber(pt.validUntil) > MyUtil.dateAsNumber(r.freezeDate)))
  if(psInTime.isEmpty) null else psInTime.apply(0)	 
 }
 
 def getTableModel(pt: PTable): ModelVertex = {
  if(pt.isDerivedFromModel == 1) {
	  val modelId = List(pt.fkCube.toLong, pt.fkLevel.toLong, pt.fkDimension.toLong).max
	  if(modelId > 0) ModelVertex.findAll(By(ModelVertex.id, modelId)).apply(0) else null
  } else null
 }
 
 def hierarchyNumber(level: PTable): Int = {
  def getReference(id: Long) = PTable.findAll(By(PTable.id, id)).apply(0)
  val refs = PAttribute.findAll(By(PAttribute.fkPTable, level.id), By_>(PAttribute.reference, 0))
  if(refs.isEmpty) 0 else 1 + refs.map(r => hierarchyNumber(getReference(r.id))).max
 }
 
 def sortTable(t: PTable): Int = if(t.tableType == "level") hierarchyNumber(t) else if(t.tableType == "dimension") 1000 else 2000
 
 def createTestData(): JsCmd = {
  if(SelectedSystem.is != null) {
	  
	  val nodes = (Repository.read("configuration", -1, "nodes", "nodes",0) \\ "node")
	  val devNode = nodes.filter(n => MyUtil.getSeqHeadText(n \\ "system") == SelectedSystem.is).filter(n => MyUtil.getSeqHeadText(n \\ "environment") == "DEV")
	 
	  if(devNode.isEmpty) Alert(S.?("noDevNode"))
	  else {
	 	  
	 	  val releases = releasesOfNode(devNode.apply(0))
	 	  
	 	  if(releases.isEmpty) Alert(S.?("noReleasesOnNode"))
	 	  else { 	  
	 	
	 	 	  val tableNames = releases.map(r => (r, Repository.tableNamesOfRelease(r.id).toList)).toList
	 	 	  val tables = List.flatten(tableNames.map(pair => pair._2.map(tn => getPTable(tn, pair._1)).filter(_ != null))).distinct.sort(sortTable(_) < sortTable(_))
	 	 	  val tablesAndModels = tables zip tables.map(getTableModel)
	 	 	  
	 	 	  TestDataNode(devNode.apply(0))
	 	 	  TestDataModel(tablesAndModels)
	 	 	   	 	  
	 	 	  RedirectTo("/testData")
	 	  }
	  }
  }
  else Alert(S.?("noSystemChosen"))
 }
  
 def estimate():NodeSeq = {
  def tableInformation(tableName: String, initialSize: String, growth: String) = {
	  <label><b>{S.?("tableName")}: </b></label><span>{tableName}</span><br />
	  <label><b>{S.?("initialSize")}: </b></label><span>{initialSize}</span><br />
	  <label><b>{S.?("growthPerLoad")}: </b></label><span>{growth}</span><br />
  }
  
  val model = TestDataModel.is.partition(tm => tm._1.tableType == "dimension" || tm._1.tableType == "level" || tm._1.tableType == "timeDimension" || tm._1.tableType == "accountDimension")

  <div class="message">
   	<p class="messageTitle">{S.?("sizeEstimation")}</p>
   	<div>
   		<h3>{S.?("dimensions")}</h3><br />
   			{model._1.map(tm => tableInformation(tm._1.name, ModelUtility.initialSize(tm._2), ModelUtility.growth(tm._2)))}
   		<h3>{S.?("facts")}</h3><br />
   			{model._2.map(tm => tableInformation(tm._1.name, ModelUtility.initialSize(tm._2), ModelUtility.growth(tm._2)))}
   	</div>
  </div>
 }
 
 def allScenariosWithReleases (xhtml: NodeSeq): NodeSeq = {
  
  def makeInitialDeployment() = deploy(true)
  def transport() = deploy(false)

  bind("deployment", xhtml, "releases" -> allReleases(),
		                    "deploy" -> ajaxButton(S.?("deploy"), makeInitialDeployment _) % ("class" -> "standardButton"),
		                    "transport" -> ajaxButton(S.?("transport"), transport _) % ("class" -> "standardButton"),
		                    "createTestData" -> ajaxButton(S.?("createTestData"), createTestData _) % ("class" -> "standardButton"),
		                    "systems" -> allSystems(),
		                    "estimation" -> estimate())
  }
}