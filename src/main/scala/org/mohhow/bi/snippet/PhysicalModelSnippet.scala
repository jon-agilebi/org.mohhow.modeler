package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
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

import org.mohhow.bi.lib.Repository
import org.mohhow.bi.lib.ModelUtility
import org.mohhow.bi.util.{Utility => MyUtil}
import scala.collection.mutable.HashSet

object SelectedTable extends SessionVar[PTable](null)

class PhysicalModelSnippet {

 def selectTable(id : Long) = {
  val pt = PTable.findAll(By(PTable.id, id)).apply(0)
  SelectedTable(pt)
 }  

 def createListItem(pt : PTable) = {
  val name = pt.name.toString
  val myId = pt.id.is
  val item = link("/attribute", () => selectTable(myId), <span>{name}</span>)
  <li class='listItem'>{item}</li>
 }

 def addTable() : JsCmd = {
  val newTable = PTable.create
  newTable.name("<table name>").fkScenario(SelectedScenario.is).validFrom(new Date).isCurrent(1).isDerivedFromModel(0).save
  JsRaw("$('#physical_table_overview').append('" + createListItem(newTable) + "')")
 }
 
 /**
  * create a list of level elements starting with the most granular element and stopping  at the level element closest to the hierarchy 
  */

 def findLevel(h: ModelVertex, allLevel: List[ModelVertex]): List[ModelVertex] = {
  def f(pivot: ModelVertex, soFar: List[ModelVertex], remainder: List[ModelVertex]): List[ModelVertex] = remainder match {
	case Nil => soFar
	case head :: tail => if(MyUtil.isConnected(pivot.id, head.id)) f(head, head :: soFar, tail) 
	                     else {
	                      	   if(!tail.filter(x => MyUtil.isConnected(x.id, pivot.id)).isEmpty) f(pivot, soFar, tail) else soFar
	                     }
  }
	 
  f(h, Nil, allLevel)
 }
 
 /**
  * create a table attribute from a model attribute
  */
 
 def createAttribute(table: PTable, attr: ModelVertex, dataType: String) = {
  val newAttribute = PAttribute.create
  newAttribute.fkPTable(table).name(attr.elementName).validFrom(new Date).isCurrent(1).isDerivedFromModel(1).fkModelAttribute(attr.id).isPrimaryKey(0)
  MyUtil.setAttributePhysicalType(dataType, newAttribute).save
 }
 
 def createAttributeFromMeasure(table: PTable, m: Measure, dataType: String) = {
  val newAttribute = PAttribute.create
  newAttribute.fkPTable(table).name(m.shortName.toString.toUpperCase).validFrom(new Date).isCurrent(1).isDerivedFromModel(4).fkMeasure(m.id).isPrimaryKey(0)
  MyUtil.setAttributePhysicalType(dataType, newAttribute).save
 }
 
 def createReference(table: PTable, refTable: PTable) = {
  def findReferencingAttribute(t: PTable): Long = {
	  val keyAttribute = PAttribute.findAll(By(PAttribute.fkPTable, t.id), By(PAttribute.isPrimaryKey, 1))
	  if(keyAttribute.isEmpty) 0 else keyAttribute.apply(0).id.toLong
  }
  
  val defaultKeyType = MyUtil.getSeqHeadText(Setup.is \\ "defaultKeyType")
  val newAttribute = PAttribute.create
  newAttribute.fkPTable(table).name(MyUtil.nameFromPattern(refTable.name,"fk")).validFrom(new Date).isCurrent(1).isDerivedFromModel(3)
  newAttribute.fkModelAttribute(0).isPrimaryKey(0).reference(findReferencingAttribute(refTable))
  MyUtil.setAttributePhysicalType(defaultKeyType, newAttribute).save
 }
 
 def cubeDimensions(v: ModelVertex): List[PTable] = {
  val copies = ModelVertex.findAll(By(ModelVertex.referenceId, v.id), By(ModelVertex.elementType, "dimension"))
  List.flatten(copies.map(ModelUtility.findOriginal).map(o => PTable.findAll(By(PTable.fkDimension, o.id))))
 }
  
 def createTable(vertex: ModelVertex, refs: List[PTable], kind: String) = {
  def findCorrespondingTables(v: ModelVertex, kind: String) = {
   if(kind == "dimension") PTable.findAll(By(PTable.fkScenario, SelectedScenario.is.id), By(PTable.fkDimension, vertex.id))
   else if(kind == "level") PTable.findAll(By(PTable.fkScenario, SelectedScenario.is.id), By(PTable.fkLevel, vertex.id))
   else PTable.findAll(By(PTable.fkScenario, SelectedScenario.is.id), By(PTable.fkCube, vertex.id))
  }
  
  def findCorrespondingAttributes(vertex: ModelVertex) = {
   if(vertex.elementType == "dimension") ModelVertex.findAll(By(ModelVertex.elementType, "attribute"), By(ModelVertex.referenceId, vertex.id), By(ModelVertex.isCurrent, 1))
   else ModelVertex.findAll(By(ModelVertex.elementType, "attribute"), By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.isCurrent, 1)).filter(v => MyUtil.isConnected(v.id, vertex.id))
  }
  
  def degenerated(vertex: ModelVertex) = {
	val degDims = ModelVertex.findAll(By(ModelVertex.elementType, "dimension"), 
			                          By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementKind, "original"), 
			                          NullRef(ModelVertex.validUntil), By(ModelVertex.isDegenerated, 1)).filter(v => MyUtil.isConnected(v.id, vertex.id))
			                          
    List.flatten(degDims.map(d => ModelVertex.findAll(By(ModelVertex.elementType, "attribute"), By(ModelVertex.referenceId, d.id))))			                          
  }
  
  def getPkId(t: PTable): Long = {
   val pk = PAttribute.findAll(By(PAttribute.isPrimaryKey, 1), By(PAttribute.fkPTable, t.id))
   if(pk.isEmpty) 0 else pk(0).id
  }
  
  val defaultKeyType = MyUtil.getSeqHeadText(Setup.is \\ "defaultKeyType")
  val defaultMeasureType = MyUtil.getSeqHeadText(Setup.is \\ "defaultMeasureType")
  val defaultAttributeType = MyUtil.getSeqHeadText(Setup.is \\ "defaultAttributeType")
	 
  val correspondingTable = findCorrespondingTables(vertex, kind)
  val attrs = findCorrespondingAttributes(vertex)
  
  if(correspondingTable.isEmpty) {

	 // create a new table and add attributes
	  
	 val newTable = PTable.create
	 newTable.name(vertex.elementName.replaceAll(" ", "_")).fkScenario(SelectedScenario.is).validFrom(new Date).isCurrent(1).isDerivedFromModel(1)
	 if(vertex.elementType == "dimension") newTable.fkDimension(vertex.id).tableType("dimension")
	 else if (vertex.elementType == "level") newTable.fkLevel(vertex.id).tableType("level")
	 else newTable.fkCube(vertex.id).tableType("cube")
	 
	 newTable.save
	 
	 // create primary key
	 
	 val pkAttribute = PAttribute.create
	 pkAttribute.fkPTable(newTable).name(MyUtil.nameFromPattern(newTable.name, "pk")).validFrom(new Date).isCurrent(1).isDerivedFromModel(2).fkModelAttribute(0).isPrimaryKey(1)
	 MyUtil.setAttributePhysicalType(defaultKeyType, pkAttribute).save 
	 
	 // add foreign keys for all referencing tables
	 
	 refs.map(ref => createReference(newTable, ref))
	 
	 // add all model attributes and measures
	 
	 if(vertex.elementType == "cube") degenerated(vertex).map(attr => createAttribute(newTable, attr, defaultAttributeType))
	 
	 if(kind != "accountFact") {
		 attrs.map(attr => createAttribute(newTable, attr, defaultAttributeType))
		 val msrs = Measure.findAll(By(Measure.fkCube, vertex.id), By(Measure.status, "approved")).filter(m => m.formula == null || m.formula.length == 0)
		 msrs.map(m => createAttributeFromMeasure(newTable, m, defaultMeasureType))
	 }
	 
	// add all additional attributes
	 
	MyUtil.addAdditionalAttributes(kind, newTable)
  }
  else {
	 
	 val referenceTable = correspondingTable.apply(0)
	 
	 // check primary key
	 
	 val givenPk =  PAttribute.findAll(By(PAttribute.fkPTable, referenceTable.id), By(PAttribute.isPrimaryKey, 1))
	 
	 if(givenPk.isEmpty) {
		 
		 val pkAttribute = PAttribute.create
		 pkAttribute.fkPTable(referenceTable).name(MyUtil.nameFromPattern(referenceTable.name, "pk")).validFrom(new Date).isCurrent(1).isDerivedFromModel(2).fkModelAttribute(0).isPrimaryKey(1)
		 MyUtil.setAttributePhysicalType(defaultKeyType, pkAttribute).save 
		 
	 }
	 else givenPk.apply(0).name(MyUtil.nameFromPattern(referenceTable.name, "pk")).save
	 
	 // add references that are missing
	 
	 val rAttr = List.flatten(refs.map(r =>  PAttribute.findAll(By(PAttribute.isPrimaryKey, 1), By(PAttribute.fkPTable, r.id)))).map(_.id.toLong)
	 
	 val missingReferences = refs.filter(ref => PAttribute.findAll(By(PAttribute.fkPTable, referenceTable.id), By(PAttribute.reference, getPkId(ref))).isEmpty)
	 missingReferences.map(ref => createReference(referenceTable, ref))
	 
	 // remove references that are not required any longer
	 
	 val superfluousReferences = PAttribute.findAll(By(PAttribute.fkPTable, referenceTable.id), By_>(PAttribute.reference, 0)).filter(pa => !rAttr.exists(_ == pa.reference.toLong))
	 superfluousReferences.map(pa => pa.validUntil(new Date).isCurrent(0).save)
	 
	 // check model attributes
	 
	 for(attr <- attrs ::: degenerated(vertex)) {
	  val correspondingAttribute = PAttribute.findAll(By(PAttribute.fkPTable, referenceTable.id), By(PAttribute.fkModelAttribute, attr.id), By(PAttribute.isCurrent, 1))
	  if(correspondingAttribute.isEmpty) createAttribute(referenceTable, attr, defaultAttributeType) else correspondingAttribute.apply(0).name(attr.elementName).save
	 }
	 
	 // add measures up to now not in the table
	 
	 val measuresOfCube = Measure.findAll(By(Measure.fkCube, vertex.id), By(Measure.status, "approved")).filter(m => m.formula == null || m.formula.length == 0)
	 
	 val missingMeasures = measuresOfCube.filter(m => PAttribute.findAll(By(PAttribute.fkPTable, referenceTable.id), By(PAttribute.fkMeasure, m.id)).isEmpty)
	 missingMeasures.map(m => createAttributeFromMeasure(referenceTable, m, defaultMeasureType))
	 
	 // remove measures that are not required any longer
	 
	 val superfluousMeasures = PAttribute.findAll(By(PAttribute.fkPTable, referenceTable.id), By_>(PAttribute.fkMeasure, 0)).filter(pa => !measuresOfCube.exists(_.id.toLong == pa.fkMeasure.toLong))
	 superfluousMeasures.map(pa => pa.validUntil(new Date).isCurrent(0).save)
	 
	 // mark unused attributes as old
	 
	 PAttribute.findAll(By(PAttribute.fkPTable, referenceTable.id), By(PAttribute.isDerivedFromModel, 1)).filter(pa => !attrs.exists(_.id == pa.fkModelAttribute)).map(pa => pa.validUntil(new Date).isCurrent(0).save)
  
	 // check additional attributes
	 
	 PAttribute.findAll(By(PAttribute.fkPTable, referenceTable.id), By(PAttribute.fkModelAttribute, -1)).map(t => t.delete_!)
	 MyUtil.addAdditionalAttributes(kind, referenceTable)
  }
 }
 
 def createTables(pivots: List[ModelVertex], remainder: HashSet[ModelVertex], tablesSoFar: List[PTable]): Unit = {
  def tableOfVertex(v: ModelVertex) = {
   val matchingTables = PTable.findAll(By(PTable.fkScenario, SelectedScenario.is), By(PTable.fkLevel, v.id))
   if(!matchingTables.isEmpty) matchingTables.apply(0) else null
  }
  
  val nextPivots = List.flatten(pivots.map(p => createSnowflakeTable(p, remainder.toList, tablesSoFar))).distinct
  val newRemainder = remainder -- pivots
  if(!nextPivots.isEmpty) createTables(nextPivots, newRemainder, pivots.map(tableOfVertex).filter(_ != null).toList ::: tablesSoFar)  
 }
 
 def createSnowflakeTable(pivot: ModelVertex, otherLevel: List[ModelVertex], tablesSoFar: List[PTable]) = {
  val neighbours = otherLevel.filter(item => MyUtil.isConnected(item.id, pivot.id))
  createTable(pivot, tablesSoFar.filter(t => MyUtil.isConnected(t.fkLevel, pivot.id)), "level")
  neighbours	 
 }
 
 def createMeasureDimension(dimensionName: String) = {
	 
	 val candidates = PTable.findAll(By(PTable.fkScenario, SelectedScenario.is), By(PTable.tableType, "measureDimension"))
	 if(!candidates.isEmpty) candidates.apply(0).name(dimensionName).save
	 else {
		 
		 // create table
		 val dim = PTable.create
		 dim.name(dimensionName).fkScenario(SelectedScenario.is).validFrom(new Date).isCurrent(1).isDerivedFromModel(1).save
		 
		 val defaultKeyType = MyUtil.getSeqHeadText(Setup.is \\ "defaultKeyType")
		 val defaultMeasureType = MyUtil.getSeqHeadText(Setup.is \\ "defaultMeasureType")
		 val defaultAttributeType = MyUtil.getSeqHeadText(Setup.is \\ "defaultAttributeType")
		 
		 // add primary key
		 val pkAttribute = PAttribute.create
		 pkAttribute.fkPTable(dim).name(MyUtil.nameFromPattern(dim.name, "pk")).validFrom(new Date).isCurrent(1).isDerivedFromModel(1).fkModelAttribute(0).isPrimaryKey(1)
		 MyUtil.setAttributePhysicalType(defaultKeyType, pkAttribute).save 
		 
		 // add measure name
		 
		 val measureName = PAttribute.create
		 measureName.fkPTable(dim).name("MEASURE_NAME").validFrom(new Date).isCurrent(1).isDerivedFromModel(1).fkModelAttribute(0).isPrimaryKey(1)
		 MyUtil.setAttributePhysicalType(defaultAttributeType, measureName).save 
	 
		 // add additional attributes
		 MyUtil.addAdditionalAttributes("dimension", dim)	 
	 } 
 }
 
 def deriveFromLogic() : JsCmd = {
  
  // get setup information
   
  val setup = Repository.read("scenario", SelectedScenario.is.id, "setup","setup", -1) \\ "setup"
  val snowflakeChoice = MyUtil.getSeqHeadText(setup \\ "snowflake")  // either star or snowflake
  val accountChoice = MyUtil.getSeqHeadText(setup \\ "accountModel") // either account or noAccount
  val measureDimensionName = MyUtil.getSeqHeadText(setup \\ "nameDimMeasure") // either account or noAccount
   
  val dimensions = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "dimension"), 
		  By(ModelVertex.elementKind, "original"), NullRef(ModelVertex.validUntil), NotBy(ModelVertex.isDegenerated, 1))
   
  for(dimension <- dimensions) {
	 
	if(snowflakeChoice == "snowflake") {
	 val levelOfDimension = ModelVertex.findAll(By(ModelVertex.elementType, "level"), By(ModelVertex.referenceId, dimension.referenceId), By(ModelVertex.elementKind, "original"), NullRef(ModelVertex.validUntil))
	 val hierarchies = ModelVertex.findAll(By(ModelVertex.elementType, "hierarchy"), By(ModelVertex.referenceId, dimension.referenceId), By(ModelVertex.elementKind, "original"), NullRef(ModelVertex.validUntil))
	 
	 val hierarchiesAndLevel = hierarchies.map(h => findLevel(h, levelOfDimension))
	 val leaves = hierarchiesAndLevel.filter(_.size > 0).map(_.last).toList.distinct
	 val levelAsSet = new HashSet[ModelVertex]
	 for(l <- levelOfDimension) levelAsSet += l
	 
	 createTables(leaves, levelAsSet, Nil) 
	}
	else createTable(dimension, Nil, "dimension")
  }
  
  if(accountChoice == "accountModel") createMeasureDimension(measureDimensionName)
  
  val cubes = ModelVertex.findAll(By(ModelVertex.fkScenario, SelectedScenario.is.id), By(ModelVertex.elementType, "cube"), By(ModelVertex.elementKind, "original"), NullRef(ModelVertex.validUntil))
  
  for(cube <- cubes) {
   if(accountChoice == "accountModel") createTable(cube, cubeDimensions(cube), "accountFact") else createTable(cube, cubeDimensions(cube), "fact")
  }
   
  RedirectTo("attribute")  
 }
  
 def work (xhtml: NodeSeq): NodeSeq = {
  def showHeader() = {
	  if(SelectedTable.is != null){
			 PTable.findAll(By(PTable.id, SelectedTable.is.id.is)).map(_.header).apply(0)
	  }
	  else{
	 	  val tables = PTable.findAll(By(PTable.fkScenario, SelectedScenario.is))
	 	  if(!tables.isEmpty) tables.apply(0).header else <span>{S.?("noPhysicalTable")}</span>
	  }
  }
  
  def showRows() = {
	  if(SelectedTable.is != null){
		  List.flatten(PAttribute.findAll(By(PAttribute.fkPTable, SelectedTable.is.id.is), By(PAttribute.isCurrent, 1), OrderBy(PAttribute.id, Ascending)).map(_.tr2.toList)).toSeq
	  }
	  else{
	 	  val tables = PTable.findAll(By(PTable.fkScenario, SelectedScenario.is))
	 	  if(!tables.isEmpty) {
	 	 	  List.flatten(PAttribute.findAll(By(PAttribute.fkPTable, tables.apply(0).id.is), By(PAttribute.isCurrent, 1), OrderBy(PAttribute.id, Ascending)).map(_.tr2.toList)).toSeq
	 	  }
	 	  else <span/>
	  }
  }
  
  def copyTable(): JsCmd = {
	   RedirectTo("/tableCopy")
  }
  
  def empty(): JsCmd = Noop
  
  val addButton = if(MyUtil.isDesigner()) ajaxButton(S.?("addTable"), addTable _) % ("class" -> "standardButton") else ajaxButton(S.?("addTable"), empty _) % ("class" -> "standardButton") % ("disabled" -> "")
  val deriveButton = if(MyUtil.isDesigner()) ajaxButton(S.?("deriveFromLogic"), deriveFromLogic _) % ("class" -> "standardButton") else ajaxButton(S.?("deriveFromLogic"), empty _) % ("class" -> "standardButton") % ("disabled" -> "")
  val copyButton = if(MyUtil.isDesigner()) ajaxButton(S.?("copyTable"), copyTable _) % ("class" -> "standardButton") else ajaxButton(S.?("copyTable"), empty _) % ("class" -> "standardButton") % ("disabled" -> "")
		
  bind("physical", xhtml, "add" -> addButton,
		  				  "derive" -> deriveButton,
		  				  "copy" -> copyButton,
		                  "header" -> showHeader(),
		                  "overview" -> PTable.findAll(By(PTable.fkScenario,SelectedScenario.is.id)).map(createListItem).toSeq,
		                  "rows" -> showRows())
  }
  
 def copyForm (xhtml: NodeSeq): NodeSeq = {
	 bind("copy", xhtml, "original" -> <h3>{S.?("copyTableOriginal") + " " + SelectedTable.is.name}</h3>)
 }
 
}