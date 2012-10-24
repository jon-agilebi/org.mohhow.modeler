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
 
 def artefacts(): NodeSeq = {
  
  def showArtefact(text: String) = Alert(Repository.getArtefactAsString(SelectedRelease.is.id, text))	 

  def ddls(r: Release) = {
   val ddlsOfRelease = Repository.getArtefactList(r.id, List("sql"))
   val action = SHtml.ajaxCall(JsRaw("$(this).text()"), showArtefact _)._2
   if(ddlsOfRelease.isEmpty) NodeSeq.Empty 
   else <ul style="list-style-position:inside">{ddlsOfRelease.map(ddl => <li>{ddl}</li> % ("onclick" -> action))}</ul>
  }

  def metadata(r: Release) = {
   val artefact = Repository.getArtefactList(r.id, List("xml"))
   val action = SHtml.ajaxCall(JsRaw("$(this).text()"), showArtefact _)._2
   if(artefact.isEmpty) NodeSeq.Empty 
   else <span>{artefact}</span> % ("onclick" -> action)
  }
	 
  val release = SelectedRelease.is

  if(release == null) <nothing />
  else {
	   <div>
	   	 <h4>{S.?("metadata")}</h4>
	   		<br />
	  			{metadata(release)}
	  		<br /><br />
	   	<h4>DDL</h4>
	  		<br />
	  			{ddls(release)}
	  		<br />
	   </div>
  }
 }
 
 def delta(): NodeSeq = {
   val release = SelectedRelease.is

   if(release == null) <nothing />
   else {
	   <div>
	   	<br />
	   	<h4>DDL</h4>
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
 
 def createDDL(table: PTable) = {
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
  
  def keyConstraint(table: PTable, isPrimaryConstraint: Boolean) = { 
   def attrList(isPrimaryConstraint: Boolean) = {
	   if(isPrimaryConstraint) PAttribute.findAll(By(PAttribute.fkPTable, table.id)).filter(_.isPrimaryKey.toLong == 1).map(a => a.name.toString)
	   else PAttribute.findAll(By(PAttribute.fkPTable, table.id)).filter(_.isPartOfUniqueKey.toLong == 1).map(a => a.name.toString)
   }
   def keyword(isPrimaryConstraint: Boolean) = if(isPrimaryConstraint)  " PRIMARY KEY (" else " UNIQUE ("
   def prefix(isPrimaryConstraint: Boolean) = if(isPrimaryConstraint)  "PK_" else " UQ_"
	   
   
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
  
  val rows = commaList(PAttribute.findAll(By(PAttribute.fkPTable, table.id), By(PAttribute.isCurrent, 1)).map(createLine)) 
  val completeDDL = "CREATE TABLE " + table.name + "(" + rows + ");\n\n" + keyConstraint(table, true) + "\n\n" + keyConstraint(table, false)
  Repository.writeDDL(SelectedRelease.is.id, table.name, completeDDL)
 }
 
 def createSelects(bl: Node): Node = {
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
  
  def findAttribute(name: String): ModelVertex = {
   val attrs = ModelVertex.findAll(By(ModelVertex.elementType, "attribute"), By(ModelVertex.elementName, name), By(ModelVertex.fkScenario, SelectedScenario.is.id))	  
   if(attrs.isEmpty) null else attrs(0)
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
  
  def sql(fact: Option[PTable], msrs: List[Measure], attrs: List[(ModelVertex, String)] ): Node = {
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
		  <sql>{ModelUtility.createSelect(augmentedAttrs, augmentedMsrs, aTable(fact), "")}</sql>
   </select>
  }
  
  def aMeasure(m: Option[Measure]): Measure = m match {
	   case None => null
	   case Some(msr) => msr
   }
  
  val blockId = (bl \ "@blockId").text
  val blockType = MyUtil.getSeqHeadText(bl \ "presentationType") 
  val attrs = (bl \\ "attribute").map(a => (findAttribute(MyUtil.getSeqHeadText(a \ "name")), setOrder(MyUtil.getSeqHeadText(a \ "order")))).filter(_._1 != null)
  val msrs = (bl \\ "measure").map(m => findMeasure(MyUtil.getNodeText(m)))
  val relevantMeasures = List.flatten(msrs.toList.map(getRelevantMeasures)).toList
  val factTables = relevantMeasures.map(msr => findPhysics(msr.id, false)).map(_._2).distinct
  
  val attrXML = attrs.map(attr => <attribute><id>{attr._1.id.toString}</id><order>{attr._2}</order><emphasize></emphasize></attribute>).toSeq
  val msrXML = relevantMeasures.map(msr => <measure><id>{msr.id.toString}</id><formula>{msr.formula.toString}</formula></measure>).toSeq
  val sqlXML = factTables.map(fact => sql(fact, msrs.map(aMeasure).toList, attrs.toList))
  
  <block id={blockId}>
		<blockType>{blockType}</blockType>
        <structure>
			{attrXML}
            {msrXML}
        </structure>
		<selects>
            {sqlXML}
		</selects>
  </block> 
 }

 def createMetadata() = {
  val blocks = (Repository.read("scenario", SelectedScenario.is.id, "blocks", "blocks", -1) \\ "block").filter(b => MyUtil.getSeqHeadText(b \\ "presentationType") != null && MyUtil.getSeqHeadText(b \\ "presentationType").length > 0)
  val frames = (Repository.read("scenario", SelectedScenario.is.id, "frames", "frames", -1) \\ "fr").filter(fr => (fr \ "*").size > 0)

  val metadata = <metadata>
	  				<frames>{frames}</frames>
  					<layout>{Setup.is \\ "layout"}</layout>
	  				<blocks>{blocks}</blocks>
	  				<backend>{blocks.map(createSelects)}</backend>
	  		     </metadata>  % new UnprefixedAttribute("scenarioId", SelectedScenario.is.id.toString, Null)
  
  Repository.write("release", 0, null, "metadata", SelectedRelease.is.id, metadata)
 }
 
 def generateRelease(): JsCmd = {
  if(SelectedRelease.is != null) {
	Repository.emptyRelease(SelectedRelease.is.id.toLong)
	PTable.findAll(By(PTable.fkScenario, SelectedScenario.is.id), By(PTable.isCurrent, 1)).map(createDDL)
	createMetadata()
	Noop
  }
  else Alert(S.?("noReleaseChosen"))
 }

 def menu (xhtml: NodeSeq): NodeSeq = {
  bind("menu", xhtml, "create"  -> ajaxButton(S.?("createNewRelease"), createNewRelease _) % ("class" -> "standardButton"),
		              "announce" -> ajaxButton(S.?("announceReleaseFreeze"), announce _) % ("class" -> "standardButton"),
		              "freeze" -> ajaxButton(S.?("freeze"), freeze _) % ("class" -> "standardButton"),
		              "generate" -> ajaxButton(S.?("generate"), generateRelease _) % ("class" -> "standardButton"),
		              "show" -> Release.findAll().map(createListItem),
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
  val releaseIds =  MyUtil.getSeqHeadText(n \\ "deployment").split(";").toList.filter(r => r != null && r.length > 0)
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
 
 def createVendor(driver: String, url: String, user: String, pwd: String, aliasName: String) = {
  val vendor = new StandardDBVendor(driver, url, Full(user), Full(pwd)) 
  val connInf = new ConnectionInformation(aliasName)
  DB.defineConnectionManager(connInf, vendor)
  LiftRules.unloadHooks.append(() => vendor.closeAllConnections_!())
  connInf
 }
 
 def dropTablesOfRelease(r: Release, n: Node, aliasName: String) = {
  val tableNames = Repository.getArtefactList(r.id, List("sql")).map(fileName => fileName.substring(0, fileName.indexOf(".")))
  val store = (n \\ "store").filter(st => MyUtil.getSeqHeadText(st \ "alias") == aliasName)
  if(!store.isEmpty) {
	  val connectionInformation = createVendor(MyUtil.getSeqHeadText(store \ "storeDriver"), MyUtil.getSeqHeadText(store \ "storeUrl"), MyUtil.getSeqHeadText(store \ "storeUser"), MyUtil.getSeqHeadText(store \ "storePassword"), aliasName)
	  tableNames.map(name => DB.use(connectionInformation) { conn => DB.exec(conn, "DROP TABLE " + name + ";") {rs => "success"}})
  } 	 
 }
 
 def createTablesOfRelease(r: Release, n: Node, aliasName: String) = {
  val fileNames = Repository.getArtefactList(r.id, List("sql"))
  val store = (n \\ "store").filter(st => MyUtil.getSeqHeadText(st \ "alias") == aliasName)
  if(!store.isEmpty) {
	  val connectionInformation = createVendor(MyUtil.getSeqHeadText(store \ "storeDriver"), MyUtil.getSeqHeadText(store \ "storeUrl"), MyUtil.getSeqHeadText(store \ "storeUser"), MyUtil.getSeqHeadText(store \ "storePassword"), aliasName)
	  fileNames.map(name => DB.use(connectionInformation) { conn => DB.exec(conn, Repository.getArtefactAsString(r.id, name)) {rs => "success"}})
  } 
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
 
 def transfer(url:String, kind: String, name: String, mode: String, node: Node) = "$.post(" + url + "/deployment/transfer/" + kind + "/" +  name + "/" + mode + ", " + node.toString + ", function(resp){});"
 
 def updateDeploymentOnNode(node: Node, oldRelease: Release, newRelease: Release, systemName: String): Node = {
  val releases = (node \\ "deployment" \ "release").map(r => ((r \ "@releaseId").text, MyUtil.getNodeText(r)))
  val newReleases = if(oldRelease != null) (newRelease.id.toString, systemName) :: releases.filter(_._1 != oldRelease.id.toString).toList else (newRelease.id.toString, systemName) :: releases.toList
  val deployment = <deployment>{newReleases.map(r => <release releaseId={r._1}>{r._2}</release>).toSeq}</deployment>
  val transform = "deployment" #> deployment
  transform(node).apply(0)
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
 
  Noop
  
  /*
 
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
  
  val deploymentInformation = <items>{deploymentItems.map(item => <item><name>{item._1}</name><checksum>{item._2}</checksum></item>)}</items>
  val url = "http://" + MyUtil.getSeqHeadText(n \\ "host") + ":" + MyUtil.getSeqHeadText(n \\ "port")
  val startCommand = "$.post(" + url + "/deployment/start, " + deploymentInformation.toString + ", function(resp){});"
  val transferCommands = deploymentItems.map(item => transfer(url, item._3, item._1, "complete", item._4)).toList
  
  // node transformation
  
  val allNodes = Repository.read("configuration", -1, "nodes", "nodes",0).map(Utility.trim) \\ "node"
  val old = if(!oldRelease.isEmpty) oldRelease(0) else null
  val searchTerm = "technicalName = " + (n \ "@technicalName").text
  val transform = searchTerm #> updateDeploymentOnNode(n, old, targetRelease, SelectedAlias.is)
  val transformedNodes = <nodes>{transform(allNodes)}</nodes>
  
  Repository.write("configuration", -1, "nodes", "nodes", 0, transformedNodes)
  Nodes(transformedNodes)
  
  // fire the Java command
  JsRaw((startCommand /: transferCommands) (_ + _))  */
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
  val psInTime = ps.filter(pt => MyUtil.dateAsNumber(pt.validFrom) <= MyUtil.dateAsNumber(r.freezeDate) && (pt.validUntil == null || MyUtil.dateAsNumber(pt.validUntil) > MyUtil.dateAsNumber(r.freezeDate)))
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
	 	  /*
	 	  val releases = releasesOfNode(devNode.apply(0))
	 	  
	 	  if(releases.isEmpty) Alert(S.?("noReleasesOnNode"))
	 	  else { 	  
	 	
	 	 	  val tableNames = releases.map(r => (r, Repository.tableNamesOfRelease(r.id).toList)).toList
	 	 	  val tables = List.flatten(tableNames.map(pair => pair._2.map(tn => getPTable(tn, pair._1)).filter(_ != null))).sort(sortTable(_) < sortTable(_))
	 	 	  val tablesAndModels = tables zip tables.map(getTableModel)
	 	 	  
	 	 	  TestDataNode(devNode.apply(0))
	 	 	  TestDataModel(tablesAndModels)
	 	 	   	 	  
	 	 	  RedirectTo("/testData")
	 	  } */
	 	  RedirectTo("/testData")
	  }
  }
  else Alert(S.?("noSystemChosen"))
 }
  
 def estimate():NodeSeq = {
  def tableInformation(tableName: String, initialSize: String, growth: String) = {
	  <label><b>{S.?("tableName")}:</b></label><span>{tableName}</span><br />
	  <label><b>{S.?("initialSize")}: </b></label><span>{initialSize}</span><br />
	  <label><b>{S.?("growth")}: </b></label><span>{growth}</span><br />
  }
  
  val model = TestDataModel.is.partition(tm => tm._1.tableType == "dimension" || tm._1.tableType == "level" || tm._1.tableType == "timeDimension" || tm._1.tableType == "accountDimension")
   	 
	 <div class="message">
   		<p class="messageTitle">{S.?("sizeEstimation")}</p>
   		 <div>
   			<h4>{S.?("dimensions")}</h4><br /><br />
   			{model._1.map(tm => tableInformation(tm._1.name, ModelUtility.initialSize(tm._2), ModelUtility.growth(tm._2)))}
   			<h4>{S.?("facts")}</h4><br /><br />
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