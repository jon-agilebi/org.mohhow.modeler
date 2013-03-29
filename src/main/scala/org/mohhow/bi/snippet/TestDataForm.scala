package org.mohhow.snippet

import net.liftweb._
import http._
import util._
import util.Helpers._
import common._
import scala.xml._
import org.mohhow.model._
import java.util.Date
import org.mohhow.bi.lib.Repository
import org.mohhow.bi.lib.TestDataUtility
import java.math.BigDecimal

import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib._

import mapper._
import util._
import Helpers._

import js._
import JsCmds._
import scala.collection.mutable._

object TestDataForm extends LiftScreen {
	
 def checkPercent(digits: String): List[FieldError] = {
	 try {
		 val percent = new BigDecimal(digits)
		 if(percent.floatValue() < 0 || percent.floatValue() > 100) S.?("outOfPercentageRange") else Nil
	 }
	 catch {
		 case e: Exception =>  S.?("notANumber")
	 } 
 }
 
 val initialLoadDateField = new Field { 
    type ValueType = String 
    override def name = S.?("initialLoadDate") 
    lazy val manifest = buildIt[String]
    override def default = ""
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
 } 	
	
 val reportingDateField = new Field { 
    type ValueType = String 
    override def name = S.?("reportingDate") 
    lazy val manifest = buildIt[String]
    override def default = ""
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
 } 	
 
 val periodStartField = new Field { 
    type ValueType = String 
    override def name = S.?("periodStart") 
    lazy val manifest = buildIt[String]
    override def default = ""
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
 } 	
 
 val periodEndField = new Field { 
    type ValueType = String 
    override def name = S.?("periodEnd") 
    lazy val manifest = buildIt[String]
    override def default = ""
    override def toForm: Box[NodeSeq] =  SHtml.text(is, set _, "class" -> "dateInput") 
 } 	
	
 val initialLoadDate = initialLoadDateField
 val reportingDate = reportingDateField
 val periodStart = periodStartField
 val periodEnd = periodEndField
 val percentOfLoad = field(S.?("percentOfLoad"), "", checkPercent _)
 val testDataName = field(S.?("testDataName"), "")
 
 def getPAttributes(pt: PTable): List[PAttribute] = {
  val attrs = PAttribute.findAll(By(PAttribute.fkPTable, pt.id), OrderBy(PAttribute.id, Ascending))
  //attrs.filter(attr => MyUtil.dateAsNumber(attr.validFrom) <= MyUtil.dateAsNumber(r.freezeDate) && (attr.validUntil == null || MyUtil.dateAsNumber(attr.validUntil) > MyUtil.dateAsNumber(r.freezeDate))).toList	 
  attrs.toList 
 }

 def getAttributeModel(pa: PAttribute): ModelVertex = if(pa.isDerivedFromModel == 1) ModelVertex.findAll(By(ModelVertex.id, pa.fkModelAttribute)).apply(0) else null
 
 def vertexGrowth(v: ModelVertex): (Long, Long) = {
  if(v!= null && v.elementDetail != null && v.elementDetail.split(";").size == 2) {
	  val prms = v.elementDetail.split(";")
	  (prms(0).toLong, prms(1).toLong)
  }
  else (0,0)
 }
  
 override def cancelButton = <button>{S.?("cancel")}</button>
 override def finishButton = <button>{S.?("finish")}</button>
 
 def createVendor(driver: String, url: String, user: String, pwd: String, aliasName: String) = {
  val vendor = new StandardDBVendor(driver, url, Full(user), Full(pwd)) 
  val connInf = new ConnectionInformation(aliasName)
  DB.defineConnectionManager(connInf, vendor)
  LiftRules.unloadHooks.append(() => vendor.closeAllConnections_!())
  connInf
 }
 
 def detail(m: ModelVertex, pa: PAttribute) = {
  if(pa.fkMeasure > 0) {
	  val msrs = Measure.findAll(By(Measure.id, pa.fkMeasure))
	  if(msrs.size == 1) {
	 	  val mockup = msrs(0).mockup.toString
	 	  if(mockup != null && mockup.length > 0 && mockup.startsWith("rnd")) mockup.substring(4, mockup.length - 1)
	 	  else ""
	  }
  }
  else if (pa.reference > 0) {
   val attr = PAttribute.findAll(By(PAttribute.id, pa.reference)).apply(0)
   val table = PTable.findAll(By(PTable.id, attr.fkPTable)).apply(0)
   table.name 
  }
  else if(pa.fkModelAttribute == -1) pa.dataType
  else if(m == null) "" 
  else m.elementDetail
 }
 
 def kind(pa: PAttribute, m: ModelVertex, tableKind: String): String = {
  def refDate(refId: Long) = PTable.findAll(By(PTable.id, PAttribute.findAll(By(PAttribute.id, refId)).apply(0).fkPTable)).apply(0).tableType == "dateDimension" 
 
  if (pa.isPrimaryKey > 0) "pk"
  else if (pa.reference > 0 && refDate(pa.reference)) "fkDate"
  else if (pa.reference > 0) "fk"
  else if (tableKind == "dateDimension" && pa.fkModelAttribute > 0) "time"
  else if (tableKind == "measureDimension" && pa.isDerivedFromModel == 1) "measure"
  else if (pa.fkMeasure > 0 ) "rnd"
  else if(pa.fkModelAttribute == -1) "metadata"
  else if (m != null && m.elementDetail != null && m.elementDetail.split(";").size > 1) "choice"
  else if (m != null && m.elementDetail != null) "pattern"
  else "null"
 }
 
 def deployContent(testDataName: String, table: PTable, lowerBound: Int, upperBound: Int, tableSizes: Map[String, Int], tableKind:String, alias: String, initialLoad: Long) = { 
  println("now deploy " + table.name)
  val attrs = getPAttributes(table)
  val enrichedAttrs = attrs zip attrs.map(attr => getAttributeModel(attr))
  val metadata = enrichedAttrs.map(m => (kind(m._1, m._2, tableKind).toString , m._1.isPartOfUniqueKey > 0, detail(m._2, m._1).toString))
  val msrs = Measure.findAll(By(Measure.status, "approved")).map(_.shortName.toString).toList.sort(_ < _)
  val ops = metadata.map(item => TestDataUtility.defineOperator(item, msrs, tableSizes, BIService.storeMap(alias)._3, initialLoad.toInt))
  val ub = if(upperBound < lowerBound) msrs.size else upperBound
  
  val theRange = if(table.tableType == "dateDimension") MyUtil.allDays(lowerBound,ub, Nil).map(_.toInt) else List.range(lowerBound, ub)
  val content = MyUtil.csv(theRange.map(i => ops.map(op => op(i).toString)))
  val fileName = Repository.storeFlatFile(testDataName, table.name, content)
				 
  val truncateCommand = "TRUNCATE TABLE " + table.name + " CASCADE;"
  val loadCommand = BIService.storeMap(alias)._4.replaceAll("tableName", table.name).replaceAll("fileName", fileName)
  
  val releases = (TestDataNode.is \\ "deployment" \ "release").map(r => ((r \ "@releaseId").text, MyUtil.getSeqHeadText(r))).toList
  val connectionInformation = BIService.storeMap(alias)._1
  
  try{
	  DB.runUpdate(truncateCommand, Nil, connectionInformation)
	  DB.runUpdate(loadCommand, Nil, connectionInformation)
  }
  catch {
	  case e:Exception => println(e.toString)
  }
 }
 
 def getRightAlias(t: PTable, releaseMap: List[(String, String)]): Option[String] = {
  val rls = Release.findAll(By(Release.fkScenario, t.fkScenario))
  if(rls.isEmpty) None 
  else {
	  val matchingItem = releaseMap.filter(_._1 == rls(0).id.toString)
	  if(matchingItem.isEmpty) None else Some(matchingItem(0)._2)
  }
 }
  
 def finish() {
  
  def date(d: String) = MyUtil.dateAsNumber(MyUtil.asDate(d))
  
  val repositoryAnswer = Repository.prepareTestData(testDataName)
   
  if(repositoryAnswer._1) {
	
	 val tablesAndModels =  TestDataModel.is   
	 val tableSizes = Map.empty[String, Int];
	 val releases = (TestDataNode.is \\ "deployment" \ "release").map(r => ((r \ "@releaseId").text, MyUtil.getSeqHeadText(r))).toList
	 
	 for(tm <- tablesAndModels) {
		 
		 // first loop: compute table sizes
		 
		 val tableName = tm._1.name.toString
		 
		 if(tm._1.tableType == "dateDimension") tableSizes += (tableName -> List(date(periodEndField.toString).toInt, date(reportingDate.toString).toInt).max)
		 else {
			 val reducedSize = ModelUtility.computeReducedSize(tm._2, tableName, date(periodEndField.toString), date(periodStartField.toString), date(initialLoadDate.toString), percentOfLoad.toString.toLong)
			 tableSizes += (tableName -> reducedSize)
	 	 }
	 }
	 
	 for(tm <- tablesAndModels) {
		 
	  // second loop: create files and make a TRUNCATE+INSERT
			 
	  val tableName = tm._1.name.toString
	  
	  val maybeAlias = getRightAlias(tm._1, releases)
	  
	  maybeAlias match {
	  
	 	  case None => Noop
	 	  case Some(alias) => {
	  
			  if(tm._1.tableType == "dateDimension") {
			  	  
			 	  val end = MyUtil.year(date(periodEndField.toString).toInt) * 10000 + 1231 
			 	  val start = MyUtil.year(date(initialLoadDate.toString).toInt) * 10000 + 101
			 	  
			 	  deployContent(testDataName, tm._1, start, end, tableSizes, "dateDimension", alias, date(initialLoadDate.toString))	 
			  }
			  else if(tm._1.tableType == "measureDimension") deployContent(testDataName, tm._1, 1, -1, tableSizes, "measureDimension", alias, 1)
			  else deployContent(testDataName, tm._1, 1, tableSizes(tm._1.name), tableSizes, "otherCases", alias, date(initialLoadDate.toString))
	 	   
	 	 }
	  }
	 }
  }
  else Alert(S.?(repositoryAnswer._2))
 } 
}