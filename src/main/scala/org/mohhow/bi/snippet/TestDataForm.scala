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
  val attrs = PAttribute.findAll(By(PAttribute.fkPTable, pt.id))
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
  
 def finish() {
  def detail(m: ModelVertex) = if(m == null) "" else m.elementDetail
  def kind(pa: PAttribute, m: ModelVertex) = {
   if (pa.isPrimaryKey > 0) "pk"
   else if (pa.reference > 0) "fk"
   else if (m != null && m.elementDetail != null && m.elementDetail.split(";").size > 1) "choice"
   else "pattern"
  }
  
  def date(d: String) = MyUtil.dateAsNumber(MyUtil.asDate(d))
  
  val repositoryAnswer = Repository.prepareTestData(testDataName)
   
  if(repositoryAnswer._1) {
	
	 val tablesAndModels =  TestDataModel.is   
	 val tableSizes = Map.empty[String, Int];
	 
	 //val store = (TestDataNode.is \\ "store").filter(st => MyUtil.getSeqHeadText(st \ "alias") == SelectedAlias.is)
	 //if(!store.isEmpty) {
		 
		 //val connectionInformation = createVendor(MyUtil.getSeqHeadText(store \ "storeDriver"), MyUtil.getSeqHeadText(store \ "storeUrl"), MyUtil.getSeqHeadText(store \ "storeUser"), MyUtil.getSeqHeadText(store \ "storePassword"), SelectedAlias.is)
	 
		 for(tm <- tablesAndModels) {
			 
			 val tableName = tm._1.name.toString
			 println(tableName) 
			 if(tm._1.tableType == "dateDimension") {
				 
			 }
			 else if(tm._1.tableType == "measureDimension") {
				 
			 }
			 else {
				 
				 val reducedSize = ModelUtility.computeReducedSize(tm._2, tableName, date(periodEndField.toString), date(periodStartField.toString), date(initialLoadDate.toString), percentOfLoad.toString.toLong)
				 tableSizes += (tableName -> reducedSize)
		         println("tablesizes is " + tableSizes.toString)
				 val attrs = getPAttributes(tm._1)
				 val enrichedAttrs = attrs zip attrs.map(attr => getAttributeModel(attr))
				 val metadata = enrichedAttrs.map(m => (kind(m._1, m._2).toString , m._1.isPartOfUniqueKey > 0, detail(m._2).toString))
				 println(metadata.toString)
				 val ops = metadata.map(item => TestDataUtility.defineOperator(item, tableSizes))
				 println(ops.toString)
				 val content = MyUtil.csv(List.range(1, reducedSize).map(i => ops.map(op => op(i).toString)))
				 val fileName = Repository.storeFlatFile(testDataName, tableName, content)
				 /*
				 val truncateCommand = "TRUNCATE TABLE " + tableName + ";"
				 val loadCommand = "LOAD DATA INFILE '" + fileName + "' INTO TABLE " + tableName + ";"
		 
				 DB.use(connectionInformation) { conn => DB.exec(conn, truncateCommand) {rs => "success"}}
				 DB.use(connectionInformation) { conn => DB.exec(conn, loadCommand) {rs => "success"}}  */
			 }
		 } 
	 //}
  }
  else Alert(S.?(repositoryAnswer._2))
 } 
}