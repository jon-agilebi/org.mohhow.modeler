package org.mohhow.bi.lib

import net.liftweb._
import common._
import http._
import rest._
import scala.xml._
import java.lang.Math._
import java.math.BigDecimal
import scala.collection.mutable._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib.{ConnectionInformation => CI}

object BIServiceUser extends RequestVar[String]("")

object BIService extends RestHelper{
  
 // variable objects that store all required session informations
	
 var isInitialized: Boolean = false
 var userMap = Map.empty[String, (String, String, List[Long])];
 var storeMap = Map.empty[String, CI];
 var blockMap = Map.empty[Long, Node];
 var sessionMemory = Map.empty[String, List[Node]]
 
 def txt(node: Node, tag: String) = MyUtil.getSeqHeadText(node \\ tag)
 
 def initializeBIService() {
  sessionMemory = Map.empty[String, List[Node]]
  
  val allUser = Repository.read("deployment", 0, "generic", "user", 0) \\ "user"
  userMap = Map.empty[String, (String, String, List[Long])];
  
  for(aUser <- allUser) userMap += (txt(aUser, "id") -> (txt(aUser, "checksum"), txt(aUser, "metadata"), txt(aUser, "blocks").substring(1).split("b").map(_.toLong).toList))
  
  val allStores = Repository.read("deployment", 0, "generic", "stores", 0) \\ "store"
  
  val allBlocks = Repository.read("deployment", 0, "generic", "blocks", 0) \\ "block"
  blockMap = Map.empty[Long, Node];
  
  for(aBlock <- allBlocks) {
	  blockMap += ((aBlock \\ "@id").text.toLong -> aBlock)
  }
  
  isInitialized = true
 }
 
 // helper method for isPartOf
 
 def isIn(x: String, l: List[String]): Boolean = l match {
  case Nil => false
  case head :: tail => if (x == head) true else isIn(x, tail)
 }
 
 // check whether left list is part of right list
 
 def isPartOf(l1: List[String], l2: List[String]) = (true /: l1.map(x => isIn(x, l2))) (_ && _)
 
 // wrapper method for DB.runQuery
 
 def query(sql:String, system:String):(List[String],List[List[String]]) = DB.runQuery(sql, Nil, storeMap(system))
 
 // check whether the given checksum matches the user's checksum
 
 def isActual(checksum: String): Boolean = userMap(BIServiceUser.is)._1 == checksum
 
 // set the parameter of the SQL statement
 
 def setSQLParameter(sql: String, filter: NodeSeq): String = {
  def replaceQuestionMarks(select: String, prms: List[String]): String = prms match {
	  case Nil => select
	  case head :: tail => {	  
	 	  val qm = """\?""".r 
	 	  val firstMatch = qm findFirstIn select
   
	 	  firstMatch match {
	 	  	case None => select
	 	  	case Some(m) => replaceQuestionMarks(qm replaceFirstIn(select, head) ,tail)  	   
	 	  }
	  }
  }
  
  if(filter == null || filter.isEmpty) sql 
  else {
	val filterList = filter.map(f => MyUtil.getSeqHeadText(f)).toList
	replaceQuestionMarks(sql, filterList)
  }
 }
 
 // main processing method
 
 def ask(blockIds: List[Long], filter: NodeSeq): List[Node] = {
  
  def getText(queryResult: (List[String], List[List[String]])) = if(!queryResult._2.isEmpty && !queryResult._2.head.isEmpty) queryResult._2.head.head else ""
  
  def first(l: List[Any],x:Any, index: Int): Int = l match {
   case Nil => -1
   case h :: tail => if(h == x) index else first(tail, x, index + 1)
  }
	  
  def extractColumn(matrix: List[List[String]], n: Int):List[List[String]] = matrix.map(xs => xs take n).toList
  def extractSelectedColumn(matrix: List[List[String]], n: Int):List[String] = matrix.map(xs => xs.apply(n)).toList
  def mergeColumns(columns: List[List[List[String]]]):List[List[String]] = List.flatten(columns).distinct
  
  def computeColumn(measureId: Long, answers: List[List[List[String]]], attributes: List[List[String]], metadata: Node) = {
   def row(list: List[String], index: Int) = if(index >= 0) list(index) else ""
   val indices = (metadata \\ "select").map(s => first((s \\ "measureId").map(MyUtil.getNodeText(_).toLong).toList, measureId, 0)).toList
   val firstRow= attributes.head
   
   if(firstRow != null) {
	   val pairs = (answers zip indices).filter(_._2 > 0).map(x => (extractColumn(x._1, firstRow.size), extractSelectedColumn(x._1, firstRow.size + x._2 - 1)))
	   val rows = (List.flatten(pairs.map(_._1)), List.flatten(pairs.map(_._2)))
	   attributes.map(attr => row(rows._2, first(rows._1, attr,0))).toList
   }
   else Nil
  }
  
  def measuresInFormula(formula: String): List[Long] = {
   val M = """(m)(\d+)""".r
   val msrs = for (M(m,d) <- M findAllIn formula) yield d.toLong
   msrs.toList
  }
  
  def item(n: Long, list:List[(Long, List[String])]): List[String] = list match {
	   case Nil => Nil
	   case head :: tail => if(head._1 == n) head._2 else item(n, tail)
   }
  
  def arrangeColumns(reference: List[Long], sortedSoFar: List[List[String]], toBeSorted:List[(Long, List[String])]): List[List[String]] = reference match {
   case Nil => sortedSoFar
   case head :: tail => arrangeColumns(tail, (item(head, toBeSorted) :: sortedSoFar.reverse).reverse, toBeSorted)
  }
  
  def toRow(matrix: List[List[String]], m: Int) = matrix.map(column => column(m)).toList
  
  def asString(d: Option[BigDecimal]): String = d match {
   case None => ""
   case Some(x) => x.toString
  }
  
  def asDecimal(d: Option[BigDecimal]): BigDecimal = d match {
   case None => new BigDecimal("0")
   case Some(x) => x
  }
  
  def computeMeasure(measure: Node, columns: List[(Long, List[String])], isPie: Boolean): List[String] = {
   def norm(x: BigDecimal, n: BigDecimal) = x.divide(n.divide(new BigDecimal(2 * PI), 10, BigDecimal.ROUND_HALF_UP), 10, BigDecimal.ROUND_HALF_UP)
   
   val formula = MyUtil.getSeqHeadText(measure \\ "formula")
   if(formula == null || formula == "") {
	   
	   val results = columns.filter(c => c._1 == MyUtil.getSeqHeadText(measure \\ "id").toLong).head._2
	   
	   if(isPie) {
	  	   
	  	   val sumResults = ((new BigDecimal("0")) /: results.map(x => new BigDecimal(x))) (_.add(_))
	  	   if(sumResults.doubleValue() != 0) results.map(r => norm(new BigDecimal(r), sumResults)).map(_.toString)
	  	   else results
	   }
	   else results
   }
   else {
	   val iDs = measuresInFormula(formula)
	   val arrangedColumns = arrangeColumns(iDs, List(), columns)
	   val results = arrangedColumns.head.indices.toList.map(i => WikiParser.evaluateTerm(formula,toRow(arrangedColumns,i).map(digits => new BigDecimal(digits))))
	   
	   if(isPie) {
	  	   val sumResults = ((new BigDecimal("0")) /: results.map(asDecimal)) (_.add(_))
	  	   if(sumResults.doubleValue() != 0) results.map(r => norm(asDecimal(r), sumResults)).map(_.toString) 
	  	   else results.map(asString)
	   }
	   else results.map(asString)
   }
  }
  
  def serialize(attributes: List[List[String]], measures: List[List[String]], asGrid: Boolean, emphasize: String) = {
   
   def serializeCell(attr:String, asGrid: Boolean, starts: Boolean, stops: Boolean, emphasize: String): Node = {
	   if(starts && !asGrid && attr == emphasize) <name em="Y">{attr}</name>
	   else if(starts && !asGrid) <name em="N">{attr}</name>
	   else if (starts) <first>{attr}</first>
	   else if (stops) <last>{attr}</last>
	   else <next>{attr}</next>
   }
   
   def serializeRow(attrs: List[String], asGrid: Boolean, starts: Boolean, emphasize: String): NodeSeq = attrs match {
	   case Nil => NodeSeq.Empty
	   case head :: Nil => serializeCell(head, asGrid,starts, true, emphasize)  				
	   case head :: tail => MyUtil.flattenNodeSeq(serializeCell(head, asGrid,starts, false, emphasize) :: serializeRow(tail, asGrid, false, emphasize).toList)
   }
   
   MyUtil.flattenNodeSeq((attributes.indices zip attributes).map(row => serializeRow(row._2 ::: toRow(measures, row._1), asGrid, true, emphasize)).toList)
   
  }
  
  def findValue(reference: Long, indices: List[Long], values: List[String]): String = indices match {
	  case Nil => ""
	  case head :: tail => if(head == reference) values.head else findValue(reference, tail, values.tail)
  }
  
  def createGridAxis(toBe: List[String], asIs: List[String]) = {
	  if(toBe.isEmpty && asIs.isEmpty) Nil
	  else if(toBe.isEmpty) asIs zip asIs
	  else toBe zip toBe.map(item => if(asIs.contains(item)) item else null)
  }
  
  // main processing of method ask starts here
 
  val blocks = new ListBuffer[Node]
  
  for(blockId <- blockIds) { 
	  
	  // read the metadata and act according to each block type
	  
	  val metadata = blockMap(blockId)
	  val blockType = MyUtil.getSeqHeadText(metadata \\ "blockType")
		 
	  if(blockType == "text") {
		  val select = metadata \\ "select"
		  val system = MyUtil.getSeqHeadText(select \\ "system")
		  val sql = setSQLParameter(MyUtil.getSeqHeadText(select \\ "sql"), filter)
		  val text = getText(query(sql, system))
			 
		  blocks +=  <block type="text">{text}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
	  } 
	  else {
			
			val allAnswers = for(select <- metadata \\ "select";
			                     val system = MyUtil.getSeqHeadText(select \\ "system");
			                     val sql = setSQLParameter(MyUtil.getSeqHeadText(select \\ "sql"), filter)
			                  ) yield query(sql, system)
			
			val measureIds = (metadata \\ "select" \\ "measureId").map(MyUtil.getNodeText(_).toLong).filter(mId => mId >= 0)
			
			if(blockType == "zero") {
				val allNumbers = List.flatten(allAnswers.map(answer => answer._2.head).toList)
				val formula = MyUtil.getSeqHeadText(metadata \\ "formula")
				if(formula == null || formula == "") blocks += <block type="zero">{allNumbers.head}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
				else {
					val indices = (metadata \\ "select" \\ "measureId").map(MyUtil.getNodeText(_).toLong).toList
					val measureIds = measuresInFormula(formula)
					val parameter = measureIds.map(measureId => findValue(measureId, indices, allNumbers))
				
					blocks += <block type="zero">{asString(WikiParser.evaluateTerm(formula, parameter.map(new BigDecimal(_))))}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
				}
			} 
			else {
				
				val attributeCount = (metadata \\ "structure" \\ "attribute").size
				val attributes = mergeColumns(allAnswers.map(x => extractColumn(x._2,attributeCount)).toList)
				val cols = measureIds zip measureIds.map(measureId => computeColumn(measureId, allAnswers.map(_._2).toList, attributes, metadata))
				val measureMetadata = (metadata \\ "structure" \\ "measure")	
				val measureColumns = measureMetadata.map(m => computeMeasure(m, cols.toList, blockType == "pie")).toList
				
				if(blockType == "grid") {
					/*
					val horizontalSpan = MyUtil.getSeqHeadText((metadata \\ "horizontalSpan"))
					val verticalSpan = MyUtil.getSeqHeadText((metadata \\ "verticalSpan"))
					val attrs = metadata \\ "structure" \\ "attribute"
					
					val horizontalCandidates = (attrs.indices zip attrs).map(item => (item._1, MyUtil.getNodeText(item._2) == horizontalSpan)).filter(_._2)
					val verticalCandidates = (attrs.indices zip attrs).map(item => (item._1, MyUtil.getNodeText(item._2) == verticalSpan)).filter(_._2)
					
					if(horizontalCandidates.isEmpty && verticalCandidates.isEmpty) {
						blocks += <block type="one">{serialize(attributes, measureColumns, true, null)}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
					}
					else {
						
						val horizontalAttributes = MyUtil.getSeqHeadText((metadata \\ "horizontalAttributes")).map(x => MyUtil.getNodeText(x)).toList
						val verticalAttributes = MyUtil.getSeqHeadText((metadata \\ "verticalAttributes")).map(x => MyUtil.getNodeText(x)).toList
						
						if(!horizontalCandidates.isEmpty && !verticalCandidates.isEmpty) {
							
							val leadingRow = createGridAxis(horizontalAttributes, attributes.apply(horizontalCandidates(0)._1.toInt)) 
							val leadingColumn = createGridAxis(verticalAttributes, attributes.apply(verticalCandidates(0)._1.toInt))
							
							followingRows =
							followingColumns = 
								
							for(rowPivot <- leadingColumn)	
							
						}
					}
					*/
				}
				else blocks += <block type="one">{serialize(attributes, measureColumns, false, null)}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
			} 
		 } 
	 }
     
  	 blocks.toList
 }
 
 def sendMetaData(fileName: String) = {
  val scorecards = Repository.read("deployment", 0, "individual", fileName, 0) \\ "scorecards"
  if(scorecards.isEmpty) <metadata /> else scorecards.apply(0)
 }
 
 def alreadyIn(aNode: Node, nodes: List[Node]): Boolean = nodes match {
  case Nil => false
  case head :: tail => if(Utility.trim(aNode) == Utility.trim(head)) true else alreadyIn(aNode, tail)
 }
 
 def merge(previous: List[Node], actual: List[Node]): List[Node] = actual match {
  case Nil =>  previous
  case head :: tail => if(alreadyIn(head, previous)) merge(previous, tail) else head :: merge(previous, tail)
 }
 
 def createResponse(answers: List[Node], mode: String): Node = {
  
  if(mode == "initial"){
	  sessionMemory += (BIServiceUser.is -> answers)
	  println("The initial request handling")
	  println(<data>{MyUtil.flattenNodeSeq(answers)}</data>)
	  <data>{MyUtil.flattenNodeSeq(answers)}</data>
  }
  else {
	  val previousNodes = sessionMemory(BIServiceUser.is)
	  val reducedNodes = answers.filter(alreadyIn(_, previousNodes))
	  val mergedNodes = merge(previousNodes, answers) 
	  
	  sessionMemory += (BIServiceUser.is -> mergedNodes)
	  println("The update request handling")
	  println(<data>{MyUtil.flattenNodeSeq(reducedNodes)}</data>)
	  <data>{MyUtil.flattenNodeSeq(reducedNodes)}</data>
  }
 }
 
 serve {
	 
  case "blocks" :: mode :: checksum :: blocks :: Nil XmlGet _ => {
   // GET command starting with 'blocks' is the standard case	  
   if(!isInitialized) initializeBIService()
   	 
   if(isActual(checksum)) {
	val blockIds = blocks.substring(1).split("b").map(_.toLong).toList
	
	if(isPartOf(blocks.substring(1).split("b").toList, userMap(BIServiceUser.is)._3.map(_.toString))) {
		val answers = ask(blockIds, NodeSeq.Empty)
		createResponse(answers, mode)
	}
	else <msg>Access to some blocks forbidden</msg>
   }
   else sendMetaData(userMap(BIServiceUser.is)._2)
  }
  
  case "blocks" :: mode :: checksum :: blocks :: Nil XmlPost xml -> _ => {
   // POST command is used when filter attributes are required
   if(!isInitialized) initializeBIService()
   	 
   if(isActual(checksum)) {
	val blockIds = blocks.substring(1).split("b").map(_.toLong).toList
	
	if(isPartOf(blocks.substring(1).split("b").toList, userMap(BIServiceUser.is)._3.map(_.toString))) {
		val answers = ask(blockIds, xml \\ "filter")
		createResponse(answers, mode)
	}
	else <msg>Access to some blocks forbidden</msg>
   }
   else sendMetaData(userMap(BIServiceUser.is)._2)
   
  }
  
  case "bi" :: "ping" :: Nil Get _ => <html><body><b>OK</b></body></html>
 }
}