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
 var storeMap = Map.empty[String, (CI, String, String, String)];
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
	  blockMap += ((aBlock \\ "@blockId").text.toLong -> aBlock)
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
 
 def query(sql: String, system: String):(List[String],List[List[String]]) = {
  val purgedSql = """&gt;""".r replaceAllIn("""&lt;""".r replaceAllIn(sql,"<"), ">")
  DB.runQuery(purgedSql, Nil, storeMap(system)._1)
 }
 
 // check whether the given checksum matches the user's checksum
 
 def isActual(checksum: String): Boolean = userMap(BIServiceUser.is)._1 == checksum
 
 // set the parameter of the SQL statement
 
 def setSQLParameter(sql: String, filter: NodeSeq, system: String): String = {
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
  
  def replaceTimeParameter(select: String, toDatePattern: String): String = {
   val timePattern = """\?[\w\_]+\?""".r 
   val firstMatch = timePattern findFirstIn select
   
   firstMatch match {
		case None => select
	 	case Some(m) => replaceTimeParameter(timePattern replaceFirstIn(select, MyUtil.timeInSql(m, toDatePattern)) , toDatePattern)  	   
   }
  }
  
  val toDateSql = storeMap(system)._2
  
  if(filter == null || filter.isEmpty) replaceTimeParameter(sql, toDateSql) 
  else {
	val filterList = filter.map(f => MyUtil.getSeqHeadText(f)).toList
	replaceQuestionMarks(replaceTimeParameter(sql, toDateSql), filterList)
  }
 }
 
 // main processing method
 
 def ask(blockIds: List[Long], filter: NodeSeq): List[Node] = {
	 
  // helper method to extract string from query result, when you expect a single value as query result	 
  
  def getText(queryResult: (List[String], List[List[String]])) = if(!queryResult._2.isEmpty && !queryResult._2.head.isEmpty) queryResult._2.head.head else ""
  
  // get index of first occurence
	  
  def first(l: List[Any],x:Any, index: Int): Int = l match {
   case Nil => -1
   case h :: tail => if(h == x) index else first(tail, x, index + 1)
  }
	  
  def extractColumn(matrix: List[List[String]], n: Int):List[List[String]] = matrix.map(xs => xs take n).toList
  def extractSelectedColumn(matrix: List[List[String]], n: Int):List[String] = matrix.map(xs => xs.apply(n)).toList
  def mergeColumns(columns: List[List[List[String]]]):List[List[String]] = List.flatten(columns).distinct
  
  def computeColumn(measureId: Long, answers: List[List[List[String]]], attributes: List[List[String]], metadata: Node) = {
   def row(list: List[String], index: Int) = if(index >= 0 && index < list.size) list(index) else ""
   val indices = (metadata \\ "select").map(s => first((s \\ "measureId").map(MyUtil.getNodeText(_).toLong).toList, measureId, 0)).toList
   
   if(attributes.size > 0 && attributes.head != null) {
	   val firstRow = attributes.head
	   val pairs = (answers zip indices).filter(_._2 > 0).map(x => (extractColumn(x._1, firstRow.size), extractSelectedColumn(x._1, x._2)))
	   val rows = (List.flatten(pairs.map(_._1)), List.flatten(pairs.map(_._2)))
	   attributes.map(attr => row(rows._2, first(rows._1, attr,0))).toList
   }
   else Nil
  }
  
  def transpose(rows: List[List[String]]): List[List[String]] = rows match {
	  case Nil => Nil
	  case firstRow :: moreRows => {
	 	  if(firstRow.size == 0) rows
	 	  else firstRow.indices.map(index => extractSelectedColumn(rows, index)).toList
	  }
  }
  
  // returns a list of all measure ids in a formula
  
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
  
  def toRow(matrix: List[List[String]], m: Int) = if(m == -1 && matrix.size > 0) List.range(0, matrix.size).map(x => "")  else matrix.map(column => column(m)).toList
  
  def asString(d: Option[BigDecimal]): String = d match {
   case None => ""
   case Some(x) => x.toString   
  }
  
  def asDecimal(d: Option[BigDecimal]): BigDecimal = d match {
   case None => new BigDecimal("0")
   case Some(x) => x
  }
  
  def scl(d: String, n: Int) = {
	  try{
	 	  (new BigDecimal(d)).setScale(n, java.math.RoundingMode.HALF_UP).toString
	  }
	  catch{
	 	  case e: Exception => d
	  }
  }
  
  def computeMeasure(measure: Node, columns: List[(Long, List[String])], isPie: Boolean): List[String] = {
   def norm(x: BigDecimal, n: BigDecimal) = x.divide(n.divide(new BigDecimal(2 * PI), 10, BigDecimal.ROUND_HALF_UP), 10, BigDecimal.ROUND_HALF_UP)
   
   val formula = """&gt;""".r replaceAllIn("""&lt;""".r replaceAllIn(MyUtil.getSeqHeadText(measure \\ "formula"), "<"), ">")
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
  
  def measureRow(measureColumns: List[List[String]], attributes: List[List[String]], vertical: String, horizontals: List[String], vPosition: Int, hPosition: Int): List[String] = {
   def pos(v: String, h: String, vPos: Int, hPos: Int, matrix: List[List[String]], index: Int): Int = matrix match {
	   case Nil => -1
	   case head :: tail => if(head(hPos) == h && head(vPos) == v) index else pos(v, h, vPos, hPos, tail, index + 1) 
   }
   
   List.flatten(horizontals.map(h => toRow(measureColumns, pos(vertical, h, vPosition, hPosition, attributes, 0))))	  
  }
  
  def hRow(measureColumns: List[List[String]], attributes: List[List[String]], axis: List[String], position: Int): List[String] = {
   def pos(x: String, p: Int, matrix: List[List[String]], index: Int): Int = matrix match {
	   case Nil => index
	   case head :: tail => if(head(p) == x) index else pos(x, p, matrix, index + 1) 
   }
   
   List.flatten(axis.map(h => toRow(measureColumns, pos(h, position, attributes, 0))))
  }
  
  def vRow(measureColumns: List[List[String]], attributes: List[List[String]], item: String, position: Int): List[String] = {
   def pos(item: String, p: Int, matrix: List[List[String]], index: Int): Int = matrix match {
	   case Nil => index
	   case head :: tail => if(head(p) == item) index else pos(item, p, matrix, index + 1) 
   }
   
   toRow(measureColumns, pos(item, position, attributes, 0)).toList
  }
  
  def serializeGrid(matrix: List[List[String]]) = {
   def serializeGridCell(attr:String, starts: Boolean, stops: Boolean): Node = {
	   if (starts) <first>{attr}</first>
	   else if (stops) <last>{attr}</last>
	   else <next>{attr}</next>
   }
   
   def serializeGridRow(row: List[String], starts: Boolean): NodeSeq = row match {
	   case Nil => NodeSeq.Empty
	   case head :: Nil => serializeGridCell(head, starts, true)  				
	   case head :: tail => MyUtil.flattenNodeSeq(serializeGridCell(head,starts, false) :: serializeGridRow(tail, false).toList)
   }
   
   MyUtil.flattenNodeSeq(matrix.map(row => serializeGridRow(row, true)).toList)
  }
  
  def serialize(attributes: List[List[String]], measures: List[List[String]], emphasize: String) = {
   
   def serializeCell(attr:String, starts: Boolean, stops: Boolean, emphasize: String): Node = {
	   if(starts && attr == emphasize) <name em="Y">{attr}</name>
	   else if(starts) <name em="N">{attr}</name>
	   else if (stops) <last>{attr}</last>
	   else <next>{attr}</next>
   }
   
   def serializeRow(attrs: List[String], starts: Boolean, emphasize: String): NodeSeq = attrs match {
	   case Nil => NodeSeq.Empty
	   case head :: Nil => serializeCell(head, starts, true, emphasize)  				
	   case head :: tail => MyUtil.flattenNodeSeq(serializeCell(head,starts, false, emphasize) :: serializeRow(tail, false, emphasize).toList)
   }
   
   MyUtil.flattenNodeSeq((attributes.indices zip attributes).map(row => serializeRow(row._2 ::: toRow(measures, row._1), true, emphasize)).toList)
   
  }
  
  def findValue(reference: Long, indices: List[Long], values: List[String]): String = indices match {
	  case Nil => ""
	  case head :: tail => if(head == reference) values.head else findValue(reference, tail, values.tail)
  }
  
  def rearrangeRow(row: List[String], reference: List[Long], part: List[Long]): List[String] = part match {
	  case Nil => Nil
	  case head :: tail => {
	 	  val index = first(reference, head, 0)
	 	  if(index == -1) Nil else row(index) :: rearrangeRow(row, reference, tail) 
	  }
  }
  
  // main processing of method ask starts here
 
  val blocks = new ListBuffer[Node]
  
  for(blockId <- blockIds) { 
	  
	  // read the metadata and act according to each block type
	  
	  val metadata = blockMap(blockId)
	  val blockType = MyUtil.getSeqHeadText(metadata \\ "blockType")
		 
	  if(blockType == "text" && (MyUtil.getSeqHeadText(metadata \\ "formula") == null || MyUtil.getSeqHeadText(metadata \\ "formula") == "")) {
		  val select = metadata \\ "select"
		  val system = MyUtil.getSeqHeadText(select \\ "system")
		  val sql = setSQLParameter(MyUtil.getSeqHeadText(select \\ "sql"), filter, system)
		  val text = getText(query(sql, system))
			 
		  blocks +=  <block type="text">{text}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
	  } 
	  else {
		  val allAnswers = for(select <- metadata \\ "select";
			                     val system = MyUtil.getSeqHeadText(select \\ "system");
			                     val sql = setSQLParameter(MyUtil.getSeqHeadText(select \\ "sql"), filter, system)
			                  ) yield query(sql, system)
			
		  val measureIds = (metadata \\ "select" \\ "measureId").map(MyUtil.getNodeText(_).toLong).filter(mId => mId >= 0)
			
		  if(blockType == "zero" || blockType == "text") {
			  val allNumbers = List.flatten(allAnswers.map(answer => answer._2.head).toList)
			  val formula = """&gt;""".r replaceAllIn("""&lt;""".r replaceAllIn(MyUtil.getSeqHeadText(metadata \\ "formula"), "<"), ">")
			  
			  if(formula == null || formula == "") blocks += <block>{allNumbers.head}</block> % new UnprefixedAttribute("id", blockId.toString, Null) % new UnprefixedAttribute("type", blockType, Null)
			  else {
				  val indices = (metadata \\ "select" \\ "measureId").map(MyUtil.getNodeText(_).toLong).toList
				  val measureIds = measuresInFormula(formula)
				  val parameter = measureIds.map(measureId => findValue(measureId, indices, allNumbers))
				  
				  if(blockType == "zero") blocks += <block type="zero">{WikiParser.evaluateIndicator(formula, parameter.map(new BigDecimal(_)))}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
				  else blocks += <block type="text">{WikiParser.evaluateIndicator(formula, parameter.map(new BigDecimal(_))).toString}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
			 }
		  } 
		  else {
				
			  val attributeCount = (metadata \\ "structure" \\ "attribute").size
			  val attributes = mergeColumns(allAnswers.map(x => extractColumn(x._2,attributeCount)).toList)
			  val cols = measureIds zip measureIds.map(measureId => computeColumn(measureId, allAnswers.map(_._2).toList, attributes, metadata))
			  val measureMetadata = (metadata \\ "structure" \\ "measure")	
			  val emphasize = MyUtil.getSeqHeadText((metadata \\ "structure" \\ "attribute" \\ "emphasize"))
			
			  val isPie = MyUtil.getSeqHeadText(metadata \\ "structure" \\ "pie") == "pie"
			  val measureColumns = measureMetadata.map(m => computeMeasure(m, cols.toList, isPie)).toList
			  
			  val scale = MyUtil.getSeqHeadText(metadata \\ "structure" \\ "scale").toInt
			 
			  if(blockType == "grid") {
					val horizontalSpan = MyUtil.getSeqHeadText((metadata \\ "structure" \\ "grid" \\ "horizontalSpan")).split(";").toList.map(_.toLong)
					val verticalSpan = MyUtil.getSeqHeadText((metadata \\ "structure" \\ "grid" \\ "verticalSpan")).split(";").toList.map(_.toLong)
					val attrIds = (metadata \\ "structure" \\ "attribute" \\ "id").map(MyUtil.getNodeText(_).toLong).toList
					
					val vertical = attributes.map(row => rearrangeRow(row, attrIds, verticalSpan)).distinct
					val horizontal = transpose(attributes.map(row => rearrangeRow(row, attrIds, horizontalSpan)).distinct)
					
					val headerRows = if(vertical.isEmpty) horizontal else horizontal.map(h => List.range(1, vertical(0).size + 1).map(index => "") ::: h)
					
					val bodyRows = if(vertical.isEmpty && horizontal.isEmpty) List(List(""))
						           else if (vertical.isEmpty) List(hRow(measureColumns, attributes, horizontal(0), first(attrIds, horizontalSpan(0), 0)))
						           else if(horizontal.isEmpty) vertical.map(v => v ::: vRow(measureColumns, attributes, v(0), first(attrIds, verticalSpan(0), 0))).toList
						           else vertical.map(v => v ::: measureRow(measureColumns, attributes, v(0), horizontal(0), first(attrIds, verticalSpan(0), 0), first(attrIds, horizontalSpan(0), 0)).map(d => scl(d, scale))).toList
				    
					blocks += <block type="grid">{serializeGrid(headerRows ::: bodyRows)}</block> % new UnprefixedAttribute("id", blockId.toString, Null) 
			 }
			 else blocks += <block type="one">{serialize(attributes, measureColumns, emphasize)}</block> % new UnprefixedAttribute("id", blockId.toString, Null)
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
   
   try {
	   if(userMap.contains(BIServiceUser.is) && isActual(checksum)) {
		val blockIds = blocks.substring(1).split("b").map(_.toLong).toList
		
		if(isPartOf(blocks.substring(1).split("b").toList, userMap(BIServiceUser.is)._3.map(_.toString))) {
			val answers = ask(blockIds, NodeSeq.Empty)
			createResponse(answers, mode)
		}
		else <msg>Access to some blocks forbidden</msg>
	   }
	   else if (userMap.contains(BIServiceUser.is)) {
	  	   sendMetaData(userMap(BIServiceUser.is)._2)
	   }
	   else <msg>Access forbidden</msg>
   }
   catch {
	   case e: Exception => println(e.toString)
	   <msg>Access to some blocks forbidden</msg>
   }
  }
  
  case "blocks" :: mode :: checksum :: blocks :: Nil XmlPost xml -> _ => {
   // POST command is used when filter attributes are required
   if(!isInitialized) initializeBIService()
   	 
   try {
	   if(userMap.contains(BIServiceUser.is) && isActual(checksum)) {
		val blockIds = blocks.substring(1).split("b").map(_.toLong).toList
		
		if(isPartOf(blocks.substring(1).split("b").toList, userMap(BIServiceUser.is)._3.map(_.toString))) {
			val answers = ask(blockIds, xml \\ "filter")
			createResponse(answers, mode)
		}
		else <msg>Access to some blocks forbidden</msg>
	   }
	   else if (userMap.contains(BIServiceUser.is)) sendMetaData(userMap(BIServiceUser.is)._2)
	   else <msg>Access forbidden</msg>
   }
   catch {
	   case e: Exception => println(e.toString)
	   <msg>Access to some blocks forbidden</msg>
   }
   
  }
  
  case "bi" :: "ping" :: Nil Get _ => <html><body><b>OK</b></body></html>
  
 }
}