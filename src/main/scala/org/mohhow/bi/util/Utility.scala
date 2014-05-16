package org.mohhow.bi.util

import net.liftweb._
import http._
import SHtml._
import S._

import mapper._
import util._
import java.text.SimpleDateFormat
import java.util.Date
import java.text.ParsePosition
import scala.xml._
import org.mohhow.model._
import org.mohhow.snippet._
import org.mohhow.bi.lib.Repository

object Utility {
	
 /*
  *  XML helper methods
  */
	
	
 /**
   * utility function used to convert a list of NodeSeq objects into a single NodeSeq object
   */
	
 def flattenNodeSeq(l: List[NodeSeq]) : NodeSeq = List.flatten(l.map(_.toList)).toSeq
 
 def toNode(seq: NodeSeq): Option[Node] = if(seq.length == 1) Some(seq(0)) else None
 
 def getNodeText(node: Node): String = node match {
  case Elem(_, _, _, _, Text(myText)) => myText
  case _ => ""
 }
 
 def getSeqHeadText(seq: NodeSeq) = if(seq.isEmpty) "" else getNodeText(seq(0))
 
 def makeUnique(text: String, list: List[String], n: Int): String = {
	if (!list.contains(text)) text 
	else {
	  val newText = text + "_" + n.toString
	  if (!list.contains(newText)) newText else makeUnique(text, list, n + 1)
	 }
 }
 
 def tagIt(text: String): NodeSeq = {
   val purgedText = """&gt;""".r replaceAllIn("""&lt;""".r replaceAllIn(text, "<"), ">")
   
   try {
	   XML.loadString("<span>" + purgedText + "</span>") \\ "span"
   }
   catch{
   		case e: Exception => {
	  	   println("Exception during parsing " + text)
	  	   <span>{text}</span>
   		}
   }
 }
 
 def repeat(text: String, n: Int): String = if(n <= 0) "" else text + repeat(text, n-1)
 
 def nvl(s: String) = if (s == null) "" else s
	 
/*
 * Methods for date formatting
 */
	
 def asDate(dayOfYear: String, timeInDay: String) : Date = {
  val day = new SimpleDateFormat("dd.MM.yyyy HH:mm")
  val pattern = dayOfYear + " " + timeInDay
  val pos = new ParsePosition (0)
  day.parse(pattern, pos)
 }
 
 def asDate(dayOfYear: String) : Date = {
  val day = new SimpleDateFormat("dd.MM.yyyy")
  val pos = new ParsePosition (0)
  day.parse(dayOfYear, pos)
 }
 
 def timeInDay(d : Date) : String = {
  if(d == null) "" else {
	val timeInDay = new SimpleDateFormat("HH:mm")
    timeInDay.format(d)
  }
 }
 
 def formatDate(d: Date) : String = {
  if(d == null) "" else {
   val dayInYear = new SimpleDateFormat("dd.MM.yyyy")
   dayInYear.format(d) 
  }
 }
 
 def dateFromNumber(n: Long) : Date = {
  if(n == 0) null 
  else {
	  val fromNumber = new SimpleDateFormat("yyyyMMdd")
	  val pos = new ParsePosition (0)
	  fromNumber.parse(n.toString, pos)
  }
 }
 
 def formatDate(d: Date, format: String) : String = {
  if(d == null) "" else {
   val sdf = new SimpleDateFormat(format)
   sdf.format(d) 
  }
 }
 
 def dateAsNumber(d: Date): Long = {
  if(d == null) 19000101
  else {
	  val dateNumberFormat = new SimpleDateFormat("yyyyMMdd")
	  dateNumberFormat.format(d).toLong
  }
 }
 
 def succDate(d: Long): Long = {
  val day = d % 100
  val m = (d/100) % 100
  val y = d/10000
  
  if (day < 28 || (day == 28 && m == 2 && y % 4 == 0) ) d + 1
  else if (day <= 29 && m == 2) y * 10000 + 301
  else if (day <= 29) d + 1
  else if (day == 30 && (m == 4 || m == 6 || m == 9 || m == 11)) y*10000 + (m+1) * 100 + 1
  else if (day ==30) d + 1
  else if (day == 31 && m == 12) (y + 1) * 10000 + 101
  else y * 10000 + (m+1)* 100 + 1
 }
 
 def duration(begin:Long, end:Long):Long = {
  def intervall(b: Long, e: Long, i: Long):Long = if(b >= e) i else intervall(succDate(b),e,i+1)
  
  if(begin > end) 0 else intervall(begin, end, 1)
 }
 
 def allDays(start: Long, end: Long, soFar: List[Long]): List[Long] = if(start < end) allDays(succDate(start), end, start :: soFar) else soFar.reverse
 
 def month(dateAsNumber: Int) = dateAsNumber/100 % 100 
 def year(dateAsNumber: Int) = dateAsNumber/10000

 def quarter(dateAsNumber: Int) = { 
  val m = month(dateAsNumber)  
  if(m % 3 == 0) m/3 else m/3 + 1 
 }

 def dayInMonth(dateAsNumber: Int) = dateAsNumber % 100


 def dayInWeek(dateAsNumber: Int) = { 
  val d = dayInMonth(dateAsNumber) 
  val mo = month(dateAsNumber)
  val m = if (mo < 3) 10 + mo else mo - 2 
  val theYear = year(dateAsNumber) 
  val y = if(mo < 3) (theYear - 1) % 100 else theYear % 100 
  val c = if(theYear % 400 == 0) (theYear - 100)/100 else theYear / 100 
  
  val result = (d + Math.floor(2.6 * m - 0.2) + y + Math.floor(y.toLong/4) + Math.floor(c.toLong/4) - 2 * c) % 7 
  if(result == 0) 7 else result
}

def dayInYear(dateAsNumber: Int) = { 
 val y = year(dateAsNumber) 
 val m = month(dateAsNumber) 
 val d = dayInMonth(dateAsNumber) 
 val feb = if((y % 4) == 0 && (y % 400) != 0) 29 else 20

 m match { 
        case 1 => d 
        case 2 => d + 31 
        case 3 => d + 31 + feb 
        case 4 => d + 31 + feb + 31 
        case 5 => d + 31 + feb + 31 + 30 
        case 6 => d + 31 + feb + 31 + 30 + 31 
        case 7 => d + 31 + feb + 31 + 30 + 31 + 30 
        case 8 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 
        case 9 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 
        case 10 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 
        case 11 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 
        case 12 => d + 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30

 }      
}

def weekInYear(dateAsNumber: Int) = {
 val d = dayInYear(dateAsNumber) 
 val y = year(dateAsNumber) 
 val first = dayInWeek(y * 10000 + 101).toInt 
 if(first > 4) (d - first + 2) / 7 + 1 else  (d + first - 2) /  7 + 1
} 

def timeInSql(timePattern: String, toDatePattern:  String): String = {
	
	val today = new Date
	
	timePattern match {
		case "?today?" =>  toDatePattern.replace("?1", formatDate(today, S.?("dateFormat"))).replaceAll("?2", S.?("dateFormat")) 
		case "?tomorrow?" =>  toDatePattern.replaceAll("?1", formatDate(dateFromNumber(succDate(dateAsNumber(today))), S.?("dateFormat"))).replaceAll("?2", S.?("dateFormat"))
		case "?actual_year?" =>  year(dateAsNumber(today).toInt).toString
		case "?previous_year?" =>  (year(dateAsNumber(today).toInt) - 1).toString
		case "?actual_quarter?" =>  quarter(dateAsNumber(today).toInt).toString
		case "?actual_month?" =>  month(dateAsNumber(today).toInt).toString
	}
	
	//"yesterday"|"actual_week"|"previous_week"|"previous_month"|"previous_quarter"
}
 
 /**
  * generic methods for the user management
  * 
  */
 
 def getUser(userId: Long) = {
  val usrs = User.findAll(By(User.id, userId))
  if(usrs.isEmpty) null else usrs(0)
 }
 
 def getUserName(userId: Long) = {
  val usrs = User.findAll(By(User.id, userId))
  if(usrs.isEmpty) "" else usrs(0).firstName + " " + usrs(0).lastName 
 }
 
 def findRoles (scenarioId: Long, roleName: String): List[User] = {
  def getUser(sr: ScenarioRole) = {
	  val usrs = User.findAll(By(User.id, sr.fkUser))
	  if(usrs.isEmpty) null else usrs(0)
  }
  ScenarioRole.findAll(By(ScenarioRole.fkScenario, scenarioId), By(ScenarioRole.role, roleName)).map(sr => getUser(sr)).filter(_ != null).toList
 }
 
 def filterScenarioRoles(soFar: List[ScenarioRole], remaining: List[ScenarioRole]): List[ScenarioRole] = remaining match {
  case Nil => soFar
  case head :: tail => if(soFar.map(_.fkUser).contains(head.fkUser)) filterScenarioRoles(soFar, tail) else filterScenarioRoles(head :: soFar, tail)
 }
 
 /**
  * Helper methods for dimensional modelling
  * 
  */
 
 def isConnected(v1: Long, v2: Long) = !ModelEdge.findAll(By(ModelEdge.head, v1), By(ModelEdge.tail, v2)).isEmpty || !ModelEdge.findAll(By(ModelEdge.head, v2), By(ModelEdge.tail, v1)).isEmpty
  
 def makeSemicolonList(items: List[String]): String = items match {
	case Nil => ""
	case head :: tail => (head /:  tail) (_ + ";" + _)
 }
 
 def makeSeparatedList(items: List[String], separator: String): String = items match {
	case Nil => ""
	case head :: tail => (head /:  tail) (_ + separator + _)
 }
 
 def csv(matrix: List[List[String]]): String =  (("" /: matrix.map(row => makeSeparatedList(row, ";"))) (_ + "\n" + _)).trim
 
 val tableTypes = List("dimension", "dateDimension", "measureDimension", "level", "fact", "snapshotFact", "transactionFact", "accountFact", "accountSnapshotFact", "accountTransactionFact")
	
 /*
  * LDAP Utilities
  */
 
 def createConfiguration(p: Provider): Map[String, String] = {
	 Map("ldap.url" -> p.url.toString, 
		 "ldap.base" -> p.base.toString, 
		 "ldap.userName" -> p.userName.toString, 
		 "ldap.password" -> p.pwd.toString, 
		 "ldap.authType" -> p.authType.toString,
		 "ldap.initial_context_factory" -> p.initialContextFactory.toString,
		 "ldap.testLookup" -> p.testLookup.toString,
		 "ldap.retryInterval" -> p.retryIntervall.toString,
	 	 "ldap.maxRetries" -> p.maxRetries.toString)
 }
 
 def attr2List(attrs: javax.naming.directory.Attributes, memberAttribute: String, displayAttribute: String): List[String] = {
   println(attrs.toString)
   val iterator = attrs.getIDs()
   getDisplayList(attrs.get(memberAttribute).toString, displayAttribute)
  }
  
  def rOfEqual(eqn: String) = {
	 val twoTerms= eqn.split("=").toList
	 if(twoTerms.size == 2) twoTerms(1) else eqn
  }
 
  def getDisplayList(attr: String, displayAttribute: String): List[String] = {
 
   val pair = attr.split(":").toList
   if(pair.size == 2) pair(1).split(",").toList.map(_.trim).filter(_.startsWith(displayAttribute)).map(rOfEqual).toList  
   else Nil
  }
  
  def getDisplayItem(attr: String, displayAttribute: String) = {
   val l = attr.split(",").toList.map(_.trim).filter(_.startsWith(displayAttribute)).map(rOfEqual)
   if(l.isEmpty) attr else l(0)
  }
  
  // end LDAP utilities
  
  def prettyTerm(text: String, isFilter: Boolean): String = {
   if(text== null || text.length == 0) text
   else {
	    
	  val pattern = """<(d|m)\d+>""".r
	  val firstMatch = pattern findFirstIn text
	  
	  firstMatch match {
	 	case Some(content) => {
	 	
		 	if(content.substring(1,2) == "m") {
		 		val bracket = if(isFilter) "*" else "\\$"
		 		val msrs = Measure.findAll(By(Measure.id, content.substring(2,content.length - 1).toLong))
		 		
		 		if(!msrs.isEmpty) {
		 			val prettyMeasure = bracket + msrs.apply(0).shortName + bracket
		 			println(text)
		 			println(prettyMeasure)
		 			val replaceMsr = (pattern replaceFirstIn(text, prettyMeasure.toString)).toString
		 			prettyTerm(replaceMsr, isFilter)
		 		}
		 		else text 
		 	}
		 	else {
		 		val openBracket = if(isFilter) "*" else "["
		 		val closingBracket = if(isFilter) "*" else "]"
		 		val elms = ModelVertex.findAll(By(ModelVertex.id, content.substring(2,content.length - 1).toLong))
		 		if(!elms.isEmpty) prettyTerm(pattern replaceFirstIn(text, openBracket + elms.apply(0).elementName  + closingBracket), isFilter)
		 		else text
		 	}
	 	}
	 	case _ => text
	  }
   }
  }
  
  /**
   * database configuration
   */
  
  def dbInfo(alias: String, dbs: NodeSeq, kind: String) = {
   val db = dbs.filter(n => getSeqHeadText(n \ "alias") == alias)
   if(db.isEmpty) "" else getSeqHeadText(db(0) \ kind)
  }
  
  def isOwner() = User.loggedIn_? && (User.currentUser == ScenarioOwner.is)
  def isAnalyst() = isOwner() || (User.loggedIn_? && Analysts.is.contains(User.currentUser openOr null))
  def isDesigner() = isOwner() || (User.loggedIn_? && Designer.is.contains(User.currentUser openOr null))
  def isReleaseManager() = isOwner() || (User.loggedIn_? && ReleaseManager.is.contains(User.currentUser openOr null))
  
 def createFrame(scorecardId: Long) = {
  val portrait = <fr orientation="portrait"></fr> % new UnprefixedAttribute("scorecardId", scorecardId.toString, Null)
  val landscape = <fr orientation="landscape"></fr> % new UnprefixedAttribute("scorecardId", scorecardId.toString, Null)
  val otherFrames = (Repository.read("scenario", SelectedScenario.is.id, "frames", "frames", -1) \\ "fr").toList
  Repository.write("scenario", SelectedScenario.is.id, "frames", "frames", -1, <frames>{flattenNodeSeq(portrait :: landscape :: otherFrames)}</frames>)
 }
 def scale(desc: String): Int = desc match {
	 case "fiveAfter" => 5
	 case "fourAfter" => 4
	 case "threeAfter" => 3
	 case "twoAfter" => 2
	 case "oneAfter" => 1
	 case "noScale" => 0
	 case "powerTen" => -1
	 case "powerHundred" => -2 
	 case "powerThousand" => -3
	 case "powerMillion" => -6
	 case "powerBillion" => -9
	 case _ => 0
 }
 
 // helper methods for table creation
 
 def e(node: Node, name: String) = getSeqHeadText(Setup.is \ "design" \ name)
 
 def nameFromPattern(attr: String, kind: String): String =  kind match {
  	case "pk" => e(Setup.is, "namePrimaryKey").replace("#tableName#", attr)
	case "fk" => e(Setup.is, "nameReferences").replace("#reference#", attr)
	case _ => ""
 }

 def setAttributePhysicalType(attributeType: String, attr: PAttribute): PAttribute = {
  
  if(attributeType != null && attributeType.length > 0) {
	  attr.dataType(getDescriptionPart(attributeType, "datatype"))
	  val length = getDescriptionPart(attributeType, "precision")
	  if(length != "none") attr.length(length.toLong)
	  val scale = getDescriptionPart(attributeType, "scale")   
	  if(scale != "none") attr.scale(scale.toLong)
  }
  
  attr
 }
 
 def getDescriptionPart(text: String, partOfInterest: String) = {
	 
	 val i = text.indexOf("(")
	 
	 partOfInterest match {
		 case "datatype" => if(i > 0) text.substring(0,i) else text
		 case _ => {
			 if(i > 0) {
				 val rightPart = text.substring(i, text.length)
				 val j = rightPart.indexOf(",")
				 if(j > 0 && partOfInterest == "precision") rightPart.substring(1,j) 
				 else if (partOfInterest == "precision") rightPart.substring(1, rightPart.length -1)
				 else if(j > 0) rightPart.substring(j + 1, rightPart.length -1)
				 else "none"
		 	}
		 	else "none" 
		 }	  
	 }
 }
 
 def addAdditionalAttributes(kind: String, table: PTable) = {
  	 
  def createAdditionalAttribute(name: String, attributeType: String) = {
   val newAttribute = PAttribute.create
   newAttribute.fkPTable(table).name(name).validFrom(new Date).isCurrent(1).isDerivedFromModel(0)
   newAttribute.fkModelAttribute(-1).isPrimaryKey(0)
   
   if(attributeType != null && attributeType.length > 0) {
	   newAttribute.dataType(getDescriptionPart(attributeType, "datatype"))
	   val length = getDescriptionPart(attributeType, "precision")
	   if(length != "none") newAttribute.length(length.toLong)
	   val scale = getDescriptionPart(attributeType, "scale")   
	   if(scale != "none") newAttribute.scale(scale.toLong)
   }
   
   newAttribute.save
  }
  
  val n = Setup.is
	 
  val additionalList = List(List(e(n, "identifierETLTableType" ), e(n, "identifierETLName" ), e(n, "identifierETLType")),
		 				   List(e(n, "timestampETLTableType" ), e(n, "timestampETLName" ), e(n, "timestampETLType")),
		 				   List(e(n, "validFromTableType" ), e(n, "validFromName" ), e(n, "validFromType")),
		 				   List(e(n, "validUntilTableType" ), e(n, "validUntilName" ), e(n, "validUntilType")),
		 				   List(e(n, "isActualTableType" ), e(n, "isActualName" ), e(n, "isActualType")),
		                   List(e(n, "consecutiveNumberTableType" ), e(n, "consecutiveNumberName" ), e(n, "consecutiveNumberType")),
		                   List(e(n, "asIsTableType" ), e(n, "asIsNumberName" ), e(n, "asIsType")),
		                   List(e(n, "toBeTableType" ), e(n, "toBeName" ), e(n, "toBeType")),
		                   List(e(n, "forecastTableType" ), e(n, "forecastName" ), e(n, "forecastType")),
		                   List(e(n, "planTableType" ), e(n, "planName" ), e(n, "planType")))
		                   
  additionalList.filter(t => t(0) == "allTable" || t(0) == kind).map(item => createAdditionalAttribute(item(1), item(2))) 	 
 }
 
 def analyse(item: String, n:Int, inp: List[String], result: List[(String, Int)]): List[(String, Int)] = inp match {
	 case Nil => result
	 case head :: tail => {
		 if(head == item) analyse(item, n + 1, tail, result)
		 else analyse(head, 1, tail, (item, n) :: result)
	 }
 }
 
 def makeCloudList(text: String, filterLanguage: String, maxWords:String): NodeSeq = {
	 
  def isTrue(bools: List[Boolean]): Boolean = bools match {
	  case Nil => true
	  case h:: tail => if(!h) false else isTrue(tail)
  }
  
  def isWord(str: String): Boolean = isTrue(str.toList.map(letter => (letter >= 'a' && letter <= 'z') || (letter >= 'A' && letter <= 'Z')).toList)	
  
  val commonWords = S.?("commonWords", filterLanguage).split(" ")
  val allWords = text.split("[\\s\\.,;!]").filter(isWord).filter(word => !commonWords.exists(_ == word.toLowerCase)).toList.sort(_ < _)
  
  if(allWords.isEmpty) NodeSeq.Empty
  else {
	  val occurences = analyse(allWords.head, 1, allWords.tail,Nil).sort(_._2 > _._2).take(maxWords.toInt)
	   flattenNodeSeq(List.flatten(occurences.map(entry => List(<name>{entry._1}</name>, <last>{entry._2.toString}</last>)))) 
  }
 }
}