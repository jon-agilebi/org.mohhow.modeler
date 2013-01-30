package org.mohhow.bi.lib

import org.mohhow._
import model._
import net.liftweb._
import net.liftweb.common._
import mapper._
import util._
import Helpers._
import java.lang.Math._
import org.mohhow.bi.util.{Utility => MyUtil}
import scala.xml._

import http._
import SHtml._
import S._

object ModelUtility {
	
 def isConnected(v1: Long, v2: Long) = !ModelEdge.findAll(By(ModelEdge.head, v1), By(ModelEdge.tail, v2)).isEmpty || !ModelEdge.findAll(By(ModelEdge.head, v2), By(ModelEdge.tail, v1)).isEmpty
 def isConnectedToSomeVertex(v: ModelVertex, l: List[ModelVertex]): Boolean = (false /: l.map(someV => isConnected(v.id, someV.id))) (_ || _) 
 
 def findChain(onChain: List[ModelVertex], remainder:List[ModelVertex]): List[ModelVertex] = remainder match {
	 case Nil => onChain
	 case _ => {
		 val neighbours = remainder.filter(v => isConnectedToSomeVertex(v, onChain))
		 if(neighbours.isEmpty) onChain 
		 else findChain(neighbours.head :: onChain, remainder.filter(_ != neighbours.head))
	 }
 }
 
 def hierarchies(referenceId: Long) = {
  def conn(l: ModelVertex, r:ModelVertex, edges: List[ModelEdge]) = !edges.filter(e => (e.head == l.id && e.tail == r.id) || (e.head == r.id && e.tail == l.id)).isEmpty 
  def getNeighbour(item: ModelVertex, candidates: List[ModelVertex], edges: List[ModelEdge]): Option[ModelVertex] = candidates match {
	  case Nil => None
	  case h :: tail => if(conn(item, h, edges) && item.y < h.y) Some(h) else getNeighbour(item, tail, edges)
  }
  
  def enricheChain(chain: List[(ModelVertex, Int)], set: Set[ModelVertex], edges: List[ModelEdge]): (List[(ModelVertex, Int)], Option[ModelVertex])  = {
	  val maximalItem = getMaximalItem(chain)
	  val neighbour = getNeighbour(maximalItem._1, set.toList, edges)
	  
	  neighbour match {
	 	  case None => (chain, neighbour)
	 	  case Some(v) => ((v, maximalItem._2 + 1) :: chain, neighbour)
	  }
  }
  
  def getMaximalItem(l: List[(ModelVertex, Int)]) = l.filter(_._2 == l.map(_._2).max).apply(0)
  def real(opt: Option[ModelVertex]) = opt match {
	  case Some(v) => List(v)
	  case None => Nil
  }
  
  def completeChains(hs: List[List[(ModelVertex, Int)]], set: Set[ModelVertex], edges: List[ModelEdge]): List[List[(ModelVertex, Int)]] = {
	  val size =  set.size
	  
	  if(size == 0) hs 
	  else {
	 	  val enrichedChains = hs.map(chain => enricheChain(chain, set, edges))
	 	  val newChains = enrichedChains.map(_._1).toList
	 	  val newSet = set -- List.flatten(enrichedChains.map(opt => real(opt._2)))
	 	  if(newSet.size < size) completeChains(newChains, newSet, edges) else newChains
	  }
  }
	 
  val vertices = ModelVertex.findAll(By(ModelVertex.referenceId, referenceId))	
  val edges = ModelEdge.findAll(By(ModelEdge.referenceId, referenceId))
  val hs = vertices.filter(_.elementType == "hierarchy").map(h => List((h, 0))).toList 
  val ls = Set() ++ vertices.filter(_.elementType == "level").toList
  
  completeChains(hs, ls, edges)
 }
 
 def above(v: ModelVertex, hierarchy: List[(ModelVertex, Int)]): List[ModelVertex] = hierarchy match {
	 case Nil => Nil
	 case head :: tail => if(head._1 == v) tail.map(_._1) else above(v, tail)
 }
 
 def attributesAbove(v: ModelVertex): List[ModelVertex] = {
   val allHierarchies = hierarchies(v.referenceId)
   val matchingHierarchy = List.flatten(allHierarchies.map(chain => chain.filter(h => isConnected(h._1.id, v.id)).map(_._1))).distinct
   
   if(matchingHierarchy.size != 1) Nil
   else {
	   val hierarchiesAbove = List.flatten(allHierarchies.map(chain => above(matchingHierarchy(0), chain))).distinct 
	   val attrs = ModelVertex.findAll(By(ModelVertex.fkScenario, v.fkScenario), By(ModelVertex.elementType, "attribute"), By(ModelVertex.referenceId, v.referenceId))
	   attrs.filter(attr => isConnectedToSomeVertex(attr, hierarchiesAbove)).toList
   }
 }
 
 def measureLoadCycle(m: Measure, isAboutActuality: Boolean): Long = {
  val unit = if(isAboutActuality) m.requiredActualityUnit else m.requiredStorageUnit
  val value = if(isAboutActuality) m.requiredActualityValue else m.requiredStorageValue
  
  if(unit == null || value == null) 0
  else {
	  
	  unit.toString match {
	 	  case "seconds" => 1
	 	  case "tMinutes" => 60
	 	  case "hours" => 60 * 60
	 	  case "calendarDays" => 60 * 60 * 24
	 	  case "workingDays" => floor(60 * 60 * 24 * 5/7).toLong
	 	  case "weeks" => 60 * 60 * 24 * 7
	 	  case "years" => 60 * 60 * 24 * 365
	 	  case _ => 60 * 60 * 24 * 30
	  }
  }
 }
 
 def cubeLoadCycle(v: ModelVertex) = {
  val cycles = Measure.findAll(By(Measure.fkCube, v.id)).map(m => measureLoadCycle(m, true))
  if(cycles.isEmpty) 0 else cycles.min
 }
 
 def loadCycle(v: ModelVertex): Long = {
  v.elementType.toString match {
	  case "cube" => cubeLoadCycle(v)
	  case "dimension" => {
	 	  val clones = ModelVertex.findAll(By(ModelVertex.elementName, v.elementName), By(ModelVertex.elementType, "dimension"), By(ModelVertex.fkScenario, v.fkScenario))
	 	  val cubes = ModelVertex.findAll(By(ModelVertex.elementType, "cube"), By(ModelVertex.fkScenario, v.fkScenario))
	 	  //cubes.filter(connectedToSome(clones)).map(cubeLoadCycle).min
	 	  cubes.map(cubeLoadCycle).min
	  }
	  case "level" => {
	 	  10
	  }
	  case _ => 0 
  }
 }
 
 def initialSize(v: ModelVertex): String = {
  val details = v.elementDetail.split(";").toList
  if(details.size == 2) details.apply(0).toString else ""
 }
 
 def initialSizeAsNumber(v: ModelVertex): Long = {
  val details = v.elementDetail.split(";").toList
  if(details.size == 2) details.apply(0).toLong else 0
 }
 
 def growth(v: ModelVertex): String = {
  val details = v.elementDetail.split(";").toList
  if(details.size == 2) details.apply(1).toString else ""
 }
 
 def growthAsNumber(v: ModelVertex): Long = {
  val details = v.elementDetail.split(";").toList
  if(details.size == 2) details.apply(1).toLong else 0
 }
 
 def computeSize(tableKind: String, initialSize: Long, growth: Long, end: Long, begin: Long, initialDate: Long, cycle: Long): Long = {
  if(tableKind == "transactionTable") Math.ceil(MyUtil.duration(end, begin) * 60 * 60 * 24/cycle).toLong * growth
  else Math.ceil(MyUtil.duration(end, initialDate) * 60 * 60 * 24/cycle).toLong * growth + initialSize
 }
 
 def computeReducedSize(v: ModelVertex, tableName: String, end: Long, begin: Long, initialLoadDate: Long, ratio: Long): Int = {
  val initialSize = initialSizeAsNumber(v)
  val growth = growthAsNumber(v)
  val cycle = loadCycle(v)
  val estimatedSize = computeSize(tableName, initialSize, growth, end, begin, initialLoadDate, cycle)
  Math.floor(estimatedSize/ratio).toInt
 }
 
 def findJoinPath(factTable: PTable, attr: PAttribute): List[PAttribute] = {
	 
  def f(source: PAttribute, target: PTable, path:List[PAttribute]): List[PAttribute] = {
	  val next = PAttribute.findAll(By(PAttribute.reference, source.id))
	  if(next.isEmpty) path 
	  else {
	 	  val fk = next(0)
	 	  val t = getTable(fk)
	 	  if(t == target) fk :: path else f(fk, target, fk :: path )
	  }
  }
  
  val keyAttribute = PAttribute.findAll(By(PAttribute.fkPTable, attr.fkPTable), By(PAttribute.isPrimaryKey, 1))
  if(keyAttribute.isEmpty) Nil 
  else {
	  f(keyAttribute(0), factTable, List(keyAttribute(0)))
  }
 }
 
 // helper method for isPartOf
 
 def isIn(x: PAttribute, l: List[PAttribute]): Boolean = l match {
  case Nil => false
  case head :: tail => if (x == head) true else isIn(x, tail)
 }
 
 def getTable(a: PAttribute) = PTable.findAll(By(PTable.id, a.fkPTable)).apply(0)
 
 // check whether left list is part of right list
 
 def isPartOf(l1: List[PAttribute], l2: List[PAttribute]) = (true /: l1.map(x => isIn(x, l2))) (_ && _)
 
 def createJoinStatement(paths: List[List[PAttribute]]): String = {
  def reduce(ls: List[List[PAttribute]]): List[List[PAttribute]] = ls match {
	  case Nil => Nil
	  case head :: tail => if((false /:tail.map(t => isPartOf(head, t))) (_ || _)) reduce(tail) else head :: reduce(tail) 
  }
  
  def onePath(l: List[PAttribute]): List[String] = l match {
	  case l :: r :: tail => getTable(l).name + "." + l.name + " = " + getTable(r).name + "." + r.name :: onePath(r:: tail)
	  case _ => List("")
  }
  
  val sortedPaths = paths.sort(_.size < _.size) 
  val reducedPaths = reduce(sortedPaths)
  ("" /: List.flatten(reducedPaths.map(a => onePath(a))).filter(_.length > 0)) (_ + " AND " + _)
 }
 
 def organizeLevel(organizedLevel: List[(ModelVertex, Int)], remainingLevel: Set[ModelVertex], n: Int): List[(ModelVertex, Int)] = {
  val nextLevel = remainingLevel.toList.filter(l => isConnectedToSomeVertex(l, organizedLevel.map(_._1)))
  val expandedOrganization = nextLevel.map(l => (l,n)).toList ::: organizedLevel
  val remainder = remainingLevel -- nextLevel
  if(remainder.isEmpty) expandedOrganization else organizeLevel(expandedOrganization, remainder, n+1)
 }
 
 def organizeAllLevel(referenceId: Long): List[(ModelVertex, Int)] = {
  val hierarchies = ModelVertex.findAll(By(ModelVertex.referenceId, referenceId), By(ModelVertex.elementType, "hierarchy")).map(h => (h,0)).toList 
  val allLevel = Set() ++ ModelVertex.findAll(By(ModelVertex.referenceId, referenceId), By(ModelVertex.elementType, "level"))
  organizeLevel(hierarchies, allLevel, 1)
 }
 
 def getHierarchyNumber(level: ModelVertex, organization: List[(ModelVertex, Int)]): Int = organization match {
	 case Nil => -1
	 case head :: tail => if(level == head._1) head._2 else getHierarchyNumber(level, tail)
 }
 
 def findLevelBelow(level: ModelVertex, otherLevel: List[ModelVertex], organization: List[(ModelVertex, Int)]): Option[ModelVertex] = otherLevel match {
	 case Nil => None
	 case head :: tail => {
		 if(head.referenceId == level.referenceId && getHierarchyNumber(head, organization) <= getHierarchyNumber(level, organization)) Some(head)
		 else findLevelBelow(level, tail, organization)
	 }
 }
 
 def determineAggregation(m: Measure, attrs: List[PAttribute], organizedLevel: List[(ModelVertex, Int)]): String = {
  def aggr(opt: Option[ModelVertex], m: Measure): String = opt match {
	  case None => "sum"
	  case Some(mv) => {
	 	  val links = MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkMeasure, m.id), By(MeasureToModelVertex.fkLevel, mv.id))
	 	  if(links.isEmpty) "sum" else links(0).aggregation
	  }
  }
  
  val modelAttrs = List.flatten(attrs.map(a => ModelVertex.findAll(By(ModelVertex.id, a.fkModelAttribute))))
  val levelOfAttributes = List.flatten(modelAttrs.map(ma => ModelVertex.findAll(By(ModelVertex.referenceId, ma.referenceId), By(ModelVertex.elementType, "level")).filter(l => isConnected(l.id, ma.id)))) 
  val levelOfMeasure = List.flatten(MeasureToModelVertex.findAll(By(MeasureToModelVertex.fkMeasure, m.id)).map(link => ModelVertex.findAll(By(ModelVertex.id, link.fkLevel))))	 
  val levelsBelow = levelOfAttributes.map(loa => findLevelBelow(loa, levelOfMeasure, organizedLevel)).filter(_ != None)
  if(levelsBelow.isEmpty) "sum" else levelsBelow.map(lb => aggr(lb, m)).apply(0)
 }
 
 def createSelect(attrs: List[(PAttribute, String, String, String)], msrs: List[(Measure, String, String)], fact: PTable, filter: String): String = {
  def makeLevel(attrs: List[(PAttribute, String, String, String)]) = {
   if(attrs.isEmpty) Nil else {
	   val representative = ModelVertex.findAll(By(ModelVertex.id, attrs(0)._1.fkModelAttribute)).apply(0)
	   organizeAllLevel(representative.referenceId)	
   }
  }
  
  def maybeSomething(whatFollows: String, something: String) = if(whatFollows.length > 0) something else ""
  
  //val representative = ModelVertex.findAll(By(ModelVertex.id, attrs(0)._1.fkModelAttribute)).apply(0)
  val organizedLevel = makeLevel(attrs)
	 
  val select = "SELECT "
  val attrList = MyUtil.makeSeparatedList(attrs.map(a => a._2 + "." + a._3),",")
  val msrList = MyUtil.makeSeparatedList(msrs.map(m => determineAggregation(m._1, attrs.map(_._1), organizedLevel) + "(" + m._2 + "." + m._3 + ")"),",")
  val from = MyUtil.makeSeparatedList(List(fact.name.toString) ::: attrs.map(_._2).distinct, ",")
  val joins = createJoinStatement(attrs.map(a => findJoinPath(fact, a._1)))
  val groupBy = if(attrList.size > 0) "\nGROUP BY " + attrList else ""
  val orderedAttributes = attrs.filter(_._4.length > 0).map(a => a._2 + "." + a._3 + " " + a._4)
  val orderBy = if(orderedAttributes.size > 0) "\nORDER BY " + MyUtil.makeSeparatedList(orderedAttributes, ",") else ""
  val conjunction = if(joins == null || joins.length == 0) "" else " AND " 
	  
	  
  select + attrList  + maybeSomething(msrList, ", ") + msrList + "\nFROM "  + from + maybeSomething(joins + filter, "\nWHERE ") + joins + conjunction + filter  + groupBy + orderBy 
 }
 
 def findOriginal(v: ModelVertex): ModelVertex = {
  val org =  ModelVertex.findAll(By(ModelVertex.fkScenario, v.fkScenario), 
		                         By(ModelVertex.elementType, v.elementType),
		                         By(ModelVertex.elementName, v.elementName),
		                         By(ModelVertex.elementKind, "original"))
		                         
  if(org.isEmpty) null else org(0)
 } 



/**
 * consistency checks: Connectedness of diagrams, correct relation types
 */

 def checkPair(v1: ModelVertex, v2:ModelVertex): Boolean = {
  val allowedCombinations = List(("cube", "dimension"), ("dimension", "hierarchy"), ("hierarchy", "level"), ("dimension", "level"), ("level", "level"), ("level", "attribute"), ("level", "scope"), ("level", "element"))	
  allowedCombinations.exists(item =>  item == (v1.elementType, v2.elementType) || item == (v2.elementType, v1.elementType))
 }

 def checkEdge(v1: ModelVertex, v2:ModelVertex, edges: List[ModelEdge]): (Int, ModelEdge) = edges match {
	case Nil => (0, null)
	case e :: es => {
		if((e.head == v1.id && e.tail == v2.id) || (e.head == v2.id && e.tail == v1.id)) {
			if(checkPair(v1, v2)) (1, null) else (2, e)
		}
		else checkEdge(v1, v2, es)
	}
 }

 def neighbours(v: ModelVertex, otherVertices: List[ModelVertex], edges: List[ModelEdge], alreadyFound: List[(ModelVertex, ModelEdge)]): List[(ModelVertex, ModelEdge)] = otherVertices match {
	case Nil => alreadyFound
	case ov :: ovs => {
		val checkedEdge = checkEdge(v, ov, edges)
		checkedEdge._1 match {
			case 0 => neighbours(v, ovs, edges, alreadyFound)
			case 1 => neighbours(v, ovs, edges, (ov, null) :: alreadyFound)
			case 2 => neighbours(v, ovs, edges, (ov, checkedEdge._2) :: alreadyFound)
		}
	}
 }
 
 def component(alreadyFound: List[ModelVertex], otherVertices: List[ModelVertex], edges: List[ModelEdge], badEdges: List[ModelEdge]): (List[ModelVertex], List[ModelEdge]) = {
	 
  if(otherVertices.isEmpty) (alreadyFound, badEdges) 
  else {
	  val matches = alreadyFound.map(v => (v, neighbours(v, otherVertices, edges, Nil)))
	  val newVertices = List.flatten(matches.map(_._2.map(_._1))).distinct
	  val withNewMatches = alreadyFound ::: newVertices
	  val remainingVertices = otherVertices.filter(ov => !newVertices.exists(_ == ov))
	  val newBadEdges = List.flatten(matches.map(item => item._2.map(innerItem => (item._1, innerItem._1, innerItem._2)))).filter(_._3 != null)
	  
	  if(newVertices.isEmpty) (alreadyFound, badEdges) 
	  else component(withNewMatches, remainingVertices, edges, badEdges ::: newBadEdges.map(_._3))
  }
 }

 def checkGraph(vertices: List[ModelVertex], edges: List[ModelEdge]): List[(String, Long, Long, Long)] = {
	 
  if(vertices.isEmpty && !edges.isEmpty) List(("noVertices", edges(0).referenceId,0,0))
  else {
	 val checkResult = component(List(vertices.head), vertices.tail, edges, Nil)
	 val badEdges = checkResult._2.map(badEdge => ("badEdge", badEdge.referenceId.toLong, badEdge.head.toLong, badEdge.tail.toLong))
	 val zero = 0
	 println("found " + checkResult._1.size.toString + " absolute " + vertices.size.toString)
	 if(checkResult._1.size < vertices.size) ("isolatedVertices", vertices.head.referenceId.toLong,zero.toLong, zero.toLong) :: badEdges else badEdges
  }
 }

 def checkAllGraphs(scenarioId: Long): List[(String, Long, Long, Long)] = {
  val vertices = ModelVertex.findAll(By(ModelVertex.fkScenario, scenarioId), By(ModelVertex.isCurrent, 1))
  val edges = ModelEdge.findAll(By(ModelEdge.fkScenario, scenarioId), By(ModelEdge.isCurrent, 1))
  val rIds = vertices.map(_.referenceId).distinct
  
  val results = for {
	 rId <- rIds
	 val vs = vertices.filter(_.referenceId == rId)
	 val es = edges.filter(_.referenceId == rId)
  } yield checkGraph(vs, es)
  
  List.flatten(results)
 }
 
 def presentGraphResult(errors: List[(String, Long, Long, Long)]): Node = {
  def v(id: Long): (String, String) = {
   if(id == 0) ("", "") 
   else {
	   val vs = ModelVertex.findAll(By(ModelVertex.id, id))
	   if(vs.isEmpty) ("", "")  else (vs(0).elementType.toString, vs(0).elementName.toString)
   }
  }
  
  def row(item: (String, Long, Long, Long)): Node = {
	val referringVertex = v(item._2)
	val badHead = v(item._3)
    val badTail = v(item._4)
	val badEdge = if(item._3 > 0 && item._4 > 0) ", " + S.?(badHead._1) + " " + badHead._2 + ", " +  S.?(badTail._1) + " " + badTail._2 else ""
    		  
	<li>{S.?("diagram") + " " + S.?(referringVertex._1) + " " + S.?(referringVertex._2) + ": " +  S.?(item._1) + badEdge}</li>
  }

  <ul style="list-style-position:inside">{errors.map(row).toSeq}</ul>	 
 
 }
}