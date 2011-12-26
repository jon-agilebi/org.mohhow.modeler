package org.mohhow.bi.lib

import scala.xml._
import java.io._
import org.apache.commons.io._

object Repository {
	
 private val configurationRoot = "/Users/Jon/MOHHOW/test/configuration/"
 private val deploymentRoot = "/Users/Jon/MOHHOW/test/deployment/"
 private val scenarioRoot = "/Users/Jon/MOHHOW/test/scenario/"
  
 def write(category: String, scenarioId: Long, artefactKind: String, artefactName: String, releaseId: Long, rootNode: Node) {
   val folder = getScenarioSubFolder(category, scenarioId.toString, artefactKind, releaseId, true)
   val path = folder + artefactName + getSuffix(artefactKind)
   XML.save(folder + artefactName + getSuffix(artefactKind), rootNode)
 }
  
 def read(category: String, scenarioId: Long, artefactKind: String, artefactName: String, releaseId: Long): NodeSeq = {
  val folder = getScenarioSubFolder(category, scenarioId.toString, artefactKind, releaseId, false)
  getXML(folder + artefactName + getSuffix(artefactKind))
 }
  
 def getXML(path: String): NodeSeq = {
  try {
	 	  XML.loadFile(path)
  } catch {
	  case ex: Exception => println(ex.toString); <nothing />
  }
 }
 
 def getSuffix(artefactKind: String) = artefactKind match {
  case "vision" => ".xml"
  case "setup" => ".xml"
  case "ddl" => ".sql"
  case "sql" => ".sql"
  case _ => ".xml"
 }
 
 def getScenarioSubFolder(category: String, scenarioId: String, artefactKind: String, releaseId: Long, createFolder: Boolean) = category match {
  case "configuration" => configurationRoot  // examples for setup.xml, nodes.xml, vision.xml
  case "scenario" => {
	  val scenarioPath = scenarioRoot + "scenario" + scenarioId.toString + "/" + artefactKind + "/"
	  val folder = new File(scenarioPath)
	  if(!folder.exists()) FileUtils.forceMkdir(folder)
	  scenarioPath
  }
  case "deployment" => {
	  val deploymentPath = deploymentRoot + "deployment/" + scenarioId.toString + "/" + artefactKind + "/"
	  val folder = new File(deploymentPath)
	  if(!folder.exists()) FileUtils.forceMkdir(folder)
	  deploymentPath
  }
 }
 
 def createScenario(scenarioId:Long) {
  var path = ""
  val scenarioPath = scenarioRoot + "scenario" + scenarioId.toString + "/"
  val scenarioFolder = new File(scenarioPath)
  println("Path: " + scenarioPath)
  scenarioFolder.mkdir()
  
  val artefacts = List("setup", "vision", "scorecards")
  
  for(artefact <- artefacts) {
	  path = scenarioPath + artefact + "/"
	  println(path)
	  var f = new File(path)
	   var source = new File(configurationRoot + "/" + artefact + "/")
	  f.mkdir()
	  
	  FileUtils.copyDirectory(source, f)
  }
 }
 
 def createDeployment(scenarioId:Long): Boolean ={
  var path = ""
  val deploymentPath = deploymentRoot + "deployment/" + scenarioId.toString + "/" 
  val deploymentFolder = new File(deploymentPath)
  deploymentFolder.mkdir()
  
  val artefacts = List("ddl", "sql")
  
  for(artefact <- artefacts) {
	  path = deploymentPath + artefacts + "/"
	  var f = new File(path)
	  f.mkdir()
  }
  
  true
 }
}