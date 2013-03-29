package org.mohhow.bi.lib

import scala.xml._
import java.io._
import org.apache.commons.io._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._

import scala.collection.mutable._
import scala.collection.JavaConverters._

object Repository {
	
 private val configurationRoot = Props.get("configurationRoot") openOr ""
 private val deploymentRoot = Props.get("deploymentRoot") openOr ""
 private val transferRoot = Props.get("transferRoot") openOr ""
 private val scenarioRoot = Props.get("scenarioRoot") openOr ""
 
 def rootPathsExist(): Boolean = {
	 
	 val conf = new File(configurationRoot)
	 val depl = new File(deploymentRoot)
	 val trans = new File(transferRoot)
	 val scen = new File(scenarioRoot)
	 
	 conf.exists() && depl.exists() && trans.exists() && scen.exists
 }
  
 def write(category: String, scenarioId: Long, artefactKind: String, artefactName: String, releaseId: Long, rootNode: Node) {
   val folder = getScenarioSubFolder(category, scenarioId.toString, artefactKind, releaseId, true)
   val path = folder + artefactName + getSuffix(artefactKind)
   XML.saveFull(folder + artefactName + getSuffix(artefactKind), rootNode, "UTF-8", true, null)
 }
  
 def read(category: String, scenarioId: Long, artefactKind: String, artefactName: String, releaseId: Long): NodeSeq = {
  val folder = getScenarioSubFolder(category, scenarioId.toString, artefactKind, releaseId, false)
  getXML(folder + artefactName + getSuffix(artefactKind))
 }
 
 def readAsFile(category: String, scenarioId: Long, artefactKind: String, artefactName: String, releaseId: Long): File = {
  val folder = getScenarioSubFolder(category, scenarioId.toString, artefactKind, releaseId, false)
  new File(folder + artefactName + getSuffix(artefactKind))
 }
 
 def emptyTransfer() = {
  val transferGeneric = new File(transferRoot + "/generic")
  FileUtils.cleanDirectory(transferGeneric)
  val transferIndividual = new File(transferRoot + "/individual")
  FileUtils.cleanDirectory(transferIndividual)
 }
 
 def emptyRelease(releaseId: Long) = {
  val release = new File(scenarioRoot + "release/" + releaseId.toString)
  if(!release.exists()) release.mkdir()
  FileUtils.cleanDirectory(release)
 }
 
 def writeToTransfer(artefactName: String, isGeneric: Boolean, rootNode: Node) {
  val pathGeneric = transferRoot + "/generic/"
  val pathIndividual = transferRoot + "/individual/"
  
  if(isGeneric) XML.saveFull(pathGeneric + artefactName + ".xml", rootNode, "UTF-8", true, null)
  else XML.saveFull(pathIndividual + artefactName + ".xml", rootNode, "UTF-8", true, null)
 }
 
 def deploy(alsoGeneric: Boolean) = {
	 
  if(alsoGeneric) {
	val generic = new File(deploymentRoot + "/generic")
	FileUtils.cleanDirectory(generic)
	val transferGeneric = new File(transferRoot + "/generic")
	FileUtils.copyDirectory(transferGeneric, generic)
  }
	 
  val individual = new File(deploymentRoot + "/individual")
  FileUtils.cleanDirectory(individual)
  val transferIndividual = new File(transferRoot + "/individual")
  FileUtils.copyDirectory(transferIndividual, individual)
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
  case "blocks" => ".xml"
  case "metadata" => ".xml"
  case "svg" => ".svg"
  case "documentation" => ".pdf"
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
  case "transfer" => {
	  val transferPath = transferRoot + artefactKind + "/"
	  val folder = new File(transferPath)
	  if(!folder.exists()) FileUtils.forceMkdir(folder)
	  transferPath
  }
  case "deployment" => {
	  val deploymentPath = deploymentRoot + artefactKind + "/"
	  val folder = new File(deploymentPath)
	  if(!folder.exists()) FileUtils.forceMkdir(folder)
	  deploymentPath
  }
  case "release" => {
	  val releasePath = scenarioRoot + "release/" + releaseId.toString + "/"
	  val folder = new File(releasePath)
	  if(!folder.exists()) FileUtils.forceMkdir(folder)
	  releasePath
  }
 }
 
 def createScenario(scenarioId:Long) {
  var path = ""
  val scenarioPath = scenarioRoot + "scenario" + scenarioId.toString + "/"
  val scenarioFolder = new File(scenarioPath)
  println("Path: " + scenarioPath)
  scenarioFolder.mkdir()
  
  val artefacts = List("setup", "vision", "blocks", "frames")
  
  for(artefact <- artefacts) {
	  path = scenarioPath + artefact + "/"
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
 
 def writeDDL(releaseId: Long, tableName: String, text: String, isDelta: Boolean) = {
  val ddlPath = if(isDelta) scenarioRoot + "release/" + releaseId.toString + "/delta/" + tableName + ".sql"  else scenarioRoot + "release/" + releaseId.toString + "/" + tableName + ".sql"
  val f = new File(ddlPath)
  FileUtils.writeStringToFile(f, text)
 }
 
 def emptyDocumentation(releaseId:Long): File = {
  val releasePath = scenarioRoot + "release/" + releaseId.toString + "/"
  val f = new File(releasePath + "documentation.pdf")
  f
 }
 
 def getArtefactList(releaseId: Long, extensions: List[String], isDelta: Boolean): List[String] = {
  val content = new ListBuffer[String]
  val releasePath = if(isDelta) scenarioRoot + "release/" + releaseId.toString + "/delta/" else scenarioRoot + "release/" + releaseId.toString + "/"
  val f = new File(releasePath)
  if(f.exists()) {
	  val iterator = FileUtils.iterateFiles(f, extensions.toArray, false)
	  while(iterator.hasNext()) { content += iterator.next().getName}
  }
  content.toList
 }
 
 def getArtefactAsString(releaseId: Long, name:String) = {
  val artefactPath = scenarioRoot + "release/" + releaseId.toString + "/" + name
  val f = new File(artefactPath)
  FileUtils.readFileToString(f)	 
 }
 
 def getMetadataOfRelease(releaseId: Long) = {
  val path = scenarioRoot + "release/" + releaseId.toString + "/metadata.xml"
  getXML(path)
 }
 
 def prepareTestData(name: String) = {
  val path = scenarioRoot + "testdata/" 
  val r = new File(path)
  
  if(!r.exists()) r.mkdir()

  val testDataPath = path + name + "/"
  val t = new File(testDataPath)
  if(t.exists()) (false, "testDataAlreadyExists") 
  else {
	  t.mkdir()
	  (true, testDataPath)
  }
 }
 
 def storeFlatFile(testDataName: String, tableName: String, text: String) = {
  val filePath = scenarioRoot + "testdata/"  + testDataName + "/" + tableName + ".csv" 
  val f = new File(filePath)
  FileUtils.writeStringToFile(f, text)
  filePath
 }
 
 def tableNamesOfRelease(releaseId:Long) = {
  val releasePath = scenarioRoot + "release/"  + releaseId.toString + "/" 
  val r = new File(releasePath)
  val fileNames = for {
	  aFile <- FileUtils.listFiles(r, List("sql").toArray, false).asScala.toList
	  fileName = aFile.getName()				
	 } yield fileName.substring(0, fileName.length - 4)
 fileNames
 }
 
 def copy(kind: String, sourceId: Long, targetId: Long) = {
  val source = getScenarioSubFolder("scenario", sourceId.toString, kind, -1, false)
  val target = getScenarioSubFolder("scenario", targetId.toString, kind, -1, false)
  
  FileUtils.copyDirectory(new File(source), new File(target))
	 
 }
}