package org.mohhow.bi.lib

import net.liftweb._

import common._
import http._
import rest._
import scala.xml._
import scala.actors._
import java.lang.Math._
import scala.collection.mutable

import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib.{Repository => MyRep}

object DeploymentService extends RestHelper {

 var state = "waiting"
 val deploymentItems = mutable.Map.empty[String, Int]
 
 def stopDeployment() = {
  state = "waiting"
  MyRep.emptyTransfer()
 }
 
 def check(name: String, xml:Node): Boolean = {
  if(xml == null || !deploymentItems.contains(name) || xml.hashCode != deploymentItems(name)) {
	stopDeployment()
	false
  }
  else true
 }
 
 /*
 def dropTablesOfRelease(r: Release, n: Node, aliasName: String) = {
  val tableNames = Repository.getArtefactList(r.id, List("sql")).map(fileName => fileName.substring(0, fileName.indexOf(".")))
  val connectionInformation = BIService.storeMap(aliasName)
  tableNames.map(name => DB.use(connectionInformation) { conn => DB.exec(conn, "DROP TABLE " + name + ";") {rs => "success"}}) 	 
 }
 */
 serve {
	 
  case XmlPost(List("deployment", "start"), request) => {
   println("starte deployment")	  
   
   deploymentItems.clear
   
   for(item <- request._1 \\ "item") {
	val itemName = MyUtil.getSeqHeadText(item \\ "name")
	println(itemName)
	val itemChecksum = MyUtil.getSeqHeadText(item \\ "checksum").toInt
	deploymentItems += (itemName -> itemChecksum)
   }
   
   if(!deploymentItems.isEmpty) {
	state = "deploying"
	MyRep.emptyTransfer()
	
	<deploying />
   }
   else <nothingToDeploy />
  }
  
  case XmlPost(List("deployment", "transfer", kind, name), request) => { 
   println("kind " + kind + " name " + name)
   if(state == "deploying" && check(name, request._1)) {
	   println("check successfull")
	   MyRep.write("deployment", 0, kind, name, 0, request._1)
	   deploymentItems -= name
	   
	   if(deploymentItems.isEmpty) {
	  	   state = "waiting"
	  	   MyRep.deploy(true)
	  	   BIService.isInitialized = false
	  	   <deployed />
	   }
	   else  <transferred />
   }
   else <badData />
  }
 } 
}