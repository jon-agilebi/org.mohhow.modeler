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

import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import net.liftweb.mapper._
import _root_.java.sql.{Connection, DriverManager}

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._

object DeploymentService extends RestHelper {

 var state = "waiting"
 val deploymentItems = mutable.Map.empty[String, Int]

 def stopDeployment() = {
  state = "waiting"
  MyRep.emptyTransfer()
 }
 
 def check(name: String, xml:Node): Boolean = {
  if(xml == null || !deploymentItems.contains(name)){ // || xml.hashCode != deploymentItems(name)) {
	stopDeployment()
	false
  }
  else true
 }
 
 def mappings(): List[(String, List[(String, String)])] = {
  def mapPart(m: Node) = (MyUtil.getSeqHeadText(m \\ "source"), MyUtil.getSeqHeadText(m \\ "target"))	 
	 
  val dbs = Repository.read("configuration", 0, "metadata", "conf_db", -1) \\ "database"
  dbs.map(db => (MyUtil.getSeqHeadText(db \\ "alias"), (db \\ "mapping").map(mapPart).toList)).toList
 }
 
 def replace(alias: String, ddl: String) = {
  def rpl(ddl: String, ms: List[(String, String)]): String = ms match {
	  case Nil => ddl
	  case m :: moreMaps => rpl(ddl.replaceAll(m._1, m._2), moreMaps)
  }
  
  rpl(ddl, List.flatten(dataTypeMapping.filter(_._1 == alias).map(_._2)))
 }
 
 val dataTypeMapping = mappings()
 
 serve {
	 
  case XmlPost(List("deployment", "start"), request) => {
   
   if(MyUtil.getSeqHeadText(request._1 \\ "token") == (Props.get("token") openOr "")) {
   
	   deploymentItems.clear
	   
	   for(item <- request._1 \\ "item") {
		val itemName = MyUtil.getSeqHeadText(item \\ "name")
		val itemChecksum = MyUtil.getSeqHeadText(item \\ "checksum").toInt
		deploymentItems += (itemName -> itemChecksum)
	   }
	   
	   if(!deploymentItems.isEmpty) {
		state = "deploying"
		MyRep.emptyTransfer()
		
		<status>deploying</status>
	   }
	   else <status>nothingToDeploy</status>
   }
   else <status>badToken</status>
  }
  
  case XmlPost(List("deployment", "transfer", kind, name), request) => { 
   if(state == "deploying" && check(name, request._1)) {
	   MyRep.write("transfer", 0, kind, name, 0, request._1)
	   deploymentItems -= name
	   
	   if(deploymentItems.isEmpty) {
	  	   state = "waiting"
	  	   MyRep.deploy(true)
	  	   BIService.isInitialized = false
	  	   <status>deployed</status>
	   }
	   else  <status>transferred</status>
   }
   else <status>badData</status>
  }
  
  case XmlPost(List("deployment", "ddl", name, alias), request) => { 
  
   if(state == "deploying") { // && check(name, request._1)) {
	   val ddl = MyUtil.getSeqHeadText(request._1 \\ "ddl")
	   val connectionInformation = BIService.storeMap(alias)._1
	   
	   try {
	  	   DB.runUpdate("DROP TABLE " + name + ";", Nil, connectionInformation) 
	   }
	   catch {
	  	   case e: Exception => S.warning("Unable to drop table" + name + "; maybe this table did not exist yet.")
	   }
	     
	   try {
	  	   DB.runUpdate(replace(alias, ddl), Nil, connectionInformation)
	   }
	    catch {
	  	   case e: Exception => println(e.toString)
	   }
	     
	  <status>{name + " deployed"}</status>
   }
   else <status>badDDL</status>
  }
 } 
}