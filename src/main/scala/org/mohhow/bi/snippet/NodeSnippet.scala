package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import http._
import SHtml._
import S._

import js._
import JsCmds._
import JE.{JsRaw,Str}

import mapper._
import util._
import Helpers._

import scala.xml._
import org.mohhow.bi.lib.Repository;

object Nodes extends SessionVar[NodeSeq](Repository.read("configuration", -1, "nodes", "nodes",0).map(Utility.trim))
object SelectedNode extends SessionVar[Node](null)

class NodeSnippet {
	
 def registerNode(): JsCmd = {
	 SelectedNode(newNode())
	 RedirectTo("/nodeEdit")
 }
 
 def editNode(): JsCmd = if(SelectedNode.is != null) RedirectTo("/nodeEdit") else Alert("Please choose a node at first")
 
 def unregisterNode(): JsCmd = {
  if(SelectedNode.is != null) {
		 Noop // TODO aus dem Knoten loeschen
  }
  else Alert("Please choose a node at first") 
 }

 def registration (xhtml: NodeSeq): NodeSeq = {
  bind("nodeMenu", xhtml, "register"  -> ajaxButton("Register Node", registerNode _) % ("class" -> "standardButton"),
		  				  "edit" -> ajaxButton("Edit Node", editNode _) % ("class" -> "standardButton"),
		                  "unregister" -> ajaxButton("Unregister", unregisterNode _) % ("class" -> "standardButton"),
		                  "ping" -> ajaxButton("ping", Noop _) % ("class" -> "standardButton"))
 }
 
 def displayNode(node: Node): Node = {
  val item = link("/node", () => {SelectedNode(node)}, <span>{getNodeText((node \\ "friendlyName").apply(0))}</span>)
  if(node == SelectedNode.is) <li class='listItem zebraHover'>{item}</li> else <li class='listItem'>{item}</li> 
 }
 
 def selectNode(node: Node): JsCmd = {
  SelectedNode(node)   
  RedirectTo("/node")
 }
 
 def getNodeText(node: Node): String = node match {
  case Elem(_, _, _, _, Text(myText)) => myText
  case _ => ""
 }
 
 def storeRows(store: Node) = {
   <tr>
	  <td>{getNodeText((store \\ "alias").apply(0))}</td>
      <td>{getNodeText((store \\ "connectionString").apply(0))}</td>
      <td>{getNodeText((store \\ "user").apply(0))}</td>
      <td>{getNodeText((store \\ "password").apply(0))}</td>
      <td>{getNodeText((store \\ "driver").apply(0))}</td>
   </tr>
 }
 
 def showStoreTable(mode: String, stores: NodeSeq): Node = {
	 
	 <table class="protocolTable nodeInput">
	  	<thead>
			<tr>
				<td>Alias</td><td>Connection String</td><td>User</td><td>Password</td><td>Driver</td>
			</tr>
	  	</thead>
	  	<tbody id={mode + "StoreTableBody"}>
			 {if(mode == "display") stores.map(storeRows).toSeq else stores.map(storeEditRows).toSeq}
	  	</tbody>
	 </table>
 }
 
 def showNodeDetails() = {
  
  val node = SelectedNode.is
  
  if(node != null) {
    val stores = node \\ "store"
    
	 <div>
	 	<span><b>Host:</b> {getNodeText((node \\ "host").apply(0))}</span><br /><br />
	  	<span><b>Port:</b> {getNodeText((node \\ "port").apply(0))}</span><br /><br />
	  	<span><b>System:</b> {getNodeText((node \\ "system").apply(0))}</span><br /><br />
	  	<span><b>Environment:</b> {getNodeText((node \\ "environment").apply(0))}</span><br /><br /><br />
	  	<b>Data Stores</b><br /><br />
	  	 {showStoreTable("display", stores)}
	  </div>
  }
  else <nothing />
 
 }

 def nodes (xhtml: NodeSeq): NodeSeq = {
	 bind("nodes", xhtml, "nodes" -> (Nodes.is \\ "node").map(Utility.trim).map(displayNode).toSeq,
			              "details" -> showNodeDetails())
 }
 
 def storeEditRows(store: Node) = {
   <tr>
	  <td><input type="text" value={getNodeText((store \\ "alias").apply(0))} size="20" maxlength="100" /></td>
      <td><input type="text" value={getNodeText((store \\ "connectionString").apply(0))} size="30" maxlength="500" /></td>
      <td><input type="text" value={getNodeText((store \\ "user").apply(0))} size="20" maxlength="100" /></td>
      <td><input type="text" value={getNodeText((store \\ "password").apply(0))} size="20" maxlength="100" /></td>
      <td><input type="text" value={getNodeText((store \\ "driver").apply(0))} size="20" maxlength="100" /></td>
   </tr>
 }
 
 def storeEditTable() = {
   val node = SelectedNode.is
   
   if(node != null) {
	   val stores = node \\ "store"
	   showStoreTable("edit", stores)
   }
   else <nothing />
 }
 
 def nodeEdit(): Node = {
  if(SelectedNode.is != null ){
	 val node = SelectedNode.is
	  
	 <table class="nodeInput">
	  <tr><td><label>Friendly Name</label></td><td><input type="text" value={getNodeText((node \\ "friendlyName").apply(0))} size="40" maxlength="100" /></td></tr>
	  <tr><td><label>Host</label></td><td><input type="text" value={getNodeText((node \\ "host").apply(0))} size="40" maxlength="100" /></td></tr>
	  <tr><td><label>Port</label></td><td><input type="text" value={getNodeText((node \\ "port").apply(0))} size="10" maxlength="10" /></td></tr>
	  <tr><td><label>System</label></td><td><input type="text" value={getNodeText((node \\ "system").apply(0))} size="40" maxlength="100" /></td></tr>
	  <tr><td><label>Environment</label></td><td><select value={getNodeText((node \\ "environment").apply(0))}><option>DEV</option><option>TEST</option><option>ACCT</option><option>PROD</option></select></td></tr>
	 </table>
  }
  else <nothing />
 }
 
 def cancelEditNode(): JsCmd = RedirectTo("node")
 
 def save(vals: String): JsCmd = {
	 Alert(vals)
 }
 
 def newNode() = {
	 <node technicalName="newNode">
		<friendlyName></friendlyName>
		<host></host>
		<port></port>
		<system></system>
		<environment></environment>
		<dataStores>
		</dataStores>
	</node>
 }

 def dataStoreEdit (xhtml: NodeSeq): NodeSeq = {
	 bind("ds", xhtml, "stores" -> storeEditTable(),
			           "node" -> nodeEdit(),
			           "cancel" -> ajaxButton("Cancel", cancelEditNode _) % ("class" -> "standardButton"),
			           "save" ->  <button class='standardButton'>Save</button> % ("onclick" -> SHtml.ajaxCall(JsRaw("$('.nodeInput').html()"), save _)._2))
 }
 
}