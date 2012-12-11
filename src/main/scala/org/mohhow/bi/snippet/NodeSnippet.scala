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
import org.mohhow.bi.lib.Repository
import org.mohhow.bi.util.{Utility => MyUtil}

object Nodes extends SessionVar[NodeSeq](Repository.read("configuration", -1, "nodes", "nodes",0).map(Utility.trim) \\ "node")
object SelectedNode extends SessionVar[Node](null)
object DataStores extends SessionVar[NodeSeq](<store />)
object DatabaseAliases extends SessionVar[List[String]](Nil)

class NodeSnippet {
	
 def newNode() = {
	 <node technicalName="newNode">
		<friendlyName></friendlyName>
		<host></host>
		<port></port>
		<system></system>
		<environment></environment>
		<dataStores></dataStores>
	 	<deployment></deployment>
	</node>
 }
	
 def registerNode(): JsCmd = {
	 SelectedNode(newNode())
	 RedirectTo("/nodeEdit")
 }
 
 def editNode(): JsCmd = if(SelectedNode.is != null) RedirectTo("/nodeEdit") else Alert(S.?("noNodeChoice"))
 
 def unregisterNode(): JsCmd = {
  def tn(node: Node) = (node \ "@technicalName").text
  if(SelectedNode.is != null) {
	
	val newNode = <nodes>{Nodes.is.filter(n => tn(n) != tn(SelectedNode.is))}</nodes>
	Repository.write("configuration", -1, "nodes", "nodes", 0, newNode)
  	Nodes(Repository.read("configuration", -1, "nodes", "nodes",0).map(Utility.trim) \\ "node")
  	SelectedNode(null)
  	RedirectTo("/node")
  }
  else Alert(S.?("noNodeChoice"))
 }
 
 def ping(): JsCmd = {
  if(SelectedNode.is != null) {
   val host = MyUtil.getSeqHeadText(SelectedNode.is \\ "host")
   val port = MyUtil.getSeqHeadText(SelectedNode.is \\ "port")
   val cmd = "$.get('http://" + host + ":" + port + "/bi/ping', function(resp){alert('" + S.?("pingSuccess") + " " + host + "')});"
   JsRaw(cmd) 
  }
  else Alert(S.?("noNodeChoice"))
 }

 def registration (xhtml: NodeSeq): NodeSeq = {
  bind("nodeMenu", xhtml, "register"  -> ajaxButton(S.?("registerNode"), registerNode _) % ("class" -> "standardButton"),
		  				  "edit" -> ajaxButton(S.?("editNode"), editNode _) % ("class" -> "standardButton"),
		                  "unregister" -> ajaxButton(S.?("unregisterNode"), unregisterNode _) % ("class" -> "standardButton"),
		                  "ping" -> ajaxButton(S.?("ping"), ping _) % ("class" -> "standardButton"))
 }
 
 def displayNode(node: Node): Node = {
  val item = link("/node", () => {SelectedNode(node)}, <span>{MyUtil.getSeqHeadText(node \\ "friendlyName")}</span>)
  if(node == SelectedNode.is) <li class='listItem zebraHover'>{item}</li> else <li class='listItem'>{item}</li> 
 }
 
 def selectNode(node: Node): JsCmd = {
  SelectedNode(node)   
  RedirectTo("/node")
 }
 
 def storeRows(store: Node) = {
   <tr>
	  <td>{MyUtil.getSeqHeadText(store \\ "alias")}</td>
      <td>{MyUtil.getSeqHeadText(store \\ "connectionString")}</td>
      <td>{MyUtil.getSeqHeadText(store \\ "user")}</td>
      <td>--</td>
      <td>{MyUtil.getSeqHeadText(store \\ "driver")}</td>
   </tr>
 }
 
 def showStoreTable(mode: String, stores: NodeSeq): Node = {
	 
	 <table class="protocolTable nodeInput">
	  	<thead>
			<tr>
				<td>{S.?("alias")}</td><td>{S.?("connectionString")}</td><td>{S.?("user")}</td><td>{S.?("password")}</td><td>{S.?("driver")}</td>
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
	 	<span><b>{S.?("host")}:</b> {MyUtil.getSeqHeadText(node \\ "host")}</span><br /><br />
	  	<span><b>{S.?("port")}:</b> {MyUtil.getSeqHeadText(node \\ "port")}</span><br /><br />
	  	<span><b>{S.?("system")}:</b> {MyUtil.getSeqHeadText(node \\ "system")}</span><br /><br />
	  	<span><b>{S.?("environment")}:</b> {MyUtil.getSeqHeadText(node \\ "environment")}</span><br /><br /><br />
	  	<b>{S.?("dataStores")}</b><br /><br />
	  	 {showStoreTable("display", stores)}
	  </div>
  }
  else <nothing />
 
 }

 def nodes (xhtml: NodeSeq): NodeSeq = {
	 bind("nodes", xhtml, "nodes" -> (Nodes.is \\ "node").map(Utility.trim).map(displayNode).toSeq,
			              "details" -> showNodeDetails())
 }
 
 def databaseChoice(dbs: List[String], choice: String) = {
	 dbs.map(db => if(db == choice) <option selected="">{db}</option> else <option>{db}</option>)
 }
 
 def readDbs() = {
  if(DatabaseAliases.is != null) DatabaseAliases((Repository.read("configuration", -1, "", "conf_db",0) \\ "alias").map(MyUtil.getNodeText(_)).toList)
  DatabaseAliases.is
 }
 
 def storeEditRows(store: Node) = {
   val choice = MyUtil.getSeqHeadText(store \\ "driver")
	 
   <tr>
	  <td><input type="text" value={MyUtil.getSeqHeadText(store \\ "alias")} size="20" maxlength="100" /></td>
      <td><input type="text" value={MyUtil.getSeqHeadText(store \\ "connectionString")} size="30" maxlength="500" /></td>
      <td><input type="text" value={MyUtil.getSeqHeadText(store \\ "user")} size="20" maxlength="100" /></td>
      <td><input type="password" value={MyUtil.getSeqHeadText(store \\ "password")} size="20" maxlength="100" /></td>
      <td><select value={choice}>{databaseChoice(readDbs(), choice)} </select></td>
   </tr>
 }
 
 def storeEditTable() = {
   val node = SelectedNode.is
   
   if(node != null) showStoreTable("edit", node \\ "store")
   else <nothing />
 }
 
 def getEnvironmentSelection(choice: String): Node = choice match {
	 case "PROD" => <select value='PROD'><option>DEV</option><option>TEST</option><option>ACCT</option><option selected="">PROD</option></select>
	 case "ACCT" => <select value='ACCT'><option>DEV</option><option>TEST</option><option selected="">ACCT</option><option>PROD</option></select>
	 case "TEST" => <select value='TEST'><option>DEV</option><option selected="">TEST</option><option>ACCT</option><option>PROD</option></select>
	 case _ => <select value='DEV'><option selected="">DEV</option><option>TEST</option><option>ACCT</option><option>PROD</option></select>
 }
 
 def nodeEdit(): Node = {
  if(SelectedNode.is != null ){
	 val node = SelectedNode.is
	  
	 <table class="nodeInput">
	  <tr><td><label>{S.?("friendlyName")}</label></td><td><input type="text" value={MyUtil.getSeqHeadText(node \\ "friendlyName")} size="40" maxlength="100" /></td></tr>
	  <tr><td><label>{S.?("host")}</label></td><td><input type="text" value={MyUtil.getSeqHeadText(node \\ "host")} size="40" maxlength="100" /></td></tr>
	  <tr><td><label>{S.?("port")}</label></td><td><input type="text" value={MyUtil.getSeqHeadText(node \\ "port")} size="10" maxlength="10" /></td></tr>
	  <tr><td><label>{S.?("system")}</label></td><td><input type="text" value={MyUtil.getSeqHeadText(node \\ "system")} size="40" maxlength="100" /></td></tr>
	  <tr><td><label>{S.?("environment")}</label></td><td>{getEnvironmentSelection(MyUtil.getSeqHeadText(node \\ "environment"))}</td></tr>
	 </table>
  }
  else <nothing />
 }
 
 def cancelEditNode(): JsCmd = RedirectTo("node")
 
 def addNode(): JsCmd = {
  val l = ("" /: readDbs().map(db => "<option>" + db + "</option>")) (_ + _)
  
  val cell20 = "<td><input type='text' value='' size='20' maxlength='100' /></td>"
  val cell30 = "<td><input type='text' value='' size='30' maxlength='500' /></td>"
  val cellSelect = "<td><select value=''>" +  l + "</select></td>"
  JsRaw("$('#editStoreTableBody').append(\"<tr>" + cell20 + cell30 + cell20 + cell20 + cellSelect + "</tr>\");")
 }
 
 def removeNode(): JsCmd = JsRaw("$('#editStoreTableBody .zebraHover').remove()")
 
 def save(rows: String): JsCmd = {
  def inp2Node(tr: Node) = ((tr \ "td" \ "input").apply(0) \ "@value").text
  def sel2Node(tr: Node) = ((tr \ "td" \ "select").apply(0) \ "@value").text
  
  def chooseTechnicalName(tn: String, suggestion: String): String = {
   if(tn != "newNode") tn 
   else {
	 val names = Nodes.is.map(n => (n \\ "@technicalName").text).toList 
	 val pattern = """[^(a-z|A-Z|0-9|_)]""".r  
	 MyUtil.makeUnique(pattern replaceAllIn(suggestion, "x"), names, 1)
   }
  }
  
  def crNode(trs: NodeSeq) = {
	val tn = (SelectedNode.is \\ "@technicalName").text
	val n = <node technicalName={chooseTechnicalName(tn, inp2Node(trs(0)))}>
				<friendlyName>{inp2Node(trs(0))}</friendlyName>
				<host>{inp2Node(trs(1))}</host>
				<port>{inp2Node(trs(2))}</port>
				<system>{inp2Node(trs(3))}</system>
				<environment>{sel2Node(trs(4))}</environment>
				<dataStores>{DataStores.is}</dataStores>
				<deployment>{SelectedNode.is \\ "deployment"}</deployment>
			</node>
				
	(n, tn == "newNode")
  }
  
  def transformNode(node: Node, transformedNode: Node) = if ((node \\ "@technicalName").text == (transformedNode \\ "@technicalName").text) transformedNode else node
  def newNodes(trN: (Node, Boolean)) = if(trN._2) MyUtil.flattenNodeSeq(Nodes.is.toList ::: List(trN._1)) else Nodes.is.map(n => transformNode(n, trN._1)).toSeq
  
  val table = XML.loadString("<table>" + rows + "</table>")
  val nodes = <nodes>{newNodes(crNode((table \\ "tr")))}</nodes>	 
  Repository.write("configuration", -1, "nodes", "nodes", 0, nodes)
  Nodes(Repository.read("configuration", -1, "nodes", "nodes",0).map(Utility.trim) \\ "node")
  
  RedirectTo("node") 
 }
 
 def saveStores(rows: String): JsCmd = {
  def saveStore(tr: Node) = {
   val inps = tr \ "td" \ "input"
   val inselect = tr \ "td" \ "select"
   		<store>
			<alias>{(inps(0) \ "@value").text}</alias>
			<connectionString>{(inps(1) \ "@value").text}</connectionString>
			<user>{(inps(2) \ "@value").text}</user>
			<password>{(inps(3) \ "@value").text}</password>
			<driver>{(inselect(0) \ "@value").text}</driver>
		</store>  
  }
  
  val trs = XML.loadString("<r>" + rows + "</r>") \ "tr"
  val stores = trs.map(tr => saveStore(tr)).toSeq
  DataStores(stores)
  Noop
 }

 def dataStoreEdit (xhtml: NodeSeq): NodeSeq = {
  val cmd = CmdPair(SHtml.ajaxCall(JsRaw("$('#editStoreTableBody').html()"), saveStores _)._2, SHtml.ajaxCall(JsRaw("$('.nodeInput').html()"), save _)._2)
  
  bind("ds", xhtml, "stores" -> storeEditTable(),
			 		"add" -> ajaxButton("+", addNode _) % ("class" -> "standardButton"),
			 		"remove" -> ajaxButton("-", removeNode _) % ("class" -> "standardButton"),
			        "node" -> nodeEdit(),
			        "cancel" -> ajaxButton(S.?("cancel"), cancelEditNode _) % ("class" -> "standardButton"),
			        "save" ->  <button class='standardButton'>{S.?("save")}</button> % ("onclick" -> cmd))
 }
}