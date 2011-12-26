package org.mohhow.snippet

import org.mohhow._
import model._
import net.liftweb._
import net.liftweb.ldap._
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
import net.liftweb.common.Empty
import java.util.Date
import org.mohhow.bi.util.{Utility => MyUtil}
import org.mohhow.bi.lib.Repository
//import scala.collection.mutable.Map

object SelectedUser extends SessionVar[User](null)
object SelectedRole extends SessionVar[String](null)
object SelectedUserRole extends SessionVar[ScenarioRole](null)
object RelevantUserGroup extends SessionVar[UserGroup](null)
object Roles extends SessionVar[NodeSeq](null)
object SelectedProvider extends SessionVar[Provider](null)

class RoleSnippet {
	
 val allRoles = List(("owner", "Scenario Owner"), ("analyst", "BI Analyst"), ("designer", "Data Model Designer"), ("business", "Business Representative"), ("release", "Release Manager"))
		             
 def getLiteralRole(role: String) = (allRoles filter (_._1 == role)).apply(0)._2
	
 def in(html: NodeSeq) = if (User.loggedIn_?) html else NodeSeq.Empty
		
 def out(html: NodeSeq) = if (!User.loggedIn_?) html else NodeSeq.Empty
	 
 def inScenario(html: NodeSeq) = {
  if (SelectedScenario.is != null) html 
  else {
	 <div>
	  <div class="upper_right"></div>
	  <div class="rightContent">
	 	<h3>You have to choose a scenario at first!</h3><br />
	 	{link("/index", () => Unit, <span>Choose a scenario on the index page</span>)}
	 </div>
	</div>
  }
 }
		
 def hasRole (roleName: String): Boolean = {
  if (SelectedScenario.is != null && User.loggedIn_?) {
		 !ScenarioRole.findAll(By(ScenarioRole.fkScenario, SelectedScenario.is.id), By(ScenarioRole.fkUser, User.currentUser), By(ScenarioRole.role, roleName)).isEmpty
  }
  else false
 }
 
 def userTasks() : NodeSeq = {
  if(User.loggedIn_?) {

	  <h3>Meetings</h3>
	  <h3>Meeting Minutes</h3>
	  <h3>Minutes to review</h3>
	  <h3>Requirements</h3>
	  <h3>Measure and Dimension Candidates</h3>
	  <h3>Logical Data Model</h3>
	  <h3>Release Management</h3>
	  <h3>Sprint Tasks</h3>
	  <h3>None of the above</h3>
	  
   } else <nothing />
 }
 
 def tasks (xhtml: NodeSeq): NodeSeq = {
  bind("task", xhtml, "tasks"  -> userTasks())
 }
 
 def addRole(): JsCmd = {
  if(SelectedUser.is != null && SelectedRole.is != null){
	  if(ScenarioRole.findAll(By(ScenarioRole.fkScenario, SelectedScenario.is.id), By(ScenarioRole.fkUser, SelectedUser.is.id), By(ScenarioRole.role, SelectedRole.is)).isEmpty) {
	 	val newRole = ScenarioRole.create
	 	newRole.role(SelectedRole.is).fkUser(SelectedUser.is).fkScenario(SelectedScenario.is).dateCreated(new Date).save
	 	SetHtml("userRoleTableContainer", userRoles())
	  }
	  else Alert("The chosen user with the chosen role is already part of the scenario.")  
  }
  else Alert("No user or role chosen")
	 
 }
 
 def removeRole(): JsCmd = {
  if(SelectedRole.is != null){
	val role = ScenarioRole.findAll(By(ScenarioRole.id, SelectedUserRole.is.id)).apply(0)
	role.delete_!
  	SetHtml("userRoleTableContainer", userRoles())	  
  }
  else
  {
	Alert("No role chosen")
  }
 }
 
 def selectUserRole(id : String) : JsCmd = {
  SelectedUserRole(ScenarioRole.findAll(By(ScenarioRole.id, id.toLong)).apply(0))
  JsRaw("$('#userRoleTable tbody tr').removeClass('zebraHover');$(this).addClass('zebraHover');")
 }
 
 def selectRole(role : String) : JsCmd = {
  SelectedRole(role)
  JsRaw("$('#roleTable tbody tr').removeClass('zebraHover');$(this).addClass('zebraHover');")
 }
 
 def userRole (role: ScenarioRole) = {
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('role')"), selectUserRole _)._2
  val theUser = User.findAll(By(User.id, role.fkUser))
  
  if(!theUser.isEmpty) <tr><td>{theUser(0).firstName + " " + theUser(0).lastName}</td><td>{getRoleName(role.role)}</td></tr> % new UnprefixedAttribute("role", role.id.toString, Null) % ("onclick" -> action)
  else <tr><td></td><td>{getRoleName(role.role)}</td></tr> % new UnprefixedAttribute("role", role.id.toString, Null) % ("onclick" -> action)
 } 
 
 def selectUser(id : String) : JsCmd = {
  SelectedUser(User.findAll(By(User.id, id.toLong)).apply(0))
  JsRaw("$('#userTable tbody tr').removeClass('zebraHover');$(this).addClass('zebraHover');")
 }  

 def displayUser(user: User) = {
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('user')"), selectUser _)._2
	 <tr><td>{user.firstName  + " " +  user.lastName}</td></tr>  % new UnprefixedAttribute("user", user.id.toString, Null) % ("onclick" -> action)
 }
 
 def roleRow(aRole: Node) = {
   val action = SHtml.ajaxCall(JsRaw("$(this).attr('role')"), selectRole _)._2
	<tr><td>{MyUtil.getNodeText((aRole \\ "name").apply(0))}</td><td>{MyUtil.getNodeText((aRole \\ "description").apply(0))}</td></tr> % new UnprefixedAttribute("role", MyUtil.getNodeText((aRole \\ "identifier").apply(0)), Null) % ("onclick" -> action)
 }
 
 def getRoleName(identifier: String) = { 
  def item(role: Node) = if(MyUtil.getNodeText((role \\ "identifier").apply(0)) == identifier) MyUtil.getNodeText((role \\ "name").apply(0)) else ""
  
  if(Roles.is == null) Roles((Repository.read("scenario", SelectedScenario.is.id, "setup","setup", -1) \\ "role").map(Utility.trim))
  val matchedRole = Roles.is.map(item).filter(_ != "")
  if(matchedRole.isEmpty) "" else matchedRole(0)
 }
 
 def roles(): NodeSeq = {
  if(Roles.is == null) Roles((Repository.read("scenario", SelectedScenario.is.id, "setup","setup", -1) \\ "role").map(Utility.trim))
 
  <table id="roleTable" class="protocolTable">
  	<thead>
		<tr><td>Role</td><td>Description</td></tr>
  	</thead>
    <tbody>
	 	{Roles.is.map(roleRow).toSeq}
 	</tbody>
  </table>
 }
 
 def userTable(): NodeSeq = {
	 <table id="userRoleTable" class="protocolTable">
		 <tbody>
	        {User.findAll().map(displayUser).toSeq}
	     </tbody>
	  </table> 
 }
 
 def userRoles(): NodeSeq = {
	 <table id="roleTable" class="protocolTable">
		 <thead>
	     	<td>User</td>
	        <td>Role</td>
	     </thead>
	     <tbody>
	         {ScenarioRole.findAll(By(ScenarioRole.fkScenario, SelectedScenario.is.id)).map(userRole).toSeq}         
	     </tbody>
	</table>
 }
 
 def scenarioRoles(xhtml: NodeSeq): NodeSeq = {
   	bind("role", xhtml, "add"		-> ajaxButton("Add", addRole _) % ("class" -> "standardButton"),
   						"remove" 	-> ajaxButton("remove", removeRole _) % ("class" -> "standardButton"),
			                "assign" 	-> ajaxButton("assign", Noop _) % ("class" -> "standardButton"),
                         "userRoles"   	-> userRoles(),
                         "user" 		-> userTable(),
                         "roles"	-> roles())  	
 }
 
 def addProvider(): JsCmd = {
  val newProvider = Provider.create
  newProvider.dateCreated(new Date)
  SelectedProvider(newProvider)
  RedirectTo("providerEdit")
 }
 
 def editProvider(): JsCmd = if(SelectedProvider.is != null) RedirectTo("providerEdit") else Alert("No provider chosen")
	 
 def removeProvider(): JsCmd = {
  if(SelectedProvider.is != null) { 
	  val provider = SelectedProvider.is
	  provider.delete_!
	  SelectedProvider(null)
	  RedirectTo("user")
  } else Alert("No provider chosen") 
 }
 
 def selectProvider(id : String) : JsCmd = {
  val p = Provider.findAll(By(Provider.id, id.toLong)).apply(0)
  SelectedProvider(p)
  val conf = createConfiguration(p)
  val myLdap = new LDAPVendor
  myLdap.configure(conf)
  val searchResult = myLdap.search("givenName=Thomas")
  Alert(searchResult.toString)
 } 
 
 def createConfiguration(p: Provider): Map[String, String] = {
	 Map("ldap.url" -> p.url.toString, "ldap.base" -> p.base.toString, "ldap.userName" -> p.userName.toString, "ldap.password" -> "secret", "ldap.authType" -> p.authType.toString)
  //map("ldap.initial_context_factory") = p.initialContextFactory
  //map("lift-ldap.testLookup") = p.testLookup
  //map("lift-ldap.retryInterval") = p.retryIntervall
  //map("lift-ldap.maxRetries") = p.maxRetries.toString
 
 }

 def showProvider(p: Provider): Node = {
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('provider')"), selectProvider _)._2
  val pr = <span class="providerHeader"><b>{p.friendlyName}</b></span>  % new UnprefixedAttribute("provider", p.id.toString, Null) % ("onclick" -> action)
  <li class="listItem">
	{pr} <br />
	<div class="providerDetails" >
  		<br />
  		<label>URL: </label>{p.url} <br/>
	   	<label>Base: </label>{p.base} <br/>
	   	<label>User Name: </label>{p.userName} <br/>
	   	<label>Authentication Type: </label>{p.authType} <br/>
	   	<label>Initial Context Factory: </label>{p.initialContextFactory} <br/>
	   	<label>Test Lookup: </label>{p.testLookup} <br/>
	   	<label>Retry Intervall: </label>{p.retryIntervall} <br/>
	   	<label>Maximal Retries: </label>{p.maxRetries} <br/>
	</div>
  </li> 
 }
 
 def allProvider (xhtml: NodeSeq): NodeSeq = {
  bind("provider", xhtml, "add" 	-> ajaxButton("Add", addProvider _) % ("class" -> "standardButton"),
		                  "remove" 	-> ajaxButton("Remove", removeProvider _) % ("class" -> "standardButton"),
		                  "edit"    -> ajaxButton("Edit", editProvider _) % ("class" -> "standardButton"),
		                  "nodes" -> Provider.findAll().map(showProvider),
		                  "searchResult" -> <nothing />)
 }
 
 def flattenNodeSeq(l: List[NodeSeq]) : NodeSeq = List.flatten(l.map(_.toList)).toSeq 
 
 def createBlockList() : NodeSeq = {
  val specs = Specification.findAll()
  flattenNodeSeq(specs.map(s => <li class="listItem">{s.name}</li>))
 }
 
 def toGroupTree(g : UserGroup) : NodeSeq = {
  val subGroups = UserGroup.findAll(By(UserGroup.parentId, g.id))
  val action = chooseGroup(g.id)
  if(subGroups.length > 0) <li class="listItem" groupId={g.id.toString}>{g.shortName}<ul>{subGroups.map(toGroupTree).toSeq}</ul></li> % ("onclick" -> action) 
  else <li id ={"groupId" + g.id.toString} class="listItem">{g.shortName}</li> % ("onclick" -> action)
 }
  
 def createGroupTree() : NodeSeq = {
  flattenNodeSeq(UserGroup.findAll(By(UserGroup.parentId, 0)).map(toGroupTree))
 }
 
 def addGroup(asSubgroup: Boolean) : JsCmd = {
  val g = UserGroup.create 	 
  if(RelevantUserGroup.is != null && asSubgroup) g.parentId(RelevantUserGroup.is.id) else g.parentId(0)	 
  RelevantUserGroup(g)
  JsRaw("$('#groupNameEditContainer').fadeIn();")
 }
 
 def addMaingroup = addGroup(false)
 def addSubgroup = addGroup(true)
 
 def editGroup() : JsCmd = {
  if(RelevantUserGroup.is != null)
	  JsRaw("$('#groupNameEdit').attr('value', '" + RelevantUserGroup.is.shortName + "'); $('#groupNameEditContainer').fadeIn();")
  else
	  Alert("No group chosen")
 }
 
 def chooseGroup(groupId : Long) : JsCmd = {
  val groups = UserGroup.findAll(By(UserGroup.id, groupId))
	 if(groups.size > 0){
		 RelevantUserGroup(groups.apply(0))
		 JsRaw("$('.listItem').removeClass('listItemSelected');$(this).addClass('listItemSelected')")
	 }
	 else{
		 Alert("Something wrong with the groups")
	 }
 }
 
 def saveGroup(groupName : String) : JsCmd = {
  println(groupName)
  RelevantUserGroup.is.shortName(groupName).save	 
  CmdPair(JsRaw("$('#groupNameEditContainer').fadeOut();"), SetHtml("userGroupTree", createGroupTree()))
 }
 
 def removeGroup() : JsCmd = {
  if(RelevantUserGroup.is != null){
		 RelevantUserGroup.is.delete_!;
		 SetHtml("userGroupTree", createGroupTree())
  }
  else{
	  Alert("No group chosen")
  }
 }
 
 def user (xhtml: NodeSeq): NodeSeq = {
  bind("user", xhtml, "blocks" 	-> createBlockList(),
		              "groups"    -> createGroupTree(),
		              "add" ->  ajaxButton("add", addMaingroup _) % ("class" -> "standardButton"),
		              "addSubgroup" ->  ajaxButton("add subgroup", addSubgroup _) % ("class" -> "standardButton"),
		              "edit" -> ajaxButton("edit", editGroup _) % ("class" -> "standardButton"),
		              "remove" -> ajaxButton("remove", removeGroup _) % ("class" -> "standardButton"),
		              "saveGroup" -> ajaxText("", saveGroup _) % ("id" -> "groupNameEdit"))  
 }
 
 
 
 def assignMartRole() : JsCmd = {
  if(SelectedUserId.is >= 0 && SelectedRoleId.is >= 0){
	  
	val role = MartRole.findAll(By(MartRole.id, SelectedRoleId.is)).apply(0)
	val user = User.findAll(By(User.id, SelectedUserId.is)).apply(0)
	role.fkUser(user).save
  	RedirectTo("/role") 	  
  }
  else
  {
	Alert("No role or user chosen")
  }
 }
 
 /**
  * 
  * 
  * neu
  */
 
 def addLinkToRole(): JsCmd = {
  Noop	 
 }
 
 def removeRoleLink(): JsCmd = {
  Noop	 
 }
 
 def createSpecTree(): NodeSeq = {
  
  def specTree(spec: Specification) = {
   val roles = spec.findRoles
   
   if(roles.isEmpty) <li class="treeItem"><span class="leaf">+</span><span class="emphasizable">{spec.name}</span></li>
   else <li class="treeItem"><span class="leaf">+</span><span class="emphasizable">{spec.name}</span><ul>{roles.map(r => r.roleName).toSeq}</ul></li>
  }
  
  def scenarioTree(scenario: Scenario) = {
	val specs = Specification.findAll(By(Specification.fkScenario, scenario.id), OrderBy(Specification.name, Ascending))
	  
	if(specs.isEmpty) <li class="treeItem"><span class="leaf">+</span><span class="emphasizable">{scenario.name}</span></li>
	else <li class="treeItem"><span class="leaf">+</span><span class="emphasizable">{scenario.name}</span><ul>{specs.map(specTree).toSeq}</ul></li>

  }
  
  Scenario.findAll(OrderBy(Scenario.name, Ascending)).map(scenarioTree).toSeq
 }
 
 def showRoles(): NodeSeq = {
  UserRole.findAll().map(role => <li class='listItem'>{role.roleName}</li>).toSeq	 
 }
 
 def addLinkToGroup(): JsCmd = {
  Noop	 
 }
 
 def removeGroupLink(): JsCmd = {
  Noop	 
 }
 
 def specAndRoles (xhtml: NodeSeq): NodeSeq = {
  bind("spec", xhtml, "addLink" 			-> ajaxButton("Link Role", addLinkToRole _) % ("class" -> "standardButton"),
		              "removeLink" 			-> ajaxButton("Remove Link", removeRoleLink _) % ("class" -> "standardButton"),
		              "specTree"    		-> createSpecTree(),
		              "roles" 				-> showRoles(),
		              "addGroupToRole" 		-> ajaxButton("Link Group", addLinkToGroup _) % ("class" -> "standardButton"),
		              "removeGroupFromRole" -> ajaxButton("Remove Link", removeGroupLink _) % ("class" -> "standardButton"))
 }
 
}