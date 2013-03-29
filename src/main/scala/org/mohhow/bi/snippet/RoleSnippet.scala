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
import org.mohhow.bi.lib.Authentification

object SelectedUser extends SessionVar[User](null)
object SelectedRole extends SessionVar[String](null)
object SelectedUserRole extends SessionVar[ScenarioRole](null)
object Roles extends SessionVar[NodeSeq](null)
object SelectedProvider extends SessionVar[Option[Provider]](null)
object ProviderToEdit extends SessionVar[Provider](null)
object ScenarioOwner extends SessionVar[User](null)
object Analysts extends SessionVar[List[User]](Nil)
object Designer extends SessionVar[List[User]](Nil)
object ReleaseManager extends SessionVar[List[User]](Nil)
object SelectedDn extends SessionVar[String](null)
object SelectedRoleDn extends SessionVar[RoleToGroup](null)
object SelectedRoleForDn extends SessionVar[UserRole](null)
object SelectedSpecRole extends SessionVar[UserRole](null)
object SelectedSpecForRole extends SessionVar[Specification](null)
object SelectedClient extends SessionVar[Client](null)
object SelectedUserForClient extends SessionVar[User](null)
object SelectedUserInClient extends SessionVar[User](null)

class RoleSnippet {

 /**
  * Helper methods for page rendering; real content is only delivered when the user is logged in and/or a scenario is chosen
  */

 def in(html: NodeSeq) = if(!Repository.rootPathsExist()) <div class='rightContent'>{S.?("wrongRootConfiguration")}</div> else if (User.loggedIn_?) html else NodeSeq.Empty
 
 def inAsClientAdmin(html: NodeSeq) = {
	 
  val otherwiseNode = 	 <div><div class="upper_right"></div><div class="rightContent"><h3>{S.?("noSuperAdmin")}</h3><br /></div></div>
  
  if (User.loggedIn_?) {
	  val user = User.currentUser openOr null
	  val superAdmin = Props.get("superAdmin") openOr null
	  if(user != null && user.email.toString == superAdmin) html else otherwiseNode
  }
  else otherwiseNode
 }
 
 def inAsDesigner(html: NodeSeq) = if(MyUtil.isDesigner()) html else NodeSeq.Empty
		
 def out(html: NodeSeq) = if (!User.loggedIn_?) <div>{Script(RedirectTo("/user_mgt/login"))}</div> else NodeSeq.Empty
 
 def inScenario(html: NodeSeq) = {
  if (SelectedScenario.is != null) html 
  else {
	 <div>
	  <div class="upper_right"></div>
	  <div class="rightContent">
	 	<h3>{S.?("askForScenario")}</h3><br />
	 	{link("/index", () => Unit, <span>{S.?("linkToScenarioChoice")}</span>)}
	 </div>
	</div>
  }
 }
 
 def inKind(kind: String, html: NodeSeq) = {
  if (Usage.is(kind)._1) html 
  else {
	 <div>
	  <div class="upper_right"></div>
	  <div class="rightContent">
	 	<h3>{S.?("notPartOfScenario")}</h3><br /><br />
	 	{<span>{S.?("notPartOfScenarioExplanation")}</span>}
	 </div>
	</div>
  }
 }
 
 def inVision(html: NodeSeq) = inKind("vision", html)
 def inScorecard(html: NodeSeq) = inAsDesigner(inKind("scorecardDesign", html))
 def inPhysics(html: NodeSeq) = inKind("physicalModel", html)
 
 /**
  * Helper methods to determine the role of the current user with respect to a scenario
  * 
  */
 
 val allRoles = List("owner", "analyst", "designer", "business", "releaseManager")

 def userTasks() : NodeSeq = {
  def findMeeting(mr: MeetingRecipient, scenarioId: Long) = {
   val now = new Date  
   val meetings = Meeting.findAll(By(Meeting.id, mr.fkMeeting), By(Meeting.fkScenario, scenarioId), By_>(Meeting.meetingBegin, now))
   if(meetings.isEmpty) NodeSeq.Empty else <li>{meetings(0).header}</li>
  }
  
  def minutesToWrite(u: User): NodeSeq = {
   val now = new Date
   val moderatedMeetings =  Meeting.findAll(By(Meeting.fkScenario, SelectedScenario.is), By(Meeting.moderator, u.id), By_<(Meeting.meetingEnd, now))
   moderatedMeetings.filter(m => !Minutes.findAll(By(Minutes.fkMeeting, m.id), By(Minutes.status, "new")).isEmpty).map(mi => <li>{mi.header}</li>).toSeq
  }
  
  def mostActualPublished(m: Meeting) = Minutes.findAll(By(Minutes.fkMeeting, m.id), By(Minutes.status, "published"), OrderBy(Minutes.version, Descending))
  
  def minutesToReview(u: User): NodeSeq = {
   val meetingsToReview = Meeting.findAll(By(Meeting.fkScenario, SelectedScenario.is)).filter(m => !mostActualPublished(m).isEmpty)
   meetingsToReview.filter(m => !MeetingRecipient.findAll(By(MeetingRecipient.fkMeeting, m.id), 
		                                                  By(MeetingRecipient.fkUser, u.id), 
		                                                  NotBy(MeetingRecipient.feedbackStatus, "accepted")).isEmpty).map(m => <li>{m.header}</li>).toSeq
  }
  
  def featureToDefine(u: User): NodeSeq = {
   def withoutSolution(mi: Minutes) = {
	val items = ProtocolItem.findAll(By(ProtocolItem.classification, "request"), By(ProtocolItem.fkMinutes, mi.id))
	items.filter(item => ProtocolToBacklog.findAll(By(ProtocolToBacklog.fkProtocolItem, item.id)).isEmpty).map(i => <li>{i.itemNumber.toString + ": " + i.itemText}</li>).toSeq
   }
   
   def showMinutes(mi: Minutes): NodeSeq = {
	val items = withoutSolution(mi)
	if(items.isEmpty) NodeSeq.Empty else <b>{mi.header}</b><br /><br /><ul>{items}</ul>
   }
   
   if(Analysts.is.contains(u)) {
	  val latestPublishedMinutes = Meeting.findAll(By(Meeting.fkScenario, SelectedScenario.is)).map(mostActualPublished).filter(l => !l.isEmpty).map(_.apply(0)).toSeq	  
	  MyUtil.flattenNodeSeq(latestPublishedMinutes.map(mi => showMinutes(mi)).toList)	 
   }
   else NodeSeq.Empty
  }
  
  def designToMake(u: User): NodeSeq = {
   if(Designer.is.contains(u)) {
	  Measure.findAll(By(Measure.fkScenario, SelectedScenario.is), By(Measure.status,"candidate")).map(m => <li>{m.shortName}</li>).toSeq
   }
   else NodeSeq.Empty
  }
  
  if(User.loggedIn_? && SelectedScenario.is != null) {
	  val scenarioId = SelectedScenario.is.id
	  val user = User.currentUser openOr null
	 
	  <h4>{S.?("meetings")}</h4><br />
	   	<ul style="list-style-position:inside">{MeetingRecipient.findAll(By(MeetingRecipient.fkUser, user.id)).map(mr => findMeeting(mr, scenarioId)).toSeq}</ul><br />
	  <h4>{S.?("minutes")}</h4><br />
	    <ul style="list-style-position:inside">{minutesToWrite(user)}</ul><br />
	  <h4>{S.?("minutesToReview")}</h4><br />
	    <ul style="list-style-position:inside">{minutesToReview(user)}</ul><br />
	  <h4>{S.?("requirements")}</h4><br />
	    <ul style="list-style-position:inside">{featureToDefine(user)}</ul><br />
	  <h4>{S.?("designTasks")}</h4><br />
	  	<ul style="list-style-position:inside">{designToMake(user)}</ul>
   } else <nothing />
 }
 
 def tasks (xhtml: NodeSeq): NodeSeq = {
  bind("task", xhtml, "tasks"  -> userTasks())
 }
 
 def updateRoleLists() = {
  Analysts(MyUtil.findRoles(SelectedScenario.is.id, "analyst"))
  Designer(MyUtil.findRoles(SelectedScenario.is.id, "designer"))
  ReleaseManager(MyUtil.findRoles(SelectedScenario.is.id, "releaseManager"))
 }
 
 def addRole(): JsCmd = {
  if(SelectedUser.is != null && SelectedRole.is != null){
	  if(ScenarioRole.findAll(By(ScenarioRole.fkScenario, SelectedScenario.is.id), By(ScenarioRole.fkUser, SelectedUser.is.id), By(ScenarioRole.role, SelectedRole.is)).isEmpty) {
	 	val newRole = ScenarioRole.create
	 	newRole.role(SelectedRole.is).fkUser(SelectedUser.is).fkScenario(SelectedScenario.is).dateCreated(new Date).save
	 	updateRoleLists()
	 	SetHtml("userRoleTableContainer", userRoles())
	  }
	  else Alert(S.?("userRoleCombinationAlreadyExists"))  
  }
  else Alert(S.?("noUserRoleChoice"))
 }
 
 def removeRole(): JsCmd = {
  if(SelectedRole.is != null){
	val role = ScenarioRole.findAll(By(ScenarioRole.id, SelectedUserRole.is.id)).apply(0)
	
	if(role.role.toString == "owner") Alert(S.?("cannotRemoveOwner"))
	else {
		role.delete_!
		updateRoleLists()
		SetHtml("userRoleTableContainer", userRoles())	  
	}
  }
  else Alert(S.?("noRoleChoice"))
 }
 
 def selectRole(role : String) : JsCmd = {
  SelectedRole(role)
  JsRaw("$('#roleTable tbody tr').removeClass('zebraHover');$(\"#roleTable tbody tr[role='" + role + "']\").addClass('zebraHover');")
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
		<tr><td>{S.?("role")}</td><td>{S.?("description")}</td></tr>
  	</thead>
    <tbody>
	 	{Roles.is.map(roleRow).toSeq}
 	</tbody>
  </table>
 }
 
 /**
  * Methods required to build the user table
  * 
  * 
  */
 
 def selectUser(id : String) : JsCmd = {
  SelectedUser(User.findAll(By(User.id, id.toLong)).apply(0))
  JsRaw("$('#userTable tbody tr').removeClass('zebraHover');$(\"#userTable tbody tr[user='" + id + "']\").addClass('zebraHover');")
 }  

 def displayUser(user: User) = {
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('user')"), selectUser _)._2
  <tr><td>{user.firstName  + " " +  user.lastName}</td></tr>  % new UnprefixedAttribute("user", user.id.toString, Null) % ("onclick" -> action)
 }
 
 def userTable(): NodeSeq = {
	 <table id="userTable" class="protocolTable">
		 <tbody>
	        {User.findAll().map(displayUser).toSeq}
	     </tbody>
	  </table> 
 }
 
 /**
  * Methods required to build the user role table
  * 
  */
 
 def selectUserRole(id : String) : JsCmd = {
  SelectedUserRole(ScenarioRole.findAll(By(ScenarioRole.id, id.toLong)).apply(0))
  JsRaw("$('#userRoleTable tbody tr').removeClass('zebraHover');$(\"#userRoleTable tbody tr[role='" + id + "']\").addClass('zebraHover');")
 }
 
 def userRole (role: ScenarioRole) = {
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('role')"), selectUserRole _)._2
  val theUser = User.findAll(By(User.id, role.fkUser))
  
  if(!theUser.isEmpty) <tr><td>{theUser(0).firstName + " " + theUser(0).lastName}</td><td>{getRoleName(role.role)}</td></tr> % new UnprefixedAttribute("role", role.id.toString, Null) % ("onclick" -> action)
  else <tr><td></td><td>{getRoleName(role.role)}</td></tr> % new UnprefixedAttribute("role", role.id.toString, Null) % ("onclick" -> action)
 } 
 
 def userRoles(): NodeSeq = {
	 <table id="userRoleTable" class="protocolTable">
		 <thead>
		 	<tr>
	     		<td>{S.?("user")}</td>
	     		<td>{S.?("role")}</td>
	     	</tr>
	     </thead>
	     <tbody>
	         {ScenarioRole.findAll(By(ScenarioRole.fkScenario, SelectedScenario.is.id)).map(userRole).toSeq}         
	     </tbody>
	</table>
 }
 
 def scenarioRoles(xhtml: NodeSeq): NodeSeq = {
	 
	val canChange = MyUtil.isAnalyst() || MyUtil.isDesigner() 
	val addButton = if(canChange) ajaxButton(S.?("add"), addRole _) % ("class" -> "standardButton")
	                else ajaxButton(S.?("add"), addRole _) % ("class" -> "standardButton") % ("disabled" -> "")
	                
	val removeButton = if(canChange) ajaxButton(S.?("remove"), removeRole _) % ("class" -> "standardButton")
	                   else ajaxButton(S.?("remove"), removeRole _) % ("class" -> "standardButton") % ("disabled" -> "")
	 
   	bind("role", xhtml, "add"		-> addButton,
   						"remove" 	-> removeButton,
                        "userRoles" -> userRoles(),
                        "user" 		-> userTable(),
                        "roles"		-> roles())  	
 }
 
 /**
  *  From now on comes the stuff which concerns the user administration for the scorecard application,
  *  at first handling of provider
  * 
  */
 
 def addProvider(): JsCmd = {
  val newProvider = Provider.create
  newProvider.dateCreated(new Date)
  ProviderToEdit(newProvider)
  SelectedProvider(Some(newProvider))
  RedirectTo("providerEdit")
 }
 
 def editProvider(): JsCmd = {
  if(SelectedProvider.is != null) {  
	  SelectedProvider.is match {
	 	  case Some(p) => {
	 	 	  ProviderToEdit(p)
	 	 	  RedirectTo("providerEdit")
	 	  }
	 	  case _ => Noop
	  }
  }
  else Alert(S.?("noProvider"))
 }
	 
 def removeProvider(): JsCmd = {
  if(SelectedProvider.is != null) {
	  SelectedProvider.is match {
	 	  case Some(p) => {
	 	 	  
	 	 	  // remove all links of roles to groups of this provider and then delete the provider
	 	 	  
	 	 	  RoleToGroup.findAll(By(RoleToGroup.fkProvider, p.id)).map(_.delete_!)
	 	 	  p.delete_!
	 	 	  SelectedProvider(None)
	 	 	  RedirectTo("user")
	 	  }
	 	  case _ => Noop
	  }  
  } else Alert(S.?("noProvider")) 
 }
 
 def rp(): JsCmd = Confirm(S.?("reallyWannaDestroy"), SHtml.ajaxInvoke(() => removeProvider())._2.cmd)
 
 /*
  * Selecting a provides initiates a search on the groups and members of the provider
  * 
  */
 
 def selectProvider(id : String) : JsCmd = {
  def selectDn(groupName : String) : JsCmd = {
   SelectedDn(groupName)
   JsRaw("$('.emphasizableGroup').removeClass('emphasized'); $(\".emphasizableGroup[groupName='" + groupName + "']\").addClass('emphasized');")
  }
  
  def groupToTree(groupName: String, member: List[String], displayName: String) = {
	 println("The member of " + groupName + " are " + member.toString)
	val action = SHtml.ajaxCall(JsRaw("$(this).attr('groupName')"), selectDn _)._2
	<li class="emphasizableGroup">{MyUtil.getDisplayItem(groupName, displayName)}
		<ul style="list-style-position:inside">{member.map(m => <li>{m}</li>).toSeq}
		</ul>
    </li> % new UnprefixedAttribute("groupName", groupName, Null) % ("onclick" -> action)
  }
  
  def usr(role: ScenarioRole): String = {
   val theUser = User.findAll(By(User.id, role.fkUser))
   if(theUser.isEmpty) "" else theUser(0).firstName + " " + theUser(0).lastName
  }
  
  if(id.toLong >= 0) {
  
	val p = Provider.findAll(By(Provider.id, id.toLong)).apply(0)
  	SelectedProvider(Some(p))
  	
  	try {
  	 val conf = MyUtil.createConfiguration(p)
  	 val myLdap = new LDAPVendor
  	 myLdap.configure(conf)
  	 val allGroups = myLdap.search(p.searchTerm)
  	 val tree = allGroups.map(g => groupToTree(g, MyUtil.attr2List(myLdap.attributesFromDn(g + "," + p.base), p.memberAttribute.toString, p.displayAttribute.toString), p.displayAttribute.toString)).toSeq
  	 SetHtml("providerSearchResult", tree)
  	}
	catch {
		case e: Exception => S.error(e.toString); Alert(S.?("erroneousLDAPSearch"))
	}
  }
  else {
	SelectedProvider(None)
	val scenarios = Scenario.findAll()
	val scenarioTree = scenarios.map(sc => groupToTree(sc.name, ScenarioRole.findAll(By(ScenarioRole.fkScenario, sc.id)).map(usr).toList, "notRelevant"))
	SetHtml("providerSearchResult", scenarioTree)
  }
 } 
 
 def showProvider(p: Provider): Node = {
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('provider')"), selectProvider _)._2
  val pr = <span class="providerHeader"><b>{p.friendlyName}</b></span>  % new UnprefixedAttribute("provider", p.id.toString, Null) % ("onclick" -> action)
  <li class="listItem">
	{pr} <br />
	<div class="providerDetails" >
  		<br />
  		<label>{S.?("url")}: </label>{p.url} <br/>
	   	<label>{S.?("base")}: </label>{p.base} <br/>
	   	<label>{S.?("user")}: </label>{p.userName} <br/>
        <label>{S.?("searchTerm")}: </label>{p.searchTerm} <br/>
        <label>{S.?("displayAttribute")}: </label>{p.displayAttribute} <br/>
        <label>{S.?("memberAttribute")}: </label>{p.memberAttribute} <br/>
	   	<label>{S.?("authenticationType")}: </label>{p.authType} <br/>
	   	<label>{S.?("initialContextFactory")}: </label>{p.initialContextFactory} <br/>
	   	<label>{S.?("testLookup")}: </label>{p.testLookup} <br/>
	   	<label>{S.?("retryIntervall")}: </label>{p.retryIntervall} <br/>
	   	<label>{S.?("maximalRetries")}: </label>{p.maxRetries} <br/>
	   	<label>{S.?("bindPattern")}: </label>{p.bindPattern} <br/>
	</div>
  </li> 
 }
 
 def addStandardProvider(otherProvider: NodeSeq) = {
  val action = SHtml.ajaxCall(JsRaw("$(this).attr('provider')"), selectProvider _)._2
  val standard = <span class="providerHeader"><b>{S.?("standardProvider")}</b></span>  % new UnprefixedAttribute("provider", "-1", Null) % ("onclick" -> action)	 
	 
  MyUtil.flattenNodeSeq(<li class="listItem">{standard}</li> :: otherProvider.toList)
 }
 
 def allProvider (xhtml: NodeSeq): NodeSeq = {
  bind("provider", xhtml, "add" 	-> ajaxButton(S.?("add"), addProvider _) % ("class" -> "standardButton"),
		                  "remove" 	-> ajaxButton(S.?("remove"), rp _) % ("class" -> "standardButton"),
		                  "edit"    -> ajaxButton(S.?("edit"), editProvider _) % ("class" -> "standardButton"),
		                  "nodes"   -> addStandardProvider(Provider.findAll().map(showProvider)))
 }
 
 /**
  * specification tree and related stuff
  *  
  */
 
 def createSpecTree(): NodeSeq = {
	 
  def selectSpec(id : String) : JsCmd = {
   val sp = Specification.findAll(By(Specification.id, id.toLong)).apply(0)
   SelectedSpecForRole(sp)
   JsRaw("$('.emphasizableSpec').removeClass('emphasized'); $(\".emphasizableSpec[specId='" + id + "']\").addClass('emphasized');")
  }
  
  def selectSpecRole(token : String) : JsCmd = {
   val id = token.split("_").toList(1)
   val ur = UserRole.findAll(By(UserRole.id, id.toLong)).apply(0)
   SelectedSpecRole(ur)
   JsRaw("$('.emphasizableSpRole').removeClass('emphasized'); $(\".emphasizableSpRole[urId='" + token + "']\").addClass('emphasized');")
  }
  
  def specTree(spec: Specification) = {
   val roles = spec.findRoles
   val selectSp = SHtml.ajaxCall(JsRaw("$(this).attr('specId')"), selectSpec _)._2
   val selectSpRole = SHtml.ajaxCall(JsRaw("$(this).attr('urId')"), selectSpecRole _)._2
   val specElm = <span class="emphasizableSpec">{spec.name}</span> % ("onclick" -> selectSp) % new UnprefixedAttribute("specId", spec.id.toString, Null)
   
   def rElm(r: UserRole, sp: Specification) = {
	val leaf = <span class="emphasizableSpRole">{r.roleName}</span> % ("onclick" -> selectSpRole) % new UnprefixedAttribute("urId", spec.id.toString + "_" + r.id.toString, Null)
	
	<li class="treeItem"><span class="leaf">__</span>{leaf}</li> 
   }
   
   if(roles.isEmpty) <li class="treeItem"><span class="leaf">__</span>{specElm}</li>
   else {
	   <li class="treeItem">
	   		<span class="handle closed">__</span>{specElm} 
	   		<ul>{roles.map(r => rElm(r, spec)).toSeq}</ul>
   	   </li>
   }
  }
  
  def scenarioTree(scenario: Scenario) = {
   val specs = Specification.findAll(By(Specification.fkScenario, scenario.id), ByList(Specification.implementationType, List("scorecard", "rest")), OrderBy(Specification.name, Ascending))
	  
	if(specs.isEmpty) <li class="treeItem"><span class="leaf">__</span><span>{scenario.name}</span></li>
	else <li class="treeItem"><span class="handle closed">__</span><span>{scenario.name}</span><ul>{specs.map(specTree).toSeq}</ul></li>

  }
  
  Scenario.findAll(OrderBy(Scenario.name, Ascending)).map(scenarioTree).toSeq
 }
 
 def showRoles(): NodeSeq = {
	 
  def selectRoleDn(id : String) : JsCmd = {
   val rdn = RoleToGroup.findAll(By(RoleToGroup.id, id.toLong)).apply(0)
   SelectedRoleDn(rdn)
   JsRaw("$('.emphasizableRTG').removeClass('emphasized'); $(\".emphasizableRTG[rtgId='" + id + "']\").addClass('emphasized');")
  }
  
  def selectRoleForDn(id : String) : JsCmd = {
   val ur = UserRole.findAll(By(UserRole.id, id.toLong)).apply(0)
   SelectedRoleForDn(ur)
   JsRaw("$('.emphasizableRDN').removeClass('emphasized'); $(\".emphasizableRDN[urId='" + id + "']\").addClass('emphasized');")
  }
  
  def roleTree(role: UserRole) = {
	val dns = RoleToGroup.findAll(By(RoleToGroup.fkRole, role.id))
	val selectRdn = SHtml.ajaxCall(JsRaw("$(this).attr('urId')"), selectRoleForDn _)._2
	val selectDn = SHtml.ajaxCall(JsRaw("$(this).attr('rtgId')"), selectRoleDn _)._2
	val rElm = <span class="emphasizableRDN">{role.roleName}</span> % ("onclick" -> selectRdn) % new UnprefixedAttribute("urId", role.id.toString, Null)
	
	def gElm(rtg: RoleToGroup) = {
	 val rtgSpan = <span class="emphasizableRTG">{rtg.dn}</span> % ("onclick" -> selectDn) % new UnprefixedAttribute("rtgId", rtg.id.toString, Null)
	 <li class="treeItem"><span class="leaf">__</span>{rtgSpan}</li> 
	}
	
	if(dns.isEmpty) <li class="treeItem"><span class="leaf">__</span>{rElm}</li>
	else {
		<li class="treeItem">
	   		<span class="handle closed">__</span>{rElm}
	   		<ul>{dns.map(gElm).toSeq}</ul>
   	   </li>
	}
  }
  UserRole.findAll(OrderBy(UserRole.roleName, Ascending)).map(roleTree).toSeq	 
 }
 
 def addLinkToGroup(): JsCmd = {
 
  if(SelectedDn.is != null && SelectedRoleForDn.is != null && SelectedProvider.is != null) {
	  
	  SelectedProvider.is match {
	 	  
	 	  case(Some(p)) => {
	 		  val check = RoleToGroup.findAll(By(RoleToGroup.fkProvider, p.id), 
	 		  						  		  By(RoleToGroup.fkRole, SelectedRoleForDn.is.id), By(RoleToGroup.dn, SelectedDn.is))
	 		  						  
	 		  if(check.isEmpty) {
	 			  val r2g = RoleToGroup.create
	 			  r2g.fkProvider(p).fkRole(SelectedRoleForDn.is).dn(SelectedDn.is).save
	 			  SetHtml("roleList", showRoles())
	 		  }
	 		  else Noop
	 	  }
	 	   
	 	  case _  => {
	 	 	   val check = RoleToGroup.findAll(By(RoleToGroup.fkRole, SelectedRoleForDn.is.id), By(RoleToGroup.dn, SelectedDn.is))
	 		  						  
	 		  if(check.isEmpty) {
	 		 	  val scenarios = Scenario.findAll(By(Scenario.name, SelectedDn.is))
	 		 	  val scenario = if(scenarios.isEmpty) null else scenarios(0)
	 			  val r2g = RoleToGroup.create
	 			  r2g.fkRole(SelectedRoleForDn.is).dn(SelectedDn.is).fkScenario(scenario).save
	 			  SetHtml("roleList", showRoles())
	 		  }
	 		  else Noop  
	 	  }
	 }
  }
  else Alert(S.?("noChoice"))
 }
 
 def removeGroupLink(): JsCmd = {
	
  if(SelectedRoleDn.is != null) {
	  val rdn = SelectedRoleDn.is 
	  rdn.delete_!
	  SelectedRoleDn(null)
	  SetHtml("roleList", showRoles())
  }
  else Alert(S.?("noChoice")) 
 }
 
 def addLinkToRole(): JsCmd = {
  if(SelectedSpecForRole.is != null && SelectedRoleForDn.is != null) {
	  val check = SpecificationToRole.findAll(By(SpecificationToRole.fkSpecification, SelectedSpecForRole.is.id), 
	 		  						  By(SpecificationToRole.fkRole, SelectedRoleForDn.is.id))
	 		  						  
	 if(check.isEmpty) {
		 val s2r = SpecificationToRole.create
		 s2r.fkSpecification(SelectedSpecForRole.is).fkRole(SelectedRoleForDn.is).dateCreated(new Date).save
		 SetHtml("userSpecTree", createSpecTree())
	 }
	 else Noop
  }
  else Alert(S.?("noChoice"))
 }
 
 def removeRoleLink(): JsCmd = {
  if(SelectedSpecRole.is != null  && SelectedSpecForRole.is != null) {
	  SpecificationToRole.findAll(By(SpecificationToRole.fkSpecification, SelectedSpecForRole.is.id), By(SpecificationToRole.fkRole, SelectedSpecRole.is.id)).map(link => link.delete_!)
	  SelectedSpecRole(null)
	  SetHtml("userSpecTree", createSpecTree())
  }
  else Alert(S.?("noChoice"))
 }
 
 def specAndRoles (xhtml: NodeSeq): NodeSeq = {
  bind("spec", xhtml, "addLink" 			-> ajaxButton(S.?("linkRole"), addLinkToRole _) % ("class" -> "standardButton"),
		              "removeLink" 			-> ajaxButton(S.?("removeLink"), removeRoleLink _) % ("class" -> "standardButton"),
		              "specTree"    		-> createSpecTree(),
		              "roles" 				-> showRoles(),
		              "addGroupToRole" 		-> ajaxButton(S.?("linkGroup"), addLinkToGroup _) % ("class" -> "standardButton"),
		              "removeGroupFromRole" -> ajaxButton(S.?("removeLink"), removeGroupLink _) % ("class" -> "standardButton"))
 }
 
 /**
  * next all methods for client administration
  */
 
 def addClient(): JsCmd = {
  val newClient = Client.create
  newClient.shortName("").longName("")
  SelectedClient(newClient)
  RedirectTo("clientEdit")
 }
 
 def editClient(): JsCmd = {
  if(SelectedClient.is != null) RedirectTo("clientEdit")
  else Alert(S.?("noClientSelection"))
 }
 
 def removeClient(): JsCmd = {
  if(SelectedClient.is != null) {
	  val clientToRemove = SelectedClient.is
	  
	  if(ClientToUser.findAll(By(ClientToUser.fkClient, clientToRemove.id)).isEmpty && Scenario.findAll(By(Scenario.fkClient, clientToRemove.id)).isEmpty) {
	 	  SelectedClient(null)
	 	  clientToRemove.delete_!
	 	  RedirectTo("clientAdministration")
	  }
	  else Alert(S.?("clientDependencyExists"))
  }
  else Alert(S.?("noClientSelection"))
 }
 
 def clientMapping (xhtml: NodeSeq): NodeSeq = {
	 
  def selectClient(id : String) : JsCmd = {
   val c = Client.findAll(By(Client.id, id.toLong)).apply(0)
   SelectedClient(c)
   val clientUser = c.users.all.map(userClientItem).toSeq
   val cmd1 = SetHtml("userListOfClient", clientUser)
   val cmd2 = JsRaw("$('.clientList').removeClass('emphasized');$(\".clientList[clientId=" + id + "]\").addClass('emphasized');")
   CmdPair(cmd1, cmd2)
  }
  
  def selectUserForClient(id : String) : JsCmd = {
   val u = User.findAll(By(User.id, id.toLong)).apply(0)
   SelectedUserForClient(u)
   JsRaw("$('.userForClientList').removeClass('emphasized');$(\".userForClientList[userId=" + id + "]\").addClass('emphasized');")
  }
  
  def selectUserInClient(id : String) : JsCmd = {
   val u = User.findAll(By(User.id, id.toLong)).apply(0)
   SelectedUserInClient(u)
   JsRaw("$('.userInClientList').removeClass('emphasized');$(\".userInClientList[userClientId=" + id + "]\").addClass('emphasized');")
  }
  
  def clientItem(cl: Client) = {
   val action = SHtml.ajaxCall(JsRaw("$(this).attr('clientId')"), selectClient _)._2	   
   <li class="listItem clientList">{cl.shortName}</li>  % ("onclick" -> action) % new UnprefixedAttribute("clientId", cl.id.toString, Null)
  }
  
  def userItem(u: User) = {
   val action = SHtml.ajaxCall(JsRaw("$(this).attr('userId')"), selectUserForClient _)._2
   <li class="listItem userForClientList">{u.firstName + " " + u.lastName}</li> % ("onclick" -> action) % new UnprefixedAttribute("userId", u.id.toString, Null)
  }
  
  def userClientItem(u: User) = {
   var ownershipFlag = ""
   val linkList = ClientToUser.findAll(By(ClientToUser.fkClient, SelectedClient.is.id), By(ClientToUser.fkUser,u.id))
   
   if(!linkList.isEmpty && linkList(0).canBeOwner == 1) ownershipFlag = S.?("canCreateScenario")
   val action = SHtml.ajaxCall(JsRaw("$(this).attr('userClientId')"), selectUserInClient _)._2
   <li class="listItem userInClientList">{u.firstName + " " + u.lastName + " " + ownershipFlag}</li> % ("onclick" -> action) % new UnprefixedAttribute("userClientId", u.id.toString, Null)
  }
  
  def searchUser(pattern: String) = {
   val result = User.findAll(Like(User.lastName, "%" + pattern + "%"), OrderBy(User.lastName, Ascending)).map(userItem).toSeq
   CmdPair(JsRaw("$('#userListForClientSelection').empty();"), SetHtml("userListForClientSelection", result))
  }
  
  def removeLink(): JsCmd = {
	  
   val cl = SelectedClient.is 
   val userInClient = SelectedUserInClient.is
   
   if(cl != null && userInClient != null) {
	 val result = ClientToUser.findAll(By(ClientToUser.fkUser, userInClient.id ), By(ClientToUser.fkClient, cl.id))
	 if(!result.isEmpty) result.apply(0).delete_!
	 
	 SetHtml("userListOfClient", userOfClient(cl).map(userClientItem).toSeq)
	 
	}
	else Alert(S.?("nothingChosen"))
	  
  }
  
  def userOfClient(cl: Client): List[User] = {
	  val links = ClientToUser.findAll(By(ClientToUser.fkClient, cl.id))
	  links.map(l => User.findAll(By(User.id, l.fkUser)).apply(0)).toList
  }
  
  def link(canBeOwner: Boolean): JsCmd = {
   def owner(ownerFlag:Boolean) = if(ownerFlag) 1 else 0;
   
   val cl = SelectedClient.is 
   val u = SelectedUserForClient.is
   
   if(cl != null && u != null) {
	 	  if(ClientToUser.findAll(By(ClientToUser.fkUser, u.id ), By(ClientToUser.fkClient, cl.id)).isEmpty) {
	 	 	  val newLink = ClientToUser.create
	 	 	  newLink.fkUser(u).fkClient(cl).canBeOwner(owner(canBeOwner)).dateCreated(new Date).save
	 	 	  SetHtml("userListOfClient",userOfClient(cl).map(userClientItem).toSeq)
	 	  }
	 	  else Alert(S.?("linkAlreadyExists"))
	}
	else Alert(S.?("nothingChosen"))
  }
  
  def linkOwner = link(true)
  def linkWithoutOwnership = link(false)
	 
  bind("mapping", xhtml, "addClient" -> ajaxButton(S.?("addClient"), addClient _) % ("class" -> "standardButton"),
		                 "editClient" -> ajaxButton(S.?("editClient"), editClient _) % ("class" -> "standardButton"),
		                 "removeClient" -> ajaxButton(S.?("removeClient"), removeClient _) % ("class" -> "standardButton"),
		                 "link" -> ajaxButton(S.?("link"), linkWithoutOwnership _) % ("class" -> "standardButton"),
		                 "linkWithOwnership" -> ajaxButton(S.?("linkWithOwnership"), linkOwner _) % ("class" -> "standardButton"),
		                 "removeLink" -> ajaxButton(S.?("removeLink"), removeLink _) % ("class" -> "standardButton"),
		                 "clientList" -> Client.findAll().map(clientItem).toSeq,
		                 "filter" -> ajaxText("", x => searchUser(x)),
		                 "allUser" -> User.findAll(OrderBy(User.lastName, Ascending)).map(userItem).toSeq) 
 }
}