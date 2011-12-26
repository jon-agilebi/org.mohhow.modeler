package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.org.mohhow.model._
import net.liftweb.http.auth.{AuthRole,HttpBasicAuthentication,userRoles}
import net.liftweb.mapper._


import org.mohhow.bi.lib._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
	
	// database connection
	  
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	  new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // where to search snippet
    LiftRules.addToPackages("org.mohhow")
    Schemifier.schemify(true, Schemifier.infoF _, User, Scenario, Release, ProductBacklog, Feature, Meeting, Minutes, ProtocolItem, MeetingRecipient, ProtocolComment, ProtocolToBacklog, Measure, ModelVertex, ModelEdge, MeasureToModelVertex, MeasureRange, ScenarioRole, PTable, PAttribute, Specification, Block, BlockContent, BacklogToSpecification, UserGroup, UserRole, SpecificationToRole, UserToGroup, Provider, Sprint, AcceptanceCriterion)

    // Build SiteMap
    
    def sitemap() = SiteMap(
      Menu("Home") / "index" >> User.AddUserMenusAfter,
      Menu.i("Project Setup") / "setup", // >> If(User.loggedIn_? _, S ? "Please login!"),
      Menu.i("Product Vision") /"vision", // >> If(User.loggedIn_? _, S ? "Please login!"),
      Menu("Protocols") / "protocol", 
      Menu("Product Backlog") / "backlog", 
	  //Menu("Architecture") / "architecture", 
      Menu("Specification") / "specification", // >> If(User.loggedIn_? _, S ? "Please login!"), 
      Menu("Scorecard Display") / "scorecard",
      Menu("Measure Editor") / "measure" >> Hidden,
	  Menu.i("Meetings") / "meetingEdit" >> Hidden,
	  Menu("Feature Editor") / "backlogEdit" >> Hidden,
	  Menu.i("Scenario Editor") / "scenarioEdit" >> Hidden,
	  Menu.i("Sprint Editor") / "sprintEdit" >> Hidden,
	  Menu.i("Authentication Provider Editor") / "providerEdit" >> Hidden,
	  Menu("Node Editor") / "nodeEdit" >> Hidden,
	  Menu("Release Editor") / "releaseEdit" >> Hidden,
	  Menu("Scorecard Design") / "design" >> Hidden,
      Menu("Measure Catalogue") / "catalogue",
      Menu.i("Block Presentation") / "block" >> Hidden, 
      Menu.i("Logical Data Model") / "adapt", 
      Menu.i("Logical Data Model Editor") / "adaptEdit" >> Hidden,
      Menu("Physical Data Model") / "attribute", 
	  Menu("Release Management") / "release", 
	  Menu.i("Project Plan") / "plan",
	  Menu.i("Sprint Planning") / "sprint" >> Hidden,
	  Menu.i("Node Management") / "node",
	  Menu.i("Deployment") / "deployment",
	  Menu.i("User Management") / "user")

    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)
    
	/** 
	  * now the stuff for the REST services used for the communication with the iPad devices
	  */
	  
	LiftRules.statelessDispatchTable.append(BIService);
	  
	val roles = AuthRole("User", AuthRole("PlainUser"))
	  
	def protection: LiftRules.HttpAuthProtectedResourcePF = {
	  	case Req(List("blocks", _, _, _), _, GetRequest) => Full(AuthRole("PlainUser"))
	}
	  
	//LiftRules.httpAuthProtectedResource.append(protection)
	
	LiftRules.authentication = HttpBasicAuthentication("BIService") {
		case ("jon", "jon", req) => userRoles(AuthRole("PlainUser")); true
		case _ => false
	}
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
  
  /** 
   * now the stuff for the REST services used for the communication with the iPad devices
   */
  /*
  
  AuthProtectedResource.append(protection)

 LiftRules.authentication = HttpBasicAuthentication("BIService") {
	  case ("jon", "jon", req) => true
	  case _ => false
	  
 	case (userEmail, _, authenticates) => { 
	 	  //logger.debug("Authenticating: " + userEmail) 
	 	  User.find(By(User.email, userEmail)).map { user =>
	 	  			if (authenticates(user.password.is)) { 
	 	  				//logger.debug("Auth succeeded for " + userEmail) 
	 	  				User.logUserIn(user)
	 	  			
	 	  				userRoles(user.editable.map(acct => AuthRole("PlainUser")))
	 	  				true 
	 	  			} 
	 	  			else {
	 	  					//logger.warn("Auth failed for " + userEmail) 
	 	  					false
	 	  			} 
	 	  		} openOr false
	} 
 } */
}