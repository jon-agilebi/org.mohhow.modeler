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
import scala.collection.mutable._
import org.mohhow.bi.lib._
import org.mohhow.bi.util.{Utility => MyUtil}
import scala.xml._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */

class Boot {
	
  def boot {
	
	// database connection for the repository of the Agile BI Modeler
	  
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	  new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    
    // further database connections to BI systems
    
    def txt(node: Node, tag: String) = MyUtil.getSeqHeadText(node \\ tag)
    
    BIService.storeMap = Map.empty[String, (ConnectionInformation, String, String, String)];
    
    val allStores = Repository.read("deployment", 0, "generic", "stores", 0) \\ "store"
    val dbs = Repository.read("configuration", 0, "metadata", "conf_db", -1) \\ "database"
  
    for(aStore <- allStores) {
        println("and the driver is " + MyUtil.dbInfo(txt(aStore, "driver"), dbs, "driver"))
    	val vendor = new StandardDBVendor(MyUtil.dbInfo(txt(aStore, "driver"), dbs, "driver"), 
                                     txt(aStore, "connectionString"), 
	 		                         Full(txt(aStore, "user")), 
	 		                         Full(txt(aStore, "password")))
	  
    	val connInf = new ConnectionInformation(txt(aStore, "alias"))
    	val toDatePattern = MyUtil.dbInfo(txt(aStore, "driver"), dbs, "to_date")
    	val sysdatePattern = MyUtil.dbInfo(txt(aStore, "driver"), dbs, "sysdate")
    	val loadPattern = MyUtil.dbInfo(txt(aStore, "driver"), dbs, "load")
   
    	DB.defineConnectionManager(connInf, vendor)
    	LiftRules.unloadHooks.append(() => vendor.closeAllConnections_!())
    	
    	BIService.storeMap += (txt(aStore, "alias") -> (connInf, toDatePattern, sysdatePattern, loadPattern))
    }
    
    // where to search snippet
    LiftRules.addToPackages("org.mohhow")
    Schemifier.schemify(true, Schemifier.infoF _, User, Client, ClientToUser, Scenario, ProductBacklog, Feature, Release, Meeting, Minutes, ProtocolItem, 
    		                     				  MeetingRecipient, ProtocolComment, ProtocolToBacklog, Measure, ModelVertex, ModelEdge, MeasureToModelVertex, 
    		                     				  MeasureRange, ScenarioRole, PTable, PAttribute, Specification, Block, UserRole, SpecificationToRole, RoleToGroup, 
    		                     				  Provider, Sprint, BacklogToSpecification)

    // Build SiteMap
    
    def sitemap() = SiteMap(
      Menu(S.?("indexPage")) / "index" >> User.AddUserMenusAfter,
      Menu(S.?("projectInitPage")) / "projectInit" submenus (
    	Menu(S.?("setupPage")) / "setup", 
    	Menu(S.?("planPage")) / "plan",
        Menu(S.?("visionPage")) / "vision" 
      ),
      Menu(S.?("analysisPage")) / "analysis" submenus (
    	Menu(S.?("minutesPage")) / "protocol", 
        Menu(S.?("backlogPage")) / "backlog"
      ),
      Menu(S.?("projectDesignPage")) / "projectDesign" submenus (
    	Menu(S.?("cataloguePage")) / "catalogue",
    	Menu(S.?("logicPage")) / "adapt",
    	Menu(S.?("specificationPage")) / "specification"  
      ),
    Menu(S.?("implementationPage")) / "implementation" submenus (
     Menu(S.?("scorecardPage")) / "scorecard",
     Menu(S.?("physicalPage")) / "attribute"
    ),
    Menu(S.?("administrationPage")) / "administration" submenus (
     Menu(S.?("releasePage")) / "release",
     Menu(S.?("nodePage")) / "node",
	 Menu(S.?("deploymentPage")) / "deployment",
	 Menu(S.?("userPage")) / "user",
	 Menu(S.?("clientAdministration")) / "clientAdministration"
    ),
    Menu(S.?("aboutPage")) / "about",
    Menu(S.?("measureEditor")) / "measure" >> Hidden,
	Menu(S.?("meetingEditor")) / "meetingEdit" >> Hidden,
	Menu(S.?("backlogEditor")) / "backlogEdit" >> Hidden,
	Menu(S.?("scenarioEditor")) / "scenarioEdit" >> Hidden,
	Menu(S.?("sprintEditor")) / "sprintEdit" >> Hidden,
	Menu(S.?("providerEditor")) / "providerEdit" >> Hidden,
	Menu(S.?("nodeEditor")) / "nodeEdit" >> Hidden,
	Menu(S.?("releaseEditor")) / "releaseEdit" >> Hidden,
	Menu(S.?("clientEditor")) / "clientEdit" >> Hidden,
	Menu(S.?("scorecardDesign")) / "design" >> Hidden,
    Menu(S.?("blockPresentation")) / "block" >> Hidden, 
    Menu(S.?("testDataEditor")) / "testData" >> Hidden,
	Menu(S.?("sprints")) / "sprint" >> Hidden)
	
	val withModeler = Props.get("usage") == "both" || Props.get("usage") == "modeler"
	  
    if(withModeler) LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))

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
    
    def protectRestService : LiftRules.HttpAuthProtectedResourcePF = {
    	case Req("blocks" :: _, _, GetRequest) => Full(AuthRole("biServiceClient"))
    	case Req("blocks" :: _, _, PostRequest) => Full(AuthRole("biServiceClient"))
    }
    
    LiftRules.httpAuthProtectedResource.append { protectRestService }
    
    val withService = Props.get("usage") == "both" || Props.get("usage") == "rest"
	
    if(withService) LiftRules.dispatch.append(BIService);
    
    LiftRules.dispatch.append{
    	case Req("documentation" :: releaseId :: Nil, _, _) => () => DocumentationDispatcher.sendDocumentation(releaseId)
    }
    
    if(withService) LiftRules.dispatch.append(DeploymentService)
    
    Authentification.initialize()
	
	LiftRules.authentication = HttpBasicAuthentication("RestService") {
	 case ("daisy", "duck", req) => userRoles(AuthRole("biServiceClient")); BIServiceUser("daisy"); true  
	 case(uid, pwd, req) => {
	  
	  if(Authentification.authorize(uid, pwd)) {
	 	  userRoles(AuthRole("biServiceClient"))
	 	  BIServiceUser(uid)
	 	  true
	  }
	  else {
	 	  val aUser = User.findAll(By(User.email, uid))
	     
	 	  if(!aUser.isEmpty && aUser(0).password.match_?(pwd)) {
	 		  userRoles(AuthRole("biServiceClient"))
	 		  BIServiceUser(uid)
	 		  true
	 	  }
	 	  else false  
	  }
	 }
	 case _ => {
		 false
	 }
	}
  }

  /**
   * Force the request to be UTF-8
   */
  
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}