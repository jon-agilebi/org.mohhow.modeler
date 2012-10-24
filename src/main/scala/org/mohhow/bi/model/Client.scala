package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Client extends Client with CreatedUpdated with LongKeyedMetaMapper[Client] {
   override def dbTableName = "CLIENT"
}

class Client extends LongKeyedMapper[Client] with ManyToMany with IdPK {
    def getSingleton = Client
	
 object shortName extends MappedString(this, 50) {
   override def dbColumnName = "SHORT_NAME"
 }
    
 object longName extends MappedString(this, 200) {
   override def dbColumnName = "LONG_NAME"
 }
 
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
 
 object users extends MappedManyToMany(ClientToUser, ClientToUser.fkClient, ClientToUser.fkUser, User)
}