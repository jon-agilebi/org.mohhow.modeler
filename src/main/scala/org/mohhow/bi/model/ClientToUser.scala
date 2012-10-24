package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object ClientToUser extends ClientToUser with LongKeyedMetaMapper[ClientToUser] {
   override def dbTableName = "CLIENT_TO_USER"
}

class ClientToUser extends LongKeyedMapper[ClientToUser] with IdPK {
    def getSingleton = ClientToUser
    
 object fkClient extends MappedLongForeignKey(this, Client) {
   override def dbColumnName = "FK_CLIENT"
 }
	
 object fkUser extends MappedLongForeignKey(this, User) {
   override def dbColumnName = "FK_USER"
 }
 
 object canBeOwner extends MappedLong(this) {
   override def dbColumnName = "CAN_BE_OWNER"
 }

 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }     
}