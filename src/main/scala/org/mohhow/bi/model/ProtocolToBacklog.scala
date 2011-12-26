package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object ProtocolToBacklog extends ProtocolToBacklog with LongKeyedMetaMapper[ProtocolToBacklog] {
   override def dbTableName = "PROTOCOL_TO_BACKLOG"
}

class ProtocolToBacklog extends LongKeyedMapper[ProtocolToBacklog] with IdPK {
    def getSingleton = ProtocolToBacklog
	
 object fkProtocolItem extends MappedLongForeignKey(this, ProtocolItem) {
   override def dbColumnName = "FK_PROTOCOL_ITEM"
 }

object fkFeature extends MappedLongForeignKey(this, Feature) {
   override def dbColumnName = "FK_FEATURE"
 }
	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
     
}


