package org.mohhow.model	
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object BacklogToSpecification extends BacklogToSpecification with LongKeyedMetaMapper[BacklogToSpecification] {
   override def dbTableName = "BACKLOG_TO_SPECIFICATION"
}

class BacklogToSpecification extends LongKeyedMapper[BacklogToSpecification] with IdPK {
    def getSingleton = BacklogToSpecification
	
 object fkSpecification extends MappedLongForeignKey(this, Specification) {
   override def dbColumnName = "FK_SPECIFICATION"
 }

object fkFeature extends MappedLongForeignKey(this, Feature) {
   override def dbColumnName = "FK_FEATURE"
 }
	
 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }
     
}


