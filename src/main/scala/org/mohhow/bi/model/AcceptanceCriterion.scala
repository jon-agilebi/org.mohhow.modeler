package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object AcceptanceCriterion extends AcceptanceCriterion with LongKeyedMetaMapper[AcceptanceCriterion] {
   override def dbTableName = "ACCEPTANCE_CRITERION"
}

class AcceptanceCriterion extends LongKeyedMapper[AcceptanceCriterion] with IdPK {
    def getSingleton = AcceptanceCriterion
	
 object fkFeature extends MappedLongForeignKey(this, Feature) {
   override def dbColumnName = "FK_FEATURE"
 }

 object text extends MappedPoliteString(this, 500) {
   override def dbColumnName = "TEXT"
 }

 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }   
}