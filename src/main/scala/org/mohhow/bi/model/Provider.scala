package org.mohhow.model
		
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Provider extends Provider with LongKeyedMetaMapper[Provider] {
   override def dbTableName = "PROVIDER"
}

class Provider extends LongKeyedMapper[Provider] with IdPK {
    def getSingleton = Provider
	
 object friendlyName extends MappedPoliteString(this, 100) {
   override def dbColumnName = "FRIENDLY_NAME"
 }

 object url extends MappedPoliteString(this, 500) {
   override def dbColumnName = "URL"
 }
	
 object base extends MappedPoliteString(this, 500) {
   override def dbColumnName = "BASE"
 }
	
 object userName extends MappedPoliteString(this, 100) {
   override def dbColumnName = "USER_NAME"
 }
 
 object pwd extends MappedPoliteString(this, 100) {
   override def dbColumnName = "PWD"
 }
	
 object authType extends MappedPoliteString(this, 100) {
   override def dbColumnName = "AUTH_TYPE"
 }

 object initialContextFactory extends MappedPoliteString(this, 500) {
   override def dbColumnName = "INITIAL_CONTEXT_FACTORY"
 }

 object testLookup extends MappedPoliteString(this, 500) {
   override def dbColumnName = "TEST_LOOKUP"
 }

 object retryIntervall extends MappedPoliteString(this, 10) {
   override def dbColumnName = "RETRY_INTERVALL"
 }

 object maxRetries extends MappedLong(this) {
   override def dbColumnName = "MAX_RETRIES"
 }
 
 object searchTerm extends MappedPoliteString(this, 100) {
   override def dbColumnName = "SEARCH_TERM"
 }
 
 object memberAttribute extends MappedPoliteString(this, 100) {
   override def dbColumnName = "MEMBER_ATTRIBUTE"
 }
 
 object displayAttribute extends MappedPoliteString(this, 100) {
   override def dbColumnName = "DISPLAY_ATTRIBUTE"
 }

 object dateCreated extends MappedDateTime(this) {
   override def dbColumnName = "DATE_CREATED"
 }    
}