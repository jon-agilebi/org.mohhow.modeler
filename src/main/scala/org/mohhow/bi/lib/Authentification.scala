package org.mohhow.bi.lib

import org.mohhow._
import model._
import net.liftweb._
import net.liftweb.ldap._
import http._
import SHtml._
import S._
import mapper._
import util._
import Helpers._
import scala.xml._

object Authentification {
 
 val allProvider = initialize()
 
 def createConfiguration(p: Provider): Map[String, String] = {
	 Map("ldap.url" -> p.url.toString, 
		 "ldap.base" -> p.base.toString, 
		 "ldap.userName" -> p.userName.toString, 
		 "ldap.password" -> "secret", 
		 "ldap.authType" -> p.authType.toString)
 }
 
 def createVendor(conf: Map[String, String]) = {
  val myLdap = new LDAPVendor
  myLdap.configure(conf)
  myLdap
 }

 def initialize() = Provider.findAll().map(p => createVendor(createConfiguration(p))).toList
 
 def authorize(uid: String, pwd: String) = {
  def tryVendor(uid: String, pwd: String, list: List[LDAPVendor]): Boolean = list match {
		case Nil => false
		case vendor :: vendors => if(vendor.bindUser(uid, pwd)) true else tryVendor(uid, pwd, vendors)
  }
  
  tryVendor(uid, pwd, allProvider)
 }
}