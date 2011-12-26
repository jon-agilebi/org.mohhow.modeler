package org.mohhow.snippet

import net.liftweb._
import http._
import util._
import util.Helpers._
import common._
import org.mohhow.model._
import java.util.Date
import java.util.regex.Pattern

object ProviderForm extends LiftScreen {
	
 def nvl(s: String) = if (s == null) "" else s
 
 val friendlyName = field("Friendly Name", nvl(SelectedProvider.is.friendlyName.toString), valMinLen(1, "Friendly name too short"), valMaxLen(100, "Friendly name too long"))
 val url = field("URL", nvl(SelectedProvider.is.url.toString), valMinLen(1, "URL too short"), valMaxLen(500, "URL too long"))
 val base = field("Base", nvl(SelectedProvider.is.base.toString), valMinLen(1, "Base too short"), valMaxLen(500, "Base too long"))
 val userName = field("User Name", nvl(SelectedProvider.is.userName.toString), valMinLen(1, "User Name too short"), valMaxLen(100, "User Name too long"))
 val authType = field("Authentication Type", nvl(SelectedProvider.is.authType.toString), valMinLen(1, "Authentication type too short"), valMaxLen(100, "Authentication Type too long"))
 val initialContextFactory = field("Initial Context Factory", nvl(SelectedProvider.is.initialContextFactory.toString), valMinLen(1, "Initial Context Factory"), valMaxLen(500, "Initial Context Factory"))
 val testLookup = field("Test Lookup", nvl(SelectedProvider.is.testLookup.toString), valMinLen(1, "Test Lookup"), valMaxLen(500, "Test Lookup"))
 val retryIntervall = field("Retry Intervall", nvl(SelectedProvider.is.retryIntervall.toString), valMinLen(1, "Retry Intervall too short"), valMaxLen(10, "Retry Intervall too long"))
 val maxRetries = field("Maximal Retries", nvl(SelectedProvider.is.maxRetries.toString))
 
 def finish() {
	 asLong(maxRetries.is) match {
		 case Full(mr) => {
			 val provider = SelectedProvider.is
			 provider.friendlyName(friendlyName).url(url).base(base).userName(userName).authType(authType).initialContextFactory(initialContextFactory).testLookup(testLookup).retryIntervall(retryIntervall).maxRetries(mr).save
	
			 SelectedProvider(provider)
			 S.redirectTo("/user") 
		 }
		 
		 case _ => S.error("prio not correct");
	 } 
  }
}