package org.mohhow.snippet

import net.liftweb._
import http._
import util._
import org.mohhow.model._
import java.util.Date
import org.mohhow.bi.util._
import java.util.regex.Pattern

object ReleaseForm extends LiftScreen {
	 
 val release = SelectedRelease.is 
 
 val datePattern = Pattern.compile("\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d");
 val kind = select("Release Kind", "", List("Development Snapshot", "Show Case", "Productivity"))
 val impact = select("Impact", "", List("Major Release", "Minor Release", "Patch"))
 val scheduledEnd = field("Scheduled End", "", valRegex(datePattern, "Date format must be dd.mm.yyyy"))
 
 def finish() {
  release.kind(kind).scheduledEnd(Utility.asDate(scheduledEnd)).status("active")	
  val releases = Release.findAll()
  
  if(releases.isEmpty) {
	  
	if (impact == "Major Release") {
		release.majorRelease(1)
		release.minorRelease(0)
		release.patch(0)
	} 
	else if (impact == "Minor Release") {
		release.majorRelease(0)
		release.minorRelease(1)
		release.patch(0)
	}
	else {
		release.majorRelease(0)
		release.minorRelease(0)
		release.patch(1)
	}	  
  }
  else {
	val oldRelease = releases.apply(0)
	if (impact == "Major Release") {
		release.majorRelease(oldRelease.majorRelease)
		release.minorRelease(0)
		release.patch(0)
	} 
	else if (impact == "Minor Release") {
		release.majorRelease(oldRelease.majorRelease)
		release.minorRelease(oldRelease.minorRelease)
		release.patch(0)
	}
	else {
		release.majorRelease(oldRelease.majorRelease)
		release.minorRelease(oldRelease.minorRelease)
		release.patch(oldRelease.patch)
	}
  }
  release.save
  S.redirectTo("/release")
 }
}
