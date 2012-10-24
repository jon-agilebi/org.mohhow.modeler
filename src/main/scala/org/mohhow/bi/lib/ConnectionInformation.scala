package org.mohhow.bi.lib

import net.liftweb.mapper.ConnectionIdentifier

class ConnectionInformation(name: String) extends ConnectionIdentifier {
	
	def jndiName = name

}