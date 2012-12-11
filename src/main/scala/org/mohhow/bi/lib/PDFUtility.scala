package org.mohhow.bi.lib

import java.io._
import org.apache.fop.apps.FopFactory;
import org.apache.fop.apps.Fop;
import org.apache.xmlgraphics.util.MimeConstants;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.stream.StreamSource;

object PDFUtility {
	
 val fopFactory = FopFactory.newInstance();
 val factory = TransformerFactory.newInstance();
 val foUserAgent = fopFactory.newFOUserAgent();

 def transform(source: File, targetPdf: File) = {
	val xsltFile = Repository.readAsFile("configuration", 0, "metadata", "pdf_xsl", -1)
	val transformer = factory.newTransformer(new StreamSource(xsltFile));
	val out = new java.io.FileOutputStream(targetPdf);
 	val stream = new java.io.BufferedOutputStream(out);
 	val fop = fopFactory.newFop(MimeConstants.MIME_PDF, stream);
	
	try {
		val src = new StreamSource(source);
	
		// Resulting SAX events (the generated FO) must be piped through to FOP
		val res = new SAXResult(fop.getDefaultHandler());

		// Start XSLT transformation and FOP processing
		transformer.transform(src, res);
	}
	finally {
		stream.close();
	}
 }
}