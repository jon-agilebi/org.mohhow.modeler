var paper;
var editPaper;
var symbols = [];
var elementTitles = [];
var fourMarker = [];
var frames = [];
var connections = [];
var textToEdit;
var lineToDraw;
var source;
var focus;
var elementToRemove;
var removeIcon = [];
var someoneHasFocus = false;
var hasSource = false;
var superfluousClones = [];
var justALineToRemove = false;

function setSymbolAttributes(symbol) {
	symbol.attr("fill", "lightgray");
	symbol.attr("stroke", "black");
}

function setFrameAttributes(frame) {
	frame.attr("fill", "#fff");
	frame.attr("stroke", "#000");
}

function updateElements(x, y, kind, elements) {
	var attributes;

	if(kind == "hierarchy") {
		attributes = [{cx: 16 + x, cy: 18 + y}, {cx: 8 + x, cy: 32 + y}, {cx: 24 + x, cy: 32 + y}, {path: movePath("M16,18L8,32M16,18L24,32",x,y)}];
	} 
	else if (kind == "cube") {
		attributes = [{path: movePath("M6,11L14,6L22,11L14,16L6,11L6,23M14,16L14,28M22,11L22,23M6,23L14,28L22,23",x,y)}];
	} 
	else if (kind == "attribute") {
		attributes = [{path:movePath("M7,20L14,13L21,20L14,27Z",x,y)}];
	}
	else if (kind == "dimension") {
		attributes = [{x: 6 + x, y: 9 + y}, {x: 18 + x, y: y+4}, {x: 23 + x, y: 9 + y}, {x: 4+x, y: 23 + y}, {path: movePath("M6,9L18,5.5M18,9L23,9M6,21L6,23",x,y)}];
	}	
	else if (kind == "level") {
		attributes = [{path: movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",x,y)}, {path: movePath("M12,16L8,24L16,24Z",x,y)}, {path: movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",x,y)}];
	}
	else if (kind == "member") {
		attributes = [{path: movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",x,y)}, {cx: 12 + x, cy: 20 + y}, {path: movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",x,y)}];
	}
	else if (kind == "scope") {
		attributes = [{path: movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",x,y)}, {path: movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",x,y)}];
	}
	else if (kind == "context") {
		attributes = [{path: movePath("M6,11L14,6L22,11L14,16L6,11L6,23M14,16L14,28M22,11L22,23M6,23L14,28L22,23",x,y)}, {path:movePath("M6,6A11,11,0,1,1,20,20",x,y)}];
	}
	
	for(var i = 0; i < elements.length; i++) {
		elements[i].attr(attributes[i]);
	} 
}

function computeElements(x, y, kind, myPaper) {
	
	if(kind == "hierarchy") {
		var p1 = myPaper.path(movePath("M16,18L8,32M16,18L24,32",x,y));
		setSymbolAttributes(p1);
		var c1 = myPaper.circle(16+x, 18+y, 4);
		setSymbolAttributes(c1);
		var c2 = myPaper.circle(8+x, 32+y, 4);
		setSymbolAttributes(c2);
		var c3 = myPaper.circle(24+x, 32+y, 4);
		setSymbolAttributes(c3);
		
		return [c1, c2, c3, p1];
	}
	else if (kind == "cube") {
		var p = myPaper.path(movePath("M6,11L14,6L22,11L14,16L6,11L6,23M14,16L14,28M22,11L22,23M6,23L14,28L22,23",x,y));
		setSymbolAttributes(p);
	    return [p];
	}
	else if (kind == "attribute") {
		var p = myPaper.path(movePath("M7,20L14,13L21,20L14,27Z",x,y));
		setSymbolAttributes(p);
	    return [p];
	}
	else if (kind == "dimension") {
		var r1 = myPaper.rect(6+x,9+y,12,12);
		setSymbolAttributes(r1);
		var r2 = myPaper.rect(18+x,4+y,3,3);
		setSymbolAttributes(r2);
		var r3 = myPaper.rect(23+x,9+y,3,3);
		setSymbolAttributes(r3);
		var r4 = myPaper.rect(4+x,23+y,3,3);
		setSymbolAttributes(r4);
		var p = myPaper.path(movePath("M6,9L18,5.5M18,9L23,9M6,21L6,23",x,y));
		setSymbolAttributes(p);
		
	    return [r1, r2, r3, r4, p];
	}	
	else if (kind == "level") {
		
		var lbracket = myPaper.path(movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",x,y));
		lbracket.attr("stroke", "black");
		lbracket.attr("stroke-width", 1.5);
		var triangle = myPaper.path(movePath("M12,16L8,24L16,24Z",x,y));
		setSymbolAttributes(triangle);
		var rbracket = myPaper.path(movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",x,y));
		rbracket.attr("stroke", "black");
		rbracket.attr("stroke-width", 1.5);
	    return [lbracket, triangle, rbracket];
	}
	else if (kind == "member") {
		
		var lbracket = myPaper.path(movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",x,y));
		lbracket.attr("stroke", "black");
		lbracket.attr("stroke-width", 1.5);
		var circle = myPaper.circle(12+x, 20+y, 4);
		setSymbolAttributes(circle);
		var rbracket = myPaper.path(movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",x,y));
		rbracket.attr("stroke", "black");
		rbracket.attr("stroke-width", 1.5);
	    return [lbracket, circle, rbracket];
	}
	else if (kind == "scope") {
		
		var lbracket = myPaper.path(movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",x,y));
		lbracket.attr("stroke", "black");
		lbracket.attr("stroke-width", 1.5);
		var rbracket = myPaper.path(movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",x,y));
		rbracket.attr("stroke", "black");
		rbracket.attr("stroke-width", 1.5);
	    return [lbracket, rbracket];
	}
	else if (kind == "context") {
		var p = myPaper.path(movePath("M6,11L14,6L22,11L14,16L6,11L6,23M14,16L14,28M22,11L22,23M6,23L14,28L22,23",x,y));
		setSymbolAttributes(p);
		var c = myPaper.path(movePath("M6,6A11,11,0,1,1,20,20",x,y));
		c.attr("stroke-dasharry", "-");
	    return [p, c];
	}
	else return [];
}

Raphael.el.getStatus = function() {
	
	for(var i = 0; i < elementTitles.length; i++) {
		if(elementTitles[i].frame == this) {	
			return elementTitles[i].status;
		}
	}
	
	return "unknown";
}

Raphael.el.getElementType = function() {
	
	for(var i = 0; i < elementTitles.length; i++) {
		if(elementTitles[i].frame == this) {	
			return elementTitles[i].elementType;
		}
	}
	
	return "unknown";
}

Raphael.el.getVertexId = function() {
	
	for(var i = 0; i < elementTitles.length; i++) {
		 if(elementTitles[i].frame == this) {	
			return elementTitles[i].vertexId;
		 }
	 }
		
	 return null;
}

Raphael.el.setStatus = function(status) {
	
	for(var i = 0; i < elementTitles.length; i++) {
		if(elementTitles[i].frame == this) {	
		 elementTitles[i].status = status;
		}
	}
}

Raphael.el.setVertexId = function(vertexId) {
	
	for(var i = 0; i < elementTitles.length; i++) {
		if(elementTitles[i].frame == this) {	
		 elementTitles[i].vertexId = vertexId;
		}
	}
}

Raphael.el.isInBox = function(x, y) {
	var box = this.getBBox();
	return box.x < x && x < box.x + box.width && box.y < y && y < box.y + box.height;
}

Raphael.fn.symbol  = function(kind, frame) {
	
	var x = frame.getBBox().x;
	var y = frame.getBBox().y;
	var elements = computeElements(Number(x), Number(y), kind, this);
	
	return {
		kind: kind,
		elements: elements,
		frame: frame
	};
}

Raphael.fn.connectionMarker  = function(x, y, frame) {
	var att = {"fill": "#00d", "fill-opacity": .4};
	var marker = this.rect(x, y, 4, 4);
	marker.attr(att);
	return {
		marker: marker,
		frame: frame
	};
}

function getModelConnection(source, target) {
	
 var bb1 = source.getBBox(),
     bb2 = target.getBBox(),
     p = [{x: bb1.x + bb1.width / 2, y: bb1.y - 1},
     {x: bb1.x + bb1.width / 2, y: bb1.y + bb1.height + 1},
     {x: bb1.x - 1, y: bb1.y + bb1.height / 2},
     {x: bb1.x + bb1.width + 1, y: bb1.y + bb1.height / 2},
     {x: bb2.x + bb2.width / 2, y: bb2.y - 1},
     {x: bb2.x + bb2.width / 2, y: bb2.y + bb2.height + 1},
     {x: bb2.x - 1, y: bb2.y + bb2.height / 2},
     {x: bb2.x + bb2.width + 1, y: bb2.y + bb2.height / 2}],
     d = {}, dis = [];

 for (var i = 0; i < 4; i++) {
    for (var j = 4; j < 8; j++) {
        var dx = Math.abs(p[i].x - p[j].x),
            dy = Math.abs(p[i].y - p[j].y);
        if ((i == j - 4) || (((i != 3 && j != 6) || p[i].x < p[j].x) && ((i != 2 && j != 7) || p[i].x > p[j].x) && ((i != 0 && j != 5) || p[i].y > p[j].y) && ((i != 1 && j != 4) || p[i].y < p[j].y))) {
            dis.push(dx + dy);
            d[dis[dis.length - 1]] = [i, j];
        }
    }
 }
 if (dis.length == 0) {
    var res = [0, 4];
 } else {
    res = d[Math.min.apply(Math, dis)];
 }
 var x1 = p[res[0]].x,
     y1 = p[res[0]].y,
     x4 = p[res[1]].x,
     y4 = p[res[1]].y;
 dx = Math.max(Math.abs(x1 - x4) / 2, 10);
 dy = Math.max(Math.abs(y1 - y4) / 2, 10);
 var x2 = [x1, x1, x1 - dx, x1 + dx][res[0]].toFixed(3),
     y2 = [y1 - dy, y1 + dy, y1, y1][res[0]].toFixed(3),
     x3 = [0, 0, 0, 0, x4, x4, x4 - dx, x4 + dx][res[1]].toFixed(3),
     y3 = [0, 0, 0, 0, y1 + dy, y1 - dy, y4, y4][res[1]].toFixed(3);

 return path = ["M", x1.toFixed(3), y1.toFixed(3), "C", x2, y2, x3, y3, x4.toFixed(3), y4.toFixed(3)].join(",");	
}

Raphael.fn.connection = function (source, target) {
	var path = getModelConnection(source, target);
	
	return {
        line: this.path(path).attr({stroke: "#000", fill: "none"}).hover(markLine, unmarkLine),
        from: source,
        to: target
    };
};

function updateModelConnection(connection) {
	var path = getModelConnection(connection.from, connection.to);
	connection.line.attr({path: path});
}

function moveSymbol(symbol) {
 var x = symbol.frame.getBBox().x;
 var y = symbol.frame.getBBox().y;
 updateElements(Number(x), Number(y), symbol.kind, symbol.elements);
}

Raphael.fn.elementTitle = function(text, frame, elementType, detail, vertexId) {
	var x = Number(frame.getBBox().x);
	var y = Number(frame.getBBox().y);
	
	var elm = this.text(x + 60, y + 20, text);
	elm.dblclick(showDialog);
	
	return {
		element: elm,
		frame: frame,
		status: "prototype",
		text: text,
		elementType: elementType,
		detail: detail,
		vertexId: vertexId
	};
}

var showDialog = function () {
	
	var thisStatus = "prototype";
	var thisKind;
	var someDetail;
	var someUsage;
	
	for(var i = 0; i < elementTitles.length; i++) {
		if(elementTitles[i].element == this) {	
			thisStatus = elementTitles[i].status;
			thisKind = elementTitles[i].elementType;
			someDetail = elementTitles[i].detail;
			someUsage = elementTitles[i].usage;
		}
	 }
	
	if(thisStatus != "prototype")
	{
		textToEdit = this;
		var text = "";
		
		
		if(!(this.attr("text").substring(0,2) == "<<")) text = this.attr("text");
		
		var label1 = "<label>Name</label>";
		var nameInput1 = "<input type='text' id= 'modalModelTextInput' value='";
		var nameInput2 = "' />";
		
		var cancel = "<input type='button' value='Cancel' class='standardButton' style='float: right' onclick='$.unblockUI();' />";
		var save = "<input type='button' value='Save' class='standardButton' style='float: right' onclick='saveText()' />";
		
		if(thisKind == "attribute") {
			var pattern;
			var realize;
			
			if(someDetail) pattern = "<input type='text' id= 'modalModelAttributePatternInput' value='" + someDetail + "'/>";
			else pattern = "<input type='text' id= 'modalModelAttributePatternInput' />";
			
			if(someUsage) realize = "<input type='text' id= 'modalModelAttributeUsageInput' value='" + someUsage + "'/>";
			else realize = "<input type='text' id= 'modalModelAttributeUsageInput' />";
			
			$.blockUI({message: "<div class='blockDialogue'><table><tr><td><label>Name</label></td><td>" + 
				                 nameInput1 + text + nameInput2 + "</td></tr><tr><td><label>Pattern</label></td><td>" 
				                 + pattern + "</td></tr><tr><td><label>Realises</label></td><td>" + realize + "</td></tr></table><br />"+ cancel + save + "</div>"});
			
		}
		else if(thisKind == "dimension" || thisKind == "cube") {
			
			var initialGrowth;
			var growthPerLoad;
		
			if(someDetail && someDetail.split(";").length == 2) {
				var bothValues = someDetail.split(";")
				initialGrowth = "<input type='text' id= 'modalModelAttributePatternInput' value='" + bothValues[0] + "'/>";
				growthPerLoad = "<input type='text' id= 'modalModelAttributeAdditionalPatternInput' value='" + bothValues[1] + "'/>";
			}
			else {
				initialGrowth = "<input type='text' id= 'modalModelAttributePatternInput' />";
				growthPerLoad = "<input type='text' id= 'modalModelAttributeAdditionalPatternInput'/>";
			}
			
			$.blockUI({message: "<div class='blockDialogue'><table><tr><td><label>Name</label></td><td>" + 
				                 nameInput1 + text + nameInput2 + "</td></tr><tr><td><label>Estimated initial size</label></td><td>" 
				                 + initialGrowth + "</td></tr><tr><td><label>Estimated growth per load</label></td><td>" 
				                 + growthPerLoad + "</td></tr></table><br />"+ cancel + save + "</div>"});
		}
		else {
	
			$.blockUI({message: "<div class='blockDialogue'>" + label1 + nameInput1 + text + nameInput2 + "<br />" + cancel + save + "</div>"});
		}
	}
}

function saveText() {

 var modelId = null;
 var vertexId = 0;
 
 for(var i = 0; i < elementTitles.length ;i++) {
	if(elementTitles[i].element  == textToEdit) {
		modelId = 	elementTitles[i].frame.id;
		vertexId = elementTitles[i].vertexId;
	}
 }
	
 var text = $('#modalModelTextInput').val();
 var detailText = $('#modalModelAttributePatternInput').val();
 var additionalDetailText = $('#modalModelAttributeAdditionalPatternInput').val();
 var usageText = $('#modalModelAttributeUsageInput').val();
 
 if(additionalDetailText && additionalDetailText.length > 0) detailText = detailText + ";" + additionalDetailText;
 
 if(vertexId > 0) {
	  textToEdit.attr('text',text);
	  textToEdit.attr('detail', detailText);
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] elementName").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] elementName").append(text);
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] detail").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] detail").append(detailText);
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] usage").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] usage").append(usageText);
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] status").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] status").append("changed");
	  $.unblockUI();
 }
 else if(modelId) {
	  textToEdit.attr('text',text);
	  textToEdit.attr('detail', detailText);
	  $("#logicalModelVertices v[modelId='" + modelId + "'] elementName").empty();
	  $("#logicalModelVertices v[modelId='" + modelId + "'] elementName").append(text);
	  $("#logicalModelVertices v[modelId='" + modelId + "'] detail").empty();
	  $("#logicalModelVertices v[modelId='" + modelId + "'] detail").append(detailText);
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] usage").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] usage").append(usageText);
	  $("#logicalModelVertices v[modelId='" + modelId + "'] status").empty();
	  $("#logicalModelVertices v[modelId='" + modelId + "'] status").append("changed");
	  $.unblockUI();
 }
}

function moveTitle(title) {
	var x = title.frame.getBBox().x;
	var y = title.frame.getBBox().y;
	var att = {x: Number(x) + 60, y: Number(y) + 20};
	title.element.attr(att);
}

window.onload = function () {
	paper = Raphael(460, 80, 800, 650);
	createCanvas();
}

function movePath(path, dx, dy) {

	var text = String(path);
	var parts = text.trim().split(/(\D)/);
	var k = 0;
	var leastLetter;
	
	for(var j = 0; j < parts.length; j++) {
		if(parts[j] == ',') {  
			k++;
		}
		else if(parts[j].match(/\d+/)) {
			
			var help = Number(parts[j]);
			
			if(leastLetter != 'A') {
				if (k == 0) parts[j] = help + Number(dx); else parts[j] = help + Number(dy);
			}
			else {
				if (k == 5) parts[j] = help + Number(dx); 
				else if (k == 6) parts[j] = help + Number(dy);
			}
		}
		else if(parts[j].match(/[MTLA]/)) {
			leastLetter = parts[j];
			k = 0;
		}
	}
	
	return parts.join('');
}

function createCanvas() {
	
	var canvas = paper.rect(0, 0, 800, 650);
	canvas.attr("fill", "#ffa");
	canvas.attr("stroke", "#000");	
}

function createEditCanvas() {
	
	paper.clear();
	editPaper = Raphael(180, 60, 1096, 684);
	var bigCanvas = editPaper.rect(0,0, 1096, 684);
	bigCanvas.attr("fill", "#ffa");
	bigCanvas.attr("stroke", "#000");
	
	var leftCanvas = editPaper.rect(5, 5, 274, 674);
	leftCanvas.attr("fill", "#ffd");
	leftCanvas.attr("stroke", "#000");
}

var startMove = function() {
	
	if(this.getStatus() == "prototype") {
		
		var clone = this.clone();
		clone.drag(move, startMove, stopMove);
		
		for(var i = 0; i < symbols.length; i++) {
			if(symbols[i].frame == this) {
			 symbols.push(editPaper.symbol(symbols[i].kind, clone));	
			}
		}
		
		for(var i = 0; i < elementTitles.length; i++) {
			 if(elementTitles[i].frame == this) {	
				 var newTitle = editPaper.elementTitle(elementTitles[i].text, clone, elementTitles[i].elementType, null, "m" + this.id);
				 elementTitles.push(newTitle);
			}
		}
	}
	
	if(this.type == "path") {
		this.opath = this.attr("path");
	}
	else {
		this.ox = this.attr("x");
		this.oy = this.attr("y");
	} 

}

var move = function(dx, dy) {
	
	removeConnectionMarker();

	if(this.type == "path") {
		var att = {path: movePath(this.opath, dx, dy)};
		this.attr(att);
	}
	else { 
		var att = {x: Number(this.ox) + Number(dx), y: Number(this.oy) + Number(dy)};
		this.attr(att);	
	}
	
	for(var k = 0; k < connections.length; k++) {
		updateModelConnection(connections[k]);
	}
	
	for(var j = 0; j < elementTitles.length; j++) {
		
		if(elementTitles[j].frame == this) {
			
			moveTitle(elementTitles[j]);
		}	
	}

	for(var i = 0; i < symbols.length; i++) {
		if(symbols[i].frame == this) {
			moveSymbol(symbols[i]);
		}	
	}
	
}

function determineModelOrVertexId(elm, isVertex) {
	
	var modelId = -1;
	var vertexId = -1;
	
	for(var i = 0; i < elementTitles.length ;i++) {
		if(elementTitles[i].frame  == elm) {
			vertexId = elementTitles[i].vertexId;
			modelId = elementTitles[i].modelId;
			break;
		}
	}
	
	if(isVertex) return vertexId; else return modelId;
}

var stopMove = function() {
	
	var x = this.getBBox().x;
	var y = this.getBBox().y;
	var vertexId = -1;
	var modelId = -1;
	
	if (x < 280) {
		this.hide();
	} 
	else {
		
		for(var i = 0; i < elementTitles.length ;i++) {
			if(elementTitles[i].frame  == this) {
				vertexId = elementTitles[i].vertexId;
				modelId = elementTitles[i].modelId;
				break;
			}
		}
		
		if(this.getStatus() == "prototype") {
			
			this.setStatus("initial");
			this.setVertexId("m" + this.id);
			this.hover(getFocus, leaveFocus);
			frames.push(this);
			$('#logicalModelVertices').append("<v modelId='" + this.id + "' vertexId='m"+ this.id + "'><elementType>" + this.getElementType() + "</elementType><elementName></elementName><x>" + x + "</x><y>" + y + "</y><detail></detail><scale>1</scale><usage>0</usage><status>new</status></v>");
		}
		else if (modelId > 0) {
			
			$("#logicalModelVertices v[modelId='" + this.id + "'] x").empty();
			$("#logicalModelVertices v[modelId='" + this.id + "'] x").append(x);
			$("#logicalModelVertices v[modelId='" + this.id + "'] y").empty();
			$("#logicalModelVertices v[modelId='" + this.id + "'] y").append(y);
			$("#logicalModelVertices v[modelId='" + this.id + "'] status").empty();
			$("#logicalModelVertices v[modelId='" + this.id + "'] status").append("changed");
		}
		else {
			
			$("#logicalModelVertices v[vertexId='" + vertexId + "'] x").empty();
			$("#logicalModelVertices v[vertexId='" + vertexId + "'] x").append(x);
			$("#logicalModelVertices v[vertexId='" + vertexId + "'] y").empty();
			$("#logicalModelVertices v[vertexId='" + vertexId + "'] y").append(y);
			$("#logicalModelVertices v[vertexId='" + vertexId + "'] status").empty();
			$("#logicalModelVertices v[vertexId='" + vertexId + "'] status").append("changed");
		}
	}  
}

var getFocus = function() {
	if(!hasSource) {
		this.animate({"fill": "#f23", "fill-opacity": .2},500);
		removeConnectionMarker();
		createConnectionMarker(this);
		someoneHasFocus = true;
		focus = this;
		justALineToRemove = false;
	}
};

var drawLine = function(dx, dy) {
	var att = {x: this.ox + dx, y: this.oy + dy};
	this.attr(att);
	
	var lx = this.ox + 2;
	var ly = this.oy + 2;
	var lx2 = this.ox + dx;
	var ly2 = this.oy + dy;
	lineToDraw.attr("path", "M" + lx + "," + ly + "L" + lx2 + "," + ly2);
};

var finishLineDrawing = function() {
	
	lineToDraw.remove();
	
	var x = this.getBBox().x + 2;
	var y = this.getBBox().y + 2;
	
	if(hasSource) {
		
		for(var i = 0; i < frames.length; i++) {
			
			if(frames[i].isInBox(x, y)) {
                var conn = editPaper.connection(source, frames[i]);
				connections.push(conn);
				$('#logicalModelEdges').append("<e modelId='" + conn.line.id + "' edgeId='m" + conn.line.id + "'><h>" + source.getVertexId() + "</h><t>" + frames[i].getVertexId() + "</t><status>new</status></e>");
			}
		}
	}
	
	hasSource = false;
	
};

var initConnection = function() {
	
	var clone = this.clone();
	clone.drag(drawLine, initConnection, finishLineDrawing);
	superfluousClones.push(clone);
	this.ox = this.attr("x");
	this.oy = this.attr("y");
	this.toFront();
	
	var lx = this.ox + 2;
	var ly = this.oy + 2;
	lineToDraw  = editPaper.path("M" + lx + "," + ly + "L" + lx + "," + ly);
	
	for(var i = 0; i < fourMarker.length; i++) {
		if(fourMarker[i].marker == this) {
			source = fourMarker[i].frame;
			hasSource = true;
		}
	}
}

function createConnectionMarker(frame) {
	var x = frame.getBBox().x;
	var y = frame.getBBox().y;
	var width = frame.getBBox().width;
	var height = frame.getBBox().height;
	
	var marker1 = editPaper.connectionMarker(x - 2, y + height/2 -2, frame);
	fourMarker.push(marker1);
	var marker2 = editPaper.connectionMarker(x + width/2 - 2, y + height -2, frame);
	fourMarker.push(marker2);
	var marker3 = editPaper.connectionMarker(x + width/2, y -2, frame);
	fourMarker.push(marker3);
	var marker4 = editPaper.connectionMarker(x + width - 2, y + height/2 -2, frame);
	fourMarker.push(marker4);
	
	for(var i = 0; i < fourMarker.length; i++) {
		fourMarker[i].marker.drag(drawLine, initConnection, finishLineDrawing);
	}
	
	createRemoveIcon(x + width - 8, y + 8, frame);
}

function removeConnectionMarker() {
	
	while(fourMarker.length > 0) {
		var aMarker = fourMarker.pop();
		aMarker.marker.remove();
	}
	
	while(removeIcon.length > 0) {
		var aPart = removeIcon.pop();
		aPart.remove();
	}
	
	while(superfluousClones.length > 0) {
		var aClone = superfluousClones.pop();
		aClone.remove();
	}
}

var leaveFocus = function() {
	
	this.animate({"fill": "#fff", "fill-opacity": 1},500);
	someoneHasFocus = false;
};

var removeElement = function () {
	
	var cancel = "<input type='button' value='Cancel' class='standardButton' onclick='$.unblockUI();' />";
	var ok = "<input type='button' value='OK' class='standardButton' onclick='removeFinally()' />";

	$.blockUI({message: "<div>Really wanna destroy?<br />" + cancel + ok + "</div>"});
	
}

function removeFinally() {
	
	removeConnectionMarker();
	if(!justALineToRemove) {
		
		var modelId = determineModelOrVertexId(elementToRemove, false);
		var vertexId = determineModelOrVertexId(elementToRemove, true);
		
		if(modelId > 0) {
			$("#logicalModelVertices v[modelId='" + modelId + "'] status").empty();
			$("#logicalModelVertices v[modelId='" + modelId + "'] status").append("removed");
		}
		else {
			$("#logicalModelVertices v[vertexId='" + vertexId + "'] status").empty();
			$("#logicalModelVertices v[vertexId='" + vertexId + "'] status").append("removed");
		}
		
		for(var i = 0; i < connections.length; i++) {
			if(connections[i].from == elementToRemove || connections[i].to == elementToRemove) {
				
				$("#logicalModelEdges e[modelId='" + connections[i].line.id + "'] status").empty();
				$("#logicalModelEdges e[modelId='" + connections[i].line.id + "'] status").append("removed");
				connections[i].line.remove();
			}
		}
		
		for(var i = 0; i < elementTitles.length; i++) {
			 if(elementTitles[i].frame == elementToRemove) {	
				 elementTitles[i].element.remove();
			 }
		}
		
		for(var i = 0; i < symbols.length; i++) {
			
			if(symbols[i].frame == elementToRemove) {
				
				for(var j = 0; j < symbols[i].elements.length; j++) {
					symbols[i].elements[j].remove();
				}
			} 
		}
	}
	
	elementToRemove.remove();
	
	$.unblockUI();
}

function createRemoveIcon(x, y, element) {
	
	var circle = editPaper.circle(x, y, 8);
	circle.attr("fill", "#f00");
	circle.attr("stroke", "#fff"); 
	circle.attr("stroke-width", 2);
	removeIcon.push(circle);
	var x1 = x - 6, y1 = y -6, x2 = x + 6, y2 = y + 6;  
	var lines = editPaper.path("M" + x1 + "," + y1 + "L" + x2 + "," + y2 + "M" + x1 + "," + y2 + "L" + x2 + "," + y1);
	lines.attr("stroke-width", 2);
	lines.attr("stroke", "#fff");
	removeIcon.push(lines);
	
	circle.dblclick(removeElement);
	lines.dblclick(removeElement);
	
	elementToRemove = element;
	
}

var markLine = function() {
	removeConnectionMarker();
	var p = this.getPointAtLength(1);
	createRemoveIcon(p.x,p.y, this);
	justALineToRemove = true;
}

var unmarkLine = function() {
	
}

/**
 * draws the diagram for one cube or dimension out of the XML information on the page
 * @param onlyDisplay
 */

function drawModel(onlyDisplay) {
	
	elementTitles = [];
	frames = [];
	symbols = [];
	
	if(onlyDisplay) {
		paper.clear(); 
		createCanvas(); 
	}
	else {
		createEditCanvas();
	}
	
	$('#logicalModelVertices v').each(function(){
		
		var vertexId =$(this).attr("vertexId");
		var elementType = $(this).find("elementType").text(); 
		var elementName = $(this).find("elementName").text();  
		var detail = $(this).find("detail").text();
	  	var x =  $(this).find("x").text();
	  	var y =  $(this).find("y").text();
	  	drawElement(elementType, onlyDisplay, Number(x), Number(y), elementName, vertexId, detail);  	
	});
	
	$('#logicalModelEdges e').each(function(edge){
		var headId = $(this).find("h").text();
		var tailId = $(this).find("t").text();
		var head = getVertexById(headId);
		var tail = getVertexById(tailId);
		if(head && tail) {
			if(onlyDisplay) paper.connection(head, tail); else connections.push(editPaper.connection(head, tail));
		}
	}); 
}  

function getVertexById(vertexId) {
	for(var i = 0; i < elementTitles.length; i++) {
		 if(elementTitles[i].vertexId == vertexId) {	
			 return elementTitles[i].frame;
		 }
	}
}

function drawFrame(onlyDisplay, isPath, x, y, width, height, r, path) {
	
	if(onlyDisplay) {
		
		if(isPath) {
			return paper.path(path);
		}
		else {
			return paper.rect(x, y, width, height, r);
		}
		
	}
	else {
		
		if(isPath) {
			return editPaper.path(path);
		}
		else {
			return editPaper.rect(x, y, width, height, r);
		}
	}
}

function drawElement(kind, onlyDisplay, x, y, elementName, vertexId) {
	drawElement(kind, onlyDisplay, x, y, elementName, vertexId, null);
}

function drawElement(kind, onlyDisplay, x, y, elementName, vertexId, detail) {
	var frame;
	var newX, newY;
	
	if(onlyDisplay) {
		newX = x - 280;
		newY = y - 20;
	}
	else {
		newX = x;
		newY = y;
	}
	
	if(kind == "hierarchy") frame = drawFrame(onlyDisplay, true, 0, 0, 0, 0, 0, movePath("M0,12L24,0L96,0L120,12L120,40L0,40Z", newX, newY));
	if(kind == "level" || kind == "member" || kind == "scope"  || kind == "dimension") frame = drawFrame(onlyDisplay, false, newX, newY, 120, 40, 10, "");
	if(kind == "attribute") frame = drawFrame(onlyDisplay, true, 0, 0, 0, 0, 0, movePath("M12,0L120,0L120,40L12,40L0,30L0,10Z", newX, newY));
	if(kind == "cube"  || kind == "context") frame = drawFrame(onlyDisplay, true, 0, 0, 0, 0, 0, movePath("M0,0L120,0L120,80L0,80ZM0,30L120,30", newX, newY));
	
	setFrameAttributes(frame);
	frames.push(frame);
	
	if(onlyDisplay) {		
		symbols.push(paper.symbol(kind, frame));
		elementTitles.push(paper.elementTitle(elementName, frame, kind, detail, vertexId));
	}
	else {
		
		symbols.push(editPaper.symbol(kind, frame));
		elementTitles.push(editPaper.elementTitle(elementName, frame, kind, detail, vertexId));
		frame.drag(move, startMove, stopMove);
		
		if(frame.getVertexId() >= 0 || (frame.getVertexId().length > 0 && frame.getVertexId().substring(0,1) == "m")) {
			frame.setStatus("sync");
			frame.hover(getFocus, leaveFocus);
		} 	
	}
}

function createPalette(elementType) {
	
	if(elementType == "dimension") {
		
		drawElement("hierarchy", false, 83, 40, "<<hierarchy>>", -1);
		drawElement("level", false, 83, 90, "<<level>>", -1);
		drawElement("member", false, 83, 140, "<<member>>", -1);
		drawElement("attribute", false, 83, 190, "<<attribute>>", -1);
		drawElement("scope", false, 83, 240, "<<scope>>", -1);
	}
	else {
		
		drawElement("cube", false, 83, 40, "<<cube>>", -1);
		drawElement("dimension", false, 83, 130, "<<dimension>>", -1);
		drawElement("level", false, 83, 180, "<<level>>", -1);
	}  
}  