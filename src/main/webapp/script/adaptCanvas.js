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
var keywords;
var dimensionMode = true;
var dimensions;
var allLevel;

function setKeywords(kwds, dms, lvls) {
	keywords = kwds;
	dimensions = dms;
	allLevel = lvls;
}

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

Raphael.fn.elementTitle = function(text, frame, elementType, detail, vertexId, usage, description) {
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
		vertexId: vertexId,
		usage: usage,
		description: description
	};
}

var showDialog = function () {
	
	var thisStatus = "prototype";
	var thisKind;
	var someDetail;
	var someUsage;
	var someDescription;
	
	for(var i = 0; i < elementTitles.length; i++) {
		if(elementTitles[i].element == this) {	
			thisStatus = elementTitles[i].status;
			thisKind = elementTitles[i].elementType;
			someDetail = elementTitles[i].detail;
			someUsage = elementTitles[i].usage;
			someDescription = elementTitles[i].description;
		}
	}
	
	if(thisStatus != "prototype")
	{
		textToEdit = this;
		var text = "";
			
		if(!(this.attr("text").substring(0,2) == "<<")) text = this.attr("text");
		
		var label1 = "<label style='float:left'>" + keywords.name + "</label>";
		var label2 = "<label style='float:left'>" + keywords.pattern + "</label>";
		var label3 = "<label style='float:left'>" + keywords.realises + "</label>";
		var label4 = "<label style='float:left'>" + keywords.initialSize + "</label>";
		var label5 = "<label style='float:left'>" + keywords.growthPerLoad + "</label>";
		var label6 = "<label style='float:left'>" + keywords.degeneratedDimension + "</label>";
		var label7 = "<label style='float:left'>" + keywords.description + "</label>";
		var label8 = "<label style='float:left'>" + keywords.isRoleChoice + "</label>";
		var label9 = "<label style='float:left'>" + keywords.roleName + "</label>";
			
		var nameInput1 = "<input type='text' width='100%' id= 'modalModelTextInput' value='";
		var nameInput2 = "' />";
		
		var cancel = "<input type='button' value='" + keywords.cancel + "' class='standardButton' style='float: right' onclick='$.unblockUI();' />";
		var save = "<input type='button' value='" + keywords.save + "' class='standardButton' style='float: right' onclick='saveText()' />";
		
		if(thisKind == "attribute") {
			var pattern;
			var realize;
			
			if(someDetail) pattern = "<input type='text' id= 'modalModelAttributePatternInput' value='" + someDetail + "'/>";
			else pattern = "<input type='text' id= 'modalModelAttributePatternInput' />";
			
			if(someUsage) realize = "<input type='text' id= 'modalModelAttributeUsageInput' value='" + someUsage + "'/>";
			else realize = "<input type='text' id= 'modalModelAttributeUsageInput' />";
			
			$.blockUI({message: "<div class='blockDialogue'><table><tr><td>" + label1 + "</td><td>" + nameInput1 + text + nameInput2 + "</td></tr>" +
				                "<tr><td>" + label2 + "</td><td>" + pattern + "</td></tr><tr><td>" + label3 +"</td><td>" + realize + 
				                "</td></tr></table><br />"+ cancel + save + "</div>"});	
		}
		else if(thisKind == "level" && !dimensionMode) {
			var levelChoice;
			
			if(text.length > 0) levelChoice = "<select id= 'modalModelTextInput' value='" + text + "'>";
			else levelChoice = "<select id= 'modalModelTextInput' value=''><option selected=''></option>";
			
			for(var i = 0; i < allLevel.length; i++) {
				if(allLevel[i] == text) levelChoice = levelChoice + "<option selected=''>" + allLevel[i] + "</option>";
				else levelChoice = levelChoice + "<option>" + allLevel[i] + "</option>";
			}
			
			levelChoice = levelChoice + "</select>";
			$.blockUI({message: "<div class='blockDialogue'>" + label1 + levelChoice + "<br />" + "<br />" + save + cancel + "</div>"});
		}
		else if(thisKind == "dimension" && !dimensionMode) {
			var dimensionChoice;
			
			if(someDescription) dimensionChoice = "<select id= 'modalModelTextInput' value='" + someDescription + "'>";
			else dimensionChoice = "<select id= 'modalModelTextInput' value=''><option selected=''></option>";
			
			for(var i = 0; i < dimensions.length; i++) {
				if(dimensions[i] == text) dimensionChoice = dimensionChoice + "<option selected=''>" + dimensions[i] + "</option>";
				else dimensionChoice = dimensionChoice + "<option>" + dimensions[i] + "</option>";
			}
			
			dimensionChoice = dimensionChoice + "</select>";
			
			var roleChoice;
			var roleCheckbox;
			
			if(someDetail == "role") roleCheckbox = "<input type='checkbox' style='float:left' id='checkRole' checked=''/>";
			else roleCheckbox = "<input type='checkbox' style='float:left' id='checkRole' />";
			roleChoice = "<tr><td>" + label8 + "</td><td>" + roleCheckbox + "</td></tr>";
			
			var roleName;
			
			if(someDetail == "role" && text) roleName = "<input type='text' id= 'modalModelRoleNameInput' value='" + text + "'/>";
			else if(someDetail == "role") roleName = "<input type='text' id= 'modalModelRoleNameInput' value=''/>";
			else roleName = "<input type='text' id= 'modalModelRoleNameInput' value='' disabled=''/>";
			
			$.blockUI({message: "<div class='blockDialogue'><table><tr><td>" + label1 + "</td><td>" + dimensionChoice + "</td></tr>" +
				roleChoice + "<tr><td>" + label9 + "</td><td>" + roleName + "</td></tr></table>" +
                cancel + save + "</div>"}); 
		}
		else if(thisKind == "dimension" || thisKind == "cube") {
			
			var initialGrowth;
			var growthPerLoad;
			var degenerationChoice = "";
			var description;
		
			if(someDetail && someDetail.split(";").length == 2) {
				var bothValues = someDetail.split(";")
				initialGrowth = "<input type='text' id= 'modalModelAttributePatternInput' value='" + bothValues[0] + "'/>";
				growthPerLoad = "<input type='text' id= 'modalModelAttributeAdditionalPatternInput' value='" + bothValues[1] + "'/>";
				
			}
			else {
				initialGrowth = "<input type='text' id= 'modalModelAttributePatternInput' />";
				growthPerLoad = "<input type='text' id= 'modalModelAttributeAdditionalPatternInput'/>";
			}
			
			if(thisKind == "dimension") {
				var dgn;
				
				if(someUsage == "degenerated") dgn = "<input type='checkbox' style='float:left' id='checkDegeneration' checked=''/>";
				else dgn = "<input type='checkbox' style='float:left' id='checkDegeneration' />";
				degenerationChoice = "<tr><td>" + label6 + "</td><td>" + dgn + "</td></tr>";
			}
			
			if(someDescription) description = "<textarea cols='40' rows='10' id='modalModelDescriptionInput'>" + someDescription +"</textarea><br />"
			else description = "<textarea cols='40' rows='10' id='modalModelDescriptionInput'></textarea><br />"
						
			$.blockUI({message: "<div class='blockDialogue'><table><tr><td>" + label1 + "</td><td>" + nameInput1 + text + nameInput2 + "</td></tr>" +
				                "<tr><td>" + label4 + "</td><td>" + initialGrowth + "</td></tr>" +
				                "<tr><td>" + label5 + "</td><td>" + growthPerLoad + "</td></tr>" + degenerationChoice + 
				                "<tr><td>" + label7 + "</td><td></td></tr></table><br />"
				                +  description + "<br />" + cancel + save + "</div>"});
		}
		else {
	
			$.blockUI({message: "<div class='blockDialogue'>" + label1 + nameInput1 + text + nameInput2 + "<br />" + "<br />" + save + cancel + "</div>"});
		}
	}
}

function saveText() {

 var modelId = null;
 var vertexId = 0;
 var index;
 
 for(var i = 0; i < elementTitles.length ;i++) {
	if(elementTitles[i].element  == textToEdit) {
		modelId = 	elementTitles[i].frame.id;
		vertexId = elementTitles[i].vertexId;
		index = i;
	}
 }
 
 // usage is either a statement about the realisation of an attribute or a statement whether a dimension is degenerated
 
 var usageText;
 
 if($('#checkDegeneration').is(':checked') ) usageText = "degenerated";
 else usageText = $('#modalModelAttributeUsageInput').val();
 
 // detail contains either the information on role playing or load cycle information
 
 var detailText;
 var descriptionText;
 
 if($('#checkRole').is(':checked')){
	 detailText = "role";
	 descriptionText = $('#modalModelRoleNameInput').val();
 }
 else {
	 detailText = $('#modalModelAttributePatternInput').val();
	 descriptionText = $('#modalModelDescriptionInput').val();
 }
 
 var text = $('#modalModelTextInput').val();
 var additionalDetailText = $('#modalModelAttributeAdditionalPatternInput').val();
 
 if(additionalDetailText && additionalDetailText.length > 0) detailText = detailText + ";" + additionalDetailText;
 
 if(vertexId > 0) {
	  elementTitles[index].usage = usageText;
	  elementTitles[index].text = text;
	  elementTitles[index].detail = detailText;
	  elementTitles[index].description = descriptionText;
	  
	  if($('#checkRole').is(':checked')) textToEdit.attr('text', descriptionText + " (" + text + ")");
	  else textToEdit.attr('text', text);
	  
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] elementName").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] elementName").append(text);
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] detail").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] detail").append(detailText);
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] usage").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] usage").append(usageText);
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] status").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] status").append("changed");
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] description").empty();
	  $("#logicalModelVertices v[vertexId='" + vertexId + "'] description").append(descriptionText);
	  $.unblockUI();
 }
 else if(modelId) {
	  elementTitles[index].usage = usageText;
	  elementTitles[index].text = text;
	  elementTitles[index].detail = detailText;
	  elementTitles[index].description = descriptionText;
	  
	  if($('#checkRole').is(':checked')) textToEdit.attr('text', descriptionText + " (" + text + ")");
	  else textToEdit.attr('text', text);
	  
	  $("#logicalModelVertices v[modelId='" + modelId + "'] elementName").empty();
	  $("#logicalModelVertices v[modelId='" + modelId + "'] elementName").append(text);
	  $("#logicalModelVertices v[modelId='" + modelId + "'] detail").empty();
	  $("#logicalModelVertices v[modelId='" + modelId + "'] detail").append(detailText);
	  $("#logicalModelVertices v[modelId='" + modelId + "'] usage").empty();
	  $("#logicalModelVertices v[modelId='" + modelId + "'] usage").append(usageText);
	  $("#logicalModelVertices v[modelId='" + modelId + "'] status").empty();
	  $("#logicalModelVertices v[modelId='" + modelId + "'] status").append("changed");
	  $("#logicalModelVertices v[modelId='" + modelId + "'] description").empty();
	  $("#logicalModelVertices v[modelId='" + modelId + "'] description").append(descriptionText);
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
				 var newTitle = editPaper.elementTitle(elementTitles[i].text, clone, elementTitles[i].elementType, null, "m" + this.id, elementTitles[i].usage, elementTitles[i].description);
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
	  	var usage = $(this).find("usage").text();
	  	var description = $(this).find("description").text();
	  	drawElement(elementType, onlyDisplay, Number(x), Number(y), elementName, vertexId, detail, usage, description);  	
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

function drawElement(kind, onlyDisplay, x, y, elementName, vertexId, detail, usage, description) {
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
		elementTitles.push(paper.elementTitle(elementName, frame, kind, detail, vertexId, usage, description));
	}
	else {
		
		symbols.push(editPaper.symbol(kind, frame));
		elementTitles.push(editPaper.elementTitle(elementName, frame, kind, detail, vertexId, usage, description));
		frame.drag(move, startMove, stopMove);
		
		if(frame.getVertexId() >= 0 || (frame.getVertexId().length > 0 && frame.getVertexId().substring(0,1) == "m")) {
			frame.setStatus("sync");
			frame.hover(getFocus, leaveFocus);
		} 	
	}
}

function createPalette(elementType) {
	
	if(elementType == "dimension") {
		
		dimensionMode = true;
		drawElement("hierarchy", false, 83, 40, "<<hierarchy>>", -1);
		drawElement("level", false, 83, 90, "<<level>>", -1);
		drawElement("member", false, 83, 140, "<<member>>", -1);
		drawElement("attribute", false, 83, 190, "<<attribute>>", -1);
		drawElement("scope", false, 83, 240, "<<scope>>", -1);
	}
	else {
		
		dimensionMode = false;
		drawElement("cube", false, 83, 40, "<<cube>>", -1);
		drawElement("dimension", false, 83, 130, "<<dimension>>", -1);
		drawElement("level", false, 83, 180, "<<level>>", -1);
	}  
}  