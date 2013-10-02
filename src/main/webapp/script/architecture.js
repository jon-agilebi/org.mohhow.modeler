var paper;
var xOffset = 200;
var yOffset = 80;
var width = 1060;
var height = 650;
var padding = 20;
var detailCanvasWidth = 275;
var stepDown = (detailCanvasWidth - 2 * padding)/3 + padding;
var items = [];
var sourceSystems = [];
var sourcesNotUsed = [];
var keywords;
var usageTypes;
var sourceSystems;

var selectedTitle;
var selectedScheme;
var selectedDescription;
var selectedUsage;
var selectedIntersection;
var schemeDisplay;

var xCreate;
var yCreate;
var xMove;
var yMove;
var itemToBeEdited;
var countCreation = 0;
var intersectionToBeEdited;
var focusedSchemeDetail;

var drawnSources = [];

Raphael.fn.architectureItem = function(itemType, title, description, usage, counter, color, id, modelId, frame, symbol, text, scheme) {
	
	return {
		itemType: itemType,
		itemTitle: title,
		itemDescription: description,
		itemUsage: usage,
		itemCounter: counter,
		itemColor: color,
		id: id,
		modelId: modelId,
		frame: frame,
		symbol: symbol,
		text: text,
		scheme: scheme
	};
}

function setKeywords(kwds, usg, systems) {
	keywords = kwds;
	usageTypes = usg;
	sourceSystems = systems;
}

function decodeColor(color) {
	if(!color) return "#fff";
	else if(color.startsWith('#')) return color; 
	else return "rgb(" + color + ")";
}

function sortItems(first, second) {
	
	if(first.itemType == "intersection" && second.itemType != "intersection") return 1;
	else if(first.itemType != "intersection" && second.itemType == "intersection") return -1;
	else {
		if(first.itemType == "tier" && second.itemType == "domain") return -1;
		else if(first.itemType == "domain" && second.itemType == "tier") return 1;
		else {
			if(first.itemCounter < second.itemCounter) return -1;
			else if(first.itemCounter == second.itemCounter) return 0;
			else return 1;	
		}
	}
}

function createArchitecture(xml) {
	
	paper = Raphael(xOffset, yOffset, width, height);
	
	var canvas = paper.rect(0, 0, width - detailCanvasWidth - 2 * padding, height);
	canvas.attr("fill", "#fff");
	canvas.attr("stroke", "#fff");
	canvas.dblclick(showCreationDialogue);
	
	var detailCanvas = paper.rect(width  - detailCanvasWidth - padding, 0, detailCanvasWidth, (height - padding)/2);
	detailCanvas.attr("fill", "#ffd");
	detailCanvas.attr("stroke", "#000");
	
	selectedTitle = paper.text(width  - detailCanvasWidth/2 - padding, padding, "");
	selectedScheme = paper.text(width  - detailCanvasWidth/2 - padding, 2 * padding, "");
	selectedUsage = paper.text(width  - detailCanvasWidth/2 - padding, 3 * padding, "");
	selectedDescription = paper.text(width  - detailCanvasWidth/2 - padding, 4 * padding, "");
	selectedIntersection = paper.text(width  - detailCanvasWidth/2 - padding, 8 * padding, "");
	
	var sourceCanvas = paper.rect(width  - detailCanvasWidth - padding, (height - padding)/2 + padding, detailCanvasWidth, (height - padding)/2);
	sourceCanvas.attr("fill", "#ffd");
	sourceCanvas.attr("stroke", "#000");
	
	// create model meta data
	
	$('#architecture_metadata').html(xml);
	
	$('#architecture_metadata').find("item").each(function(){
		var itemType = $(this).find("type").text();
		var itemName = $(this).find("name").text();
		var itemDetail = $(this).find("detail").text();
		var itemDescription = $(this).find("description").text();
		var counter = $(this).find("counter").text();
		var color = $(this).find("color").text();
		var id = $(this).find("id").text();
		var scheme = $(this).find("scheme").text();
		items.push(paper.architectureItem(itemType, itemName, itemDescription, itemDetail, counter, color, id, 0, scheme));
	});
	
	// find the source systems which are not in use yet
	
	for(var i = 0; i < sourceSystems.length; i++) {
		var notUsed = true;
		
		for(var j = 0; j < items.length; j++) {
			if(items[j].itemType == "source" && items[j].itemTitle == sourceSystems[i]) notUsed = false;
			break;
		}
		
		if(notUsed) sourcesNotUsed.push(sourceSystems[i]);
	}
	
	var sourceXOffset = width  - detailCanvasWidth + padding;
	var sourceYOffset =  (height - padding)/2 + 2* padding;
	
	// draw the source systems which are not in use yet
	
	for(var i = 0; i < sourcesNotUsed.length; i++) {
		 var drawnSource = drawSource(sourceXOffset, sourceYOffset + i * stepDown, sourcesNotUsed[i]);
		 drawnSources.push(drawnSource);
	}
		
	drawElements();
}

function drawElements() {
	
	items.sort(sortItems);
	
	var countTier = 0;
	var countDomains = 0;
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].itemType == "tier") countTier++; 
		if(items[i].itemType == "domain") countDomains++; 
	}
	
	var tierHeight = (height - (countTier + 2) * padding)/(countTier + 1);
	var domainWidth = (width - detailCanvasWidth -  2 * padding - (countDomains + 2) * padding)/(countDomains + 1);
	
	var indexTier = 0;
	var indexDomains = 0;
	
	for(var i = 0; i < items.length; i++) {
		
		var newItem;
		var newItemText;
		
		if(items[i].itemType == "tier") {
			newItem = paper.rect(0, (indexTier + 2) * padding + (indexTier + 1) * tierHeight, width - detailCanvasWidth - 2 * padding, tierHeight, padding);
			newItemText = paper.text(padding, (indexTier + 2) * padding + (indexTier + 1.5) * tierHeight, items[i].itemTitle);
			moveText(newItemText, true, true);
			newItem.drag(moveTier, startMoving, stopMovingTier);
			indexTier++;
		}
		else if(items[i].itemType == "domain") {
			newItem = paper.rect((indexDomains + 2) * padding + (indexDomains + 1) * domainWidth, 0, domainWidth, height, padding);
			newItemText = paper.text((indexDomains + 2) * padding + (indexDomains + 1.5) * domainWidth, padding, items[i].itemTitle);
			newItem.drag(moveDomain, startMoving, stopMovingDomain);
			indexDomains++;
		}
		
		if(items[i].itemType == "tier" || items[i].itemType == "domain") {
			newItem.attr("fill", decodeColor(items[i].itemColor));
			newItem.attr("stroke", "#000");	
			newItem.hover(focus, leaveFocus);
			newItemText.dblclick(edit);
			items[i].frame = newItem;
			items[i].text = newItemText;
		}
	}	
	
	drawSources();
}

function sortSources(first, second) {
	if(first.itemDetail < second.itemDetail) return -1;
	else if (first.itemDetail == second.itemDetail) return 0;
	else return 1;
}

function drawSources() {
	var allSources = items.filter(function(item) {return item.itemType == "source"});
	
	if(allSources.length > 0) {
		var givenCoordinate = allSources[0].itemUsage;
		var coordinates = findIntersectionCoordinates(givenCoordinate);
		var indexOfSameCoordinate = 0;
		
		for(var i = 0; i < allSources.length; i++) {
			if(allSources[i].itemUsage != givenCoordinate) {
				givenCoordinate = allSources[i].itemUsage;
				coordinates = findIntersectionCoordinates(givenCoordinate);
				indexOfSameCoordinate = 0;
			}
			
			var drawnSource = drawSource(coordinates[0] + padding, coordinates[1] + padding + indexOfSameCoordinate * stepDown, allSources[i].itemTitle);
			drawnSources.push(drawnSource);
		}
	}
}

function findIntersectionCoordinates(ids) {
	var list = ids.split("_");
	var x,y;
	var counter = 0;
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].id == list[0] || items[i].modelId == list[0]) {
			counter++;
			y = items[i].frame.attr("y");
			if(counter == 2) break;
		}
		
		if(items[i].id == list[1] || items[i].modelId == list[1]) {
			counter++;
			x = items[i].frame.attr("x");
			if(counter == 2) break;
		}
	}
	
	if(counter == 2) return [x,y]; 
	else return [x,0]; 
}

function createDialogueAsText(generation, title, description, scheme, usage, color) {
	
	var radioGeneration = "<input type='radio' name='tierOrDomain' value='tier' /><span>" + keywords.tier + "</span>" +
		"<input type='radio' name='tierOrDomain' value='domain' /><span>" + keywords.domain + "</span>";
	
	var deleteButton = "<input type='button' value='" + keywords.deleteIt + "' class='standardButton' style='float:right' onclick='deleteItem();' />";
	
	var choiceTierOrDomain = "";
	if(generation) choiceTierOrDomain = "<tr><td></td><td>" + radioGeneration + "</td></tr>";
	else choiceTierOrDomain = "<tr><td></td><td>" + deleteButton + "</td></tr>";
	
	var labelTitle = "<label style='float:left'>" + keywords.title + "</label>";
	var labelDescription = "<label style='float:left'>" + keywords.description + "</label>";
	var labelScheme = "<label style='float:left'>" + keywords.scheme + "</label>";
	var labelUsage = "<label style='float:left'>" + keywords.usage + "</label>";
	var labelColor = "<label style='float:left'>" + keywords.color + "</label>";
	
	var inputTitle = "<input type='text' id= 'archItemTitleInput'/>";
	if(title) inputTitle = "<input type='text' id= 'archItemTitleInput' value='" + title + "'/>";
	
	var inputScheme = "<input type='text' id= 'archItemSchemeInput'/>";
	if(scheme) inputScheme = "<input type='text' id= 'archItemSchemeInput' value='" + scheme + "'/>";
	
	var inputColor = "<input type='text' id= 'archItemColorInput' class='colorChoice'/>";
	if(color) inputColor = "<input type='text' id= 'archItemColorInput' class='colorChoice' value='" + color + "'/>";
	
	if(description) inputDescription = "<textarea cols='40' rows='5' id='archItemDescriptionInput'>" + description +"</textarea><br />"
	else inputDescription = "<textarea cols='40' rows='5' id='archItemDescriptionInput'></textarea><br />"
		
	var usageChoice = "<select id= 'archItemUsageChoice'><option selected=''></option>";
	if(usage) usageChoice = "<select id= 'archItemUsageChoice' value='" + usage + "'><option></option>";
		
	for(var i = 0; i < usageTypes.length; i++) {
		if(usageTypes[i] == usage) usageChoice = usageChoice + "<option selected=''>" + usageTypes[i] + "</option>";
		else usageChoice = usageChoice + "<option>" + usageTypes[i] + "</option>";
	}
		
	usageChoice = usageChoice + "</select>";
	
	var cancel = "<input type='button' value='" + keywords.cancel + "' class='standardButton' style='float: right' onclick='$.unblockUI();' />";
	var save = "<input type='button' value='" + keywords.save + "' class='standardButton' style='float: right' onclick='editItem()' />";
	if(generation) save = "<input type='button' value='" + keywords.save + "' class='standardButton' style='float: right' onclick='createItem()' />";
	
	var dialogue = "<div class='blockDialogue'><table>" + choiceTierOrDomain + "<tr><td>" + labelTitle + "</td><td>" + inputTitle + "</td></tr>"
		+ "<tr><td>" + labelColor + "</td><td>" + inputColor + "</td></tr>"
    	+ "<tr><td>" + labelDescription + "</td><td></td></tr><tr><td colspan='2'>" + inputDescription +"</td></tr>" + 
    	"<tr><td>" + labelScheme + "</td><td>" + inputScheme +"</td><td></td></tr><tr><td>" + labelUsage + "</td><td>" + usageChoice +
    	"</td></tr></table><br />"+ cancel + save + "</div>";
	
	return dialogue;
}

var edit = function() {

	itemToBeEdited = this;
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].text == this) {
			itemToBeEdited = items[i].frame;
			$.blockUI({message:  createDialogueAsText(false, items[i].itemTitle, items[i].itemDescription, items[i].itemScheme, items[i].itemUsage, items[i].itemColor)});
			break;
		}
	}
};

var showCreationDialogue = function(event) {
	xCreate = event.pageX - xOffset;
	yCreate = event.pageY - yOffset;
	$.blockUI({message:  createDialogueAsText(true, null, null, null, null)});
};

function editItem() {
	var editId = 0;
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].frame == itemToBeEdited) {
			items[i].itemTitle = $('#archItemTitleInput').val();
			items[i].itemScheme = $('#archItemSchemeInput').val();
			items[i].itemUsage = $('#archItemUsageChoice').val();
			items[i].itemDescription = $('#archItemDescriptionInput').val();
			items[i].itemColor = $('#archItemColorInput').val();
			items[i].scheme = $('#archItemSchemeInput').val();
			editId = -1 * items[i].modelId;
		}
	}
	
	$('#architecture_metadata items item').each(function() {
		if($(this).find("id").text() == editId) {
			$(this).find("name").empty();
			$(this).find("name").append($('#archItemTitleInput').val());
			$(this).find("description").empty();
			$(this).find("description").append($('#archItemDescriptionInput').val());
			$(this).find("detail").empty();
			$(this).find("detail").append($('#archItemDescriptionInput').val());
			$(this).find("color").empty();
			$(this).find("color").append($('#archItemColorInput').val());
			$(this).find("scheme").empty();
			$(this).find("scheme").append($('#archItemSchemeInput').val());
		}
	});
	
	// remove tier and domains
	
	removeElements();
	
	// draw tier and domains again
	
	drawElements();
	
	// give control back to the architecture page
	
	$.unblockUI();
};

function createItem() {
	
	// read the values from the dialogue
	
	var title = $('#archItemTitleInput').val();
	var tierOrDomain = $("input[name = 'tierOrDomain']:checked").val();
	var scheme = $('#archItemSchemeInput').val();
	var desc = $('#archItemDescriptionInput').val();
	var usageType = $('#archItemUsageChoice').val();
	var color = $('#archItemColorInput').val();
	
	if(!title || !tierOrDomain) alert("insufficient data");
	else {
		countCreation++;
		
		// compare with given domains and tiers
		
		var newCounter = 0;
		
		if(tierOrDomain == "tier") {
			
			for(var i = 0; i < items.length; i++) {
				if(items[i].itemType == "tier") {
					if(items[i].frame && items[i].frame.attr("y") > yCreate) break;
					newCounter++;
				}
			}
		}
		else {
			
			for(var i = 0; i < items.length; i++) {
				if(items[i].itemType == "domain") {
					if(items[i].frame && items[i].frame.attr("x") > xCreate) break;
					newCounter++;
				}
			}
		}
		
		for(var i = 0; i < items.length; i++) {
			if(items[i].itemType == tierOrDomain && items[i].itemCounter >= newCounter) {
				var givenCounter = items[i].itemCounter;
				items[i].itemCounter = givenCounter + 1;
			}
		}
		
		// create new item 
		
		items.push(paper.architectureItem(tierOrDomain, title, desc, usageType, newCounter, color, 0, countCreation, null, null, null, scheme));
		
		// remove tier and domains
		
		removeElements();
		
		// draw tier and domains again
		
		drawElements();
		
		// give control back to the architecture page
		
		$.unblockUI();
		
		$('#architecture_metadata items').append("<item><type>" + tierOrDomain + "</type><name>" + title + "</name><detail>" + usageType + "</detail><description>" + desc + "</description><counter>" + newCounter + "</counter><scheme>" +  scheme + "</scheme><color>" + color + "</color><id>-" + countCreation + "</id></item>");
	}
}

function moveText(text, horizontal, vertical) {
	var x = text.getBBox().x;
	if(horizontal) x = x + text.getBBox().width;
	var y = text.getBBox().y;
	if(vertical) y = y + text.getBBox().height;
	var att = {x: x, y: y};
	text.attr(att);
}

var moveTier = function(dx, dy) {
	var att = {y: Number(this.oy) + Number(dy)};
	this.attr(att);	
	yMove = Number(this.oy) + Number(dy);
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].frame == this) {
			var attText = {y: Number(items[i].text.oy) + Number(dy)};
			items[i].text.attr(attText);
		}
	}
};

var startMoving = function() {
	
	this.ox = this.attr("x");
	this.oy = this.attr("y");
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].frame == this) {
			items[i].text.ox = items[i].text.attr("x");
			items[i].text.oy = items[i].text.attr("y");
		}
	}
	
};

function stopMoving(tierOrDomain, xOrY, givenFrame) {
	
	var newCounter = 0;
	var givenIndex = 0;
	var givenItem = null;
	var move;
	
	if(xOrY == "x") move = xMove; else move = yMove;
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].itemType == tierOrDomain) {
			
			if(items[i].frame && items[i].frame.attr(xOrY) > move) break;
			newCounter++;
		}
	}
	
	for(var i = 0; i < items.length; i++) {

		if(items[i].itemType == tierOrDomain && items[i].frame == givenFrame) {
			givenIndex = i;
			givenItem = items[i];
			break;
		}
	}
	
	var help;
	
	if(givenItem && newCounter < givenItem.itemCounter) {
		
		for(var i = 0; i < items.length; i++) {
			if(items[i].itemType == tierOrDomain && items[i].itemCounter > newCounter && items[i].itemCounter <= givenItem.itemCounter && items[i].frame != givenFrame) {
				help = items[i].itemCounter;
				items[i].itemCounter = help + 1;
			}
		}
		
		givenItem.itemCounter = newCounter;
	}
	
	if(givenItem && newCounter > givenItem.itemCounter) {
		
		for(var i = 0; i < items.length; i++) {
			if(items[i].itemType == tierOrDomain && items[i].itemCounter > givenItem.itemCounter && items[i].itemCounter <= newCounter && items[i].frame != givenFrame) {
				help = items[i].itemCounter;
				items[i].itemCounter = help - 1;
			}
		}
		
		givenItem.itemCounter = newCounter;
	}
	
	items.sort(sortItems);
	removeElements();
	drawElements();
};

var stopMovingTier = function() {
	stopMoving("tier", "y", this);
};

var stopMovingDomain = function() {
	stopMoving("domain", "x", this);
}

var moveDomain = function(dx, dy) {
	
	var att = {x: Number(this.ox) + Number(dx)};
	this.attr(att);	
	xMove = Number(this.ox) + Number(dx);
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].frame == this) {
			var attText = {x: Number(items[i].text.ox) + Number(dx)};
			items[i].text.attr(attText);
		}
	}  
};

var focus = function(event) {
	
	var focusedTier = null;
	var focusedDomain = null;
	var xFocus = event.pageX - xOffset;
	var yFocus = event.pageY - yOffset;
	var interSectionToBeEdited = null;
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].frame == this) {
			selectedTitle.attr("text", items[i].itemTitle);
			selectedScheme.attr("text",items[i].itemScheme);
			selectedUsage.attr("text", items[i].itemUsage);
			selectedDescription.attr("text", items[i].itemDescription);
		}
		
		if(items[i].itemType == "tier" && items[i].frame.attr("y") < yFocus 
									   && items[i].frame.attr("y") + items[i].frame.attr("height") > yFocus) {
			focusedTier = items[i];
			
		} 
		
		if(items[i].itemType == "domain" && items[i].frame.attr("x") < xFocus 
				   && items[i].frame.attr("x") + items[i].frame.attr("width") > xFocus) {
			focusedDomain = items[i];
			
		} 
	}
	
	if(focusedTier && focusedDomain) {
		selectedIntersection.attr("text", keywords.tier + " " + focusedTier.itemTitle + ", " + keywords.domain + " " + focusedDomain.itemTitle);
		var scheme = keywords.scheme + ": ";
		var remainder = "--";
		focusedSchemeDetail = computeCoordinates(focusedTier, focusedDomain);
		
		for(var i = 0; i < items.length; i++) { 
			
			if(items[i].itemType == "intersection" && items[i].itemUsage == focusedSchemeDetail) {
				remainder = items[i].scheme;
				intersectionToBeEdited = items[i];
				break;
			} 
		}
		
		schemeDisplay = paper.text(focusedDomain.frame.attr("x") + focusedDomain.frame.attr("width")/2, focusedTier.frame.attr("y") + padding, scheme + remainder);	
		schemeDisplay.dblclick(editScheme);
	}  
};

var editScheme = function () {
	var cancel = "<input type='button' value='" + keywords.cancel + "' class='standardButton' style='float: right' onclick='$.unblockUI();' />";
	var save = "<input type='button' value='" + keywords.save + "' class='standardButton' style='float: right' onclick='saveIntersectionScheme()' />";
	var input = "<input type='text' id= 'schemeIntersectionInput'/>";
	if(intersectionToBeEdited) input = "<input type='text' id= 'schemeIntersectionInput' value='" + intersectionToBeEdited.scheme + "'/>";
	var dialogue = "<div class='blockDialogue'><label style='float:left'>" + keywords.scheme + "</label>" + input + "<br />"+ cancel + save + "</div>";
	$.blockUI({message:  dialogue});
};

function saveIntersectionScheme()  {
	
	var scheme = $('#schemeIntersectionInput').val();


	if(intersectionToBeEdited && intersectionToBeEdited.scheme)  {
		
		// edit existing scheme
		
		for(var i = 0; i < items.length; i++) {
			if(items[i].itemType == "intersection" && items[i].itemDetail == focusedSchemeDetail) {
				items[i].itemScheme = scheme;
				break;
			}
		}
		
		$('#architecture_metadata items item').each(function() {
			if($(this).find("type").text() == "intersection" &&  $(this).find("detail").text() == focusedSchemeDetail) {
				$(this).find("scheme").empty();
				$(this).find("scheme").append(scheme);
			}
		});
	}
	else {
		// create new scheme
	
		items.push(paper.architectureItem("intersection", "", "", focusedSchemeDetail, "", "", 0, 0, null, null, null, scheme));
		$('#architecture_metadata items').append("<item><type>intersection</type><name></name><detail>" + focusedSchemeDetail + "</detail><description></description><counter>100</counter><color></color><scheme>" + scheme + "</scheme><id></id></item>");
	}
	
	// remove tier and domains
	
	removeElements();
	
	// draw tier and domains again
	
	drawElements();
	
	$.unblockUI();
}

var leaveFocus = function() {
	
	selectedTitle.attr("text", "");
	selectedScheme.attr("text","");
	selectedUsage.attr("text", "");
	selectedDescription.attr("text", "");
	selectedIntersection.attr("text", "");
};

function removeElements() {
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].frame) items[i].frame.remove();
		if(items[i].text) items[i].text.remove();
		if(items[i].symbol) items[i].symbol.remove();
	}
};

function deleteItem() {
	
	var help = [];
	var editId = 0;
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].frame != itemToBeEdited) help.push(items[i]);
		else {
			if(items[i].frame) items[i].frame.remove();
			if(items[i].text) items[i].text.remove();
			if(items[i].symbol) items[i].symbol.remove();
			editId = -1 * items[i].modelId;
		}
	}
	
	items = help;
	
	$('#architecture_metadata items item').each(function() {
		if($(this).find("id").text() == editId) $(this).remove();
	});
	
	// remove tier and domains
	
	removeElements();
	
	// draw tier and domains again
	
	drawElements();
	
	// give control back to the architecture page
	
	$.unblockUI();	
}

function drawSource(x, y, name) {
	var w = detailCanvasWidth - 4 * padding;
	var h = w/3;
	var frame = paper.rect(x,y,w,h);
	frame.attr("fill", "#fff");
	frame.drag(moveSource, startMoveSource, stopMoveSource);
	var symb1 = paper.rect(x-8,y+12,16,8);
	var symb2 = paper.rect(x-8,y+24,16,8);
	var txt = paper.text(x + w/2, y + h/2, name);
	return {frame: frame, symbol: [symb1, symb2], text:txt};
}

var startMoveSource = function() {
	this.ox = this.attr("x");
	this.oy = this.attr("y");
};

function computeCoordinates(first, second) {
	var firstId = first.id;
	if(firstId == 0) firstId = first.modelId;
	
	if(second) {
		var secondId = second.id;
		if(secondId == 0) secondId = second.modelId;
		return firstId + "_" + secondId;
	}
	else return firstId + "_0";
}

var stopMoveSource = function() {
	
	var givenTier = null;
	var givenDomain = null;
	var givenSource = null;
	
	var myX = this.ox - xOffset;
	var myY = this.oy - yOffset;
	
	for(var i = 0; i < items.length; i++) {
		if(items[i].itemType == "tier" && items[i].frame.attr("y") < myY
				                       && items[i].frame.attr("y") + items[i].frame.attr("height") > myY) {
			
			givenTier = items[i];
		}
		
		if(items[i].itemType == "domain" && items[i].frame.attr("x") < myX
                					     && items[i].frame.attr("x") + items[i].frame.attr("width") > myX) {
				givenDomain = items[i];
		}
	}
	
	for(var i = 0; i < drawnSources.length; i++) {
		if(drawnSources[i].frame == this) {
			givenSource = drawnSources[i];
			break;
		}
	}
	
	if(givenTier && givenSource) {
		
		var coordinates = computeCoordinates(givenTier, givenDomain);
		items.push(paper.architectureItem("source", givenSource.text.attr("text"), "", coordinates, 0, "#fff", -1, 0));
		$('#architecture_metadata items').append("<item><type>source</type><name>" + givenSource.text.attr("text") + "</name><detail>" + coordinates + "</detail><description></description><counter>0</counter><color>#fff</color><id>-1</id></item>");
	}
	
	if(givenSource) {
		givenSource.frame.remove();
		givenSource.text.remove();
	}
	
	removeElements();
	drawElements();
};

var moveSource = function(dx,dy) {
	var x = Number(this.ox) + Number(dx);
	var y = Number(this.oy) + Number(dy);
	var att = {x: x , y: y};
	this.attr(att);	
	this.toFront();
	
	var w = detailCanvasWidth - 4 * padding;
	var h = w/3;
	
	for(var i = 0; i < drawnSources.length; i++) {
		if(drawnSources[i].frame == this) {
			var attText = {x: x + w/2 , y: y + h/2};
			drawnSources[i].text.attr(attText);
			drawnSources[i].text.toFront();
			drawnSources[i].symbol[0].attr({x: x-8, y: y+ 12});
			drawnSources[i].symbol[0].toFront();
			drawnSources[i].symbol[1].attr({x: x-8, y: y+ 24});
			drawnSources[i].symbol[1].toFront();
		} 
	}
};