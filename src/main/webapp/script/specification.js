
function initializeBlockInformation(blockInformation) {	
	var blocks = [];
	
	$('#dropResults').html(blockInformation);
	
	$('#dropResults').find("block").each(function(){
		var blockId = $(this).attr("blockId");
		var filter = $(this).find("filter").text();
		var attrList = [];
		$(this).find("attribute").each(function(){attrList.push({name:$(this).attr('name'), order:$(this).attr('order')})});
		
		var msrList = [];
		$(this).find("measure").each(function(){msrList.push($(this).text())});
	
		var block = {blockId: blockId, measures: msrList, attributes: attrList, filter: filter};
		blocks.push(block);
	});
	
	$('#dropResults').data('blockInformation', blocks);  
}

function sort(list, item, direction) {
	for(var i = 0; i < list.length; i++) {
		if(list[i] == item && i > 0 && direction == "down") {
			var help = list[i];
			list[i] = list[i - 1];
			list[i - 1] = help;
			return list;
		}
		else if (list[i] == item && i < list.length - 1 && direction == "up") {
			var help = list[i];
			list[i] = list[i + 1];
			list[i + 1] = help;
			return list;
		}
	}
	
	return list;
}

function changeOrder(attributeList, text) {
	
	for(var i = 0; i < attributeList.length; i++) {
		
		if(attributeList[i].name == text) {
			attributeList[i].order = (attributeList[i].order + 1) % 3;
			return attributeList;
		} 
	}
	
	return attributeList;
}

function removeElement(list, text) {
	
	for(var i = 0; i < list.length; i++) {
		if(list[i] == text) {
			list.splice(i,i);
			return list;
		}
	}
	
	return list;
}

function changeSomeBlock(action, text, detail, block) {

	switch(action) {
	
		case "addMeasure": block.measures.push(text);
			break;
		case "addAttribute": block.attributes.push({name: text, order: 0});
			break;
		case "up": if(detail == "measure") block.measures = sort(block.measures, text, "up"); else block.attributes = sort(block.attributes, text, "up");
			break;
		case "down": if(detail == "measure") block.measures = sort(block.measures, text, "down"); else block.attributes = sort(block.attributes, text, "down");
			break;
		case "changeOrder": changeOrder(block.attributes, text); 
			break;
		case "editFilter": block.filter = text;
			break;
		case "remove": if(detail == "measure") removeElement(block.measures, text); else removeElement(block.attributes, text);
			break;
		default: break;
	}
	
	return block;
}

function changeBlockInformation(blockId, action, text, detail) {
	
	var foundBlock = false;
	var blocks = $('#dropResults').data('blockInformation');
	
	for(var i = 0; i < blocks.length; i++) {
		if(blocks[i].blockId == blockId) {
			blocks[i] = changeSomeBlock(action, text, detail, blocks[i]); 
			foundBlock= true;
			break;
		}
	}
	
	if(!foundBlock) {
		var newBlock = {blockId: blockId, measures: [], attributes: [], filter:""};
		blocks.push(changeSomeBlock(action, text, detail, newBlock));
	}
	
	$('#dropResults').data('blockInformation', blocks); 
	
	for(var i = 0; i < blocks.length; i++) {
		for(var j = 0; j < blocks[i].measures.length; j++) {
			
		}
	}
}

function activateSorting() {
	$(".upButton").live('click', up);
	$(".downButton").live('click', down);
	$(".removeButton").live('click', removeIt);
	$(".orderAttribute").live('click', swapOrdering);
}

var up = function() {
	if($(this).parent().find(".emphasized")) {
		var node = $(this).parent().find(".emphasized");
		node.insertBefore(node.prev("li"));
		if($(this).is('.measureRelevant')) changeBlockInformation($(this).parent().parent().attr('blockId'), "up", node.text(), "measure");
		if($(this).is('.attributeRelevant')) changeBlockInformation($(this).parent().parent().attr('blockId'), "up", node.text(), "attribute");
	}
}

var down = function() {
	if($(this).parent().find(".emphasized")) {
		var node = $(this).parent().find(".emphasized");
		node.insertAfter(node.next("li"));
		if($(this).is('.measureRelevant')) changeBlockInformation($(this).parent().parent().attr('blockId'), "down", node.text(), "measure");
		if($(this).is('.attributeRelevant')) changeBlockInformation($(this).parent().parent().attr('blockId'), "down", node.text(), "attribute");
	}
}

var swapOrdering = function() {
	var givenOrder = $(this).text();
	var chosenOrder;
	if(givenOrder == "no order") chosenOrder = "Ascending";
	else if (givenOrder == "Ascending") chosenOrder = "Descending";
	else chosenOrder = "no order";
	$(this).text(chosenOrder);
	changeBlockInformation($(this).parent().parent().attr('blockId'), "changeOrder", $(this).attr("orderFor"), "measure");
}

var removeIt = function() {
	if($(this).parent().find(".emphasized")) {
		var node = $(this).parent().find(".emphasized");
		if($(this).is('.measureRelevant')) changeBlockInformation($(this).parent().parent().attr('blockId'), "remove", node.text(), "measure");
		if($(this).is('.attributeRelevant')) changeBlockInformation($(this).parent().parent().attr('blockId'), "remove", node.text(), "attribute");
		node.remove();
	}
}

function appendValue(textArea, textToAppend) {
	var oldText = textArea.val();
	textArea.val(oldText + textToAppend);
}
