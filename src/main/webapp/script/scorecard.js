var designPaper;
var paper;
var background;
var lineToDraw;
var isVertical;
var lineOrigin;
var orientation="portrait";
var blocks = [];
var scorecardBlocks = [];
var modelBlocks = [];
var lines = [];
var mainLine;
var originalBlock;
var selectedBlock;

function getPadBox() {
	if(orientation == "portrait") return [356, 91, 384, 502]; else return [297, 150, 502, 384];
}

function drawPie(representationDetail, x, y, width, height) {
	
	var pieElements = [];
	var cx = x + width/2;
	var cy = y + height/2;
	var r = width/4;
	var y1 = cy - r; 
	var x2 = cx + r/Math.sqrt(2);
	var y2 = cy + r/Math.sqrt(2);
	var x3 = cx - r;
	var y3 = cy;
	var rExtended = r + 5;
	var x2Extended = cx + rExtended/Math.sqrt(2);
	var y2Extended = cy + rExtended/Math.sqrt(2);
	var x3Extended = cx - rExtended;
	var y3Extended = cy;
	
	var pieBackground = designPaper.circle(x + width/2, y + height/2, width/4);
	pieElements.push(pieBackground);
	var pie1 = designPaper.path("M" + cx + ", "  + cy + "L" + cx + ", " + y1 + "A" +  r + "," + r + ",0,0,1," + x2 + "," + y2 + "L" + cx + " " + cy);
	pie1.attr("fill", "#aaa");
	pieElements.push(pie1);
	var pie2 = designPaper.path("M" + cx + ", "  + cy + "L" + x2 + ", " + y2 + "A" +  r + "," + r + ",0,0,1," + x3 + "," + y3 + "L" + cx + " " + cy);
	pie2.attr("fill", "#ccc");
	pieElements.push(pie2);
	
	var attributeElement = designPaper.text(x, y, "Attribute");
	attributeElement.attr({"font": "Arial", "font-size": 12});
	var attributeBox = attributeElement.getBBox();
	attributeElement.translate(attributeBox.width/2 + 4, height - attributeBox.height - 4);
	pieElements.push(attributeElement);
	
	var measureElement = designPaper.text(x, y, "Measure");
	measureElement.attr({"font": "Arial", "font-size": 12});
	var measureBox = measureElement.getBBox();
	measureElement.translate(width - measureBox.width/2 - 4, height - measureBox.height - 4);
	pieElements.push(measureElement);
	
	var pie3 = designPaper.path("M" + x2Extended + ", " + y2Extended + "A" +  rExtended + "," + rExtended + ",0,0,1," + x3Extended + "," + y3Extended);
	pieElements.push(pie3);
	
	return pieElements;
}

function drawBar(representationDetail, x, y, width, height) {
	
	var barElements = [];
	var x1 = x + width/3;
	var y1 = y + 24;
	var y2 = y + height - 12;
	var atomHeight = (y2 - y1)/21;
	
	barElements.push(designPaper.path("M" + x1 + "," + y1 + "L" + x1 + ", " + y2));
	barElements.push(designPaper.rect(x1, y1 + atomHeight, 0.6 * width, 4 * atomHeight));
	barElements.push(designPaper.rect(x1, y1 + 6 * atomHeight, 0.2 * width, 4 * atomHeight));
	barElements.push(designPaper.rect(x1, y1 + 11 * atomHeight, 0.3 * width, 4 * atomHeight));
	barElements.push(designPaper.rect(x1, y1 + 16 * atomHeight, 0.5 * width, 4 * atomHeight));
	var attributeElement = designPaper.text(x + width/6, y1 + 12, "Attribute"); 
	
	return barElements;	
}

function drawColumn(representationDetail, x, y, width, height) {
	
}

function drawLineChart(representationDetail, x, y, width, height) {
	
	var lineElements = [];
	var cellWidth = (width - 8)/4;
	var cellHeight = (height - 28)/4;
	var stepWidth =(width - 8)/20;
	
	var x1 = x + 4;
	var x2 = x + width - 4;
	var y1 = y + 24;
	var y2 = y + height - 4;
	var path;
	
	lineElements.push(designPaper.rect(x + 4, y + 24, width - 8, height - 28));
	
	for(var i = 1; i < 5; i++) {
		var y3 = y1 + i* cellHeight;
		var x3 = x1 + i*cellWidth;
		lineElements.push(designPaper.path("M" + x1 + "," + y3 + "L" + x2 + ", " + y3));
		lineElements.push(designPaper.path("M" + x3 + "," + y1 + "L" + x3 + ", " + y2)); 
	}
	
	var yStart = y1 + Math.random() * (height - 28);
	path = "M" + x1 + ", " + y1;
	for(var i = 1; i < 21; i++) {
		var xIth = x1 + i * stepWidth; 
		var yIth = y1 + Math.random() * (height - 28);
		path = path + "L" + xIth + ", " + yIth;
	}
	
	var theLine = designPaper.path(path);
	theLine.attr("stroke-width", 2);
	lineElements.push(theLine);
	
	return lineElements;	
}

function drawTable(representationDetail, x, y, width, height) {
	
}

function drawIndicator(representationDetail, x, y, width, height) {
	
}

function drawContainer(representationDetail, x, y, width, height) {
	
}

function drawDerivation(representationDetail, x, y, width, height) {
	
}

Raphael.fn.block = function(representationType, representationDetail, x, y, width, height, attributes, measures, title, titlePosition, draggable, blockId) {
	
	var blockBackground = designPaper.rect(x, y, width, height);
	blockBackground.attr("fill", "#fff");
	
	if(draggable) blockBackground.drag(moveBlock, dragBlock, dropBlock);
	else blockBackground.click(chooseBlock)
	
	var titleElement = designPaper.text(x, y, title);
	titleElement.attr({"font": "Arial", "font-size": 16});
	titleElement.translate(width/2, 12);
	
	var drawing = [];
	
	if(representationType == "pie") {
		drawing = drawPie(representationDetail, x, y, width, height);
	}
	else if (representationType == "bar") {
		drawing = drawBar(representationDetail, x, y, width, height);
	}
	else if (representationType == "line") {
		drawing = drawLineChart(representationDetail, x, y, width, height);
	}
	
	return {
		background: blockBackground,
		title: titleElement,
		drawing: drawing,
		status: "prototype",
		representationType: representationType,
		representationDetail: representationDetail,
		attributes: attributes,
		measures: measures,
		titlePosition: titlePosition,
		blockId: blockId
	};
}

function removeBlock(block) {
	block.background.remove();
	block.title.remove();
	
	while(block.drawing.length > 0) block.drawing.pop().remove();
}

function emptyPad() {
	while(lines.length > 0) lines.pop().remove();
	while(scorecardBlocks.length > 0) removeBlock(scorecardBlocks.pop());
}

var dragBlock = function() {
	
	
	this.ox = this.attr("x");
	this.oy = this.attr("y");
	
	for(var i = 0; i < blocks.length; i++) {
		if(blocks[i].background == this) {
			
			originalBlock = blocks[i];
			var box = blocks[i].background.getBBox();
			blocks.push(designPaper.block(blocks[i].representationType, blocks[i].representationDetail, box.x, box.y, box.width, box.height, blocks[i].attributes, blocks[i].measures, blocks[i].title.attr('text'), blocks[i].titlePosition, true, blocks[i].blockId));		
		
			this.toFront();
			blocks[i].title.toFront();
			for(var j = 0; j < blocks[i].drawing.length; j++) {
				blocks[i].drawing[j].toFront();
			}
		}
	}
}

function moveTitle(title, newX, newY) {
	var att = {x: newX, y: newY};
	title.attr(att);
}

function moveDrawing(drawing, newX, newY) {
	var att;
	
	for(var j = 0; j < drawing.length; j++) {
		var elem = originalBlock.drawing[j];
		var path = elem.attr("path");
		if(path) att = {path: movePath(path, newX, newY)}; else att = {x: elem.attr("x") + newX, y: elem.attr("y") + newY};
		drawing[j].attr(att);
	} 
}

var moveBlock = function(dx, dy) {
	
	var newX = this.ox + dx;
	var newY = this.oy + dy;
	
	var att = {x: newX, y: newY};
	this.attr(att);
	
	for(var i = 0; i < blocks.length; i++) {
		
		if(blocks[i].background == this) {
			moveDrawing(blocks[i].drawing, dx, dy);
			moveTitle(blocks[i].title,newX, newY);
		}
	}
	
}

var dropBlock = function() {
	
	var box = this.getBBox();
	var xMedian = box.x + box.width/2;
	var yMedian = box.y + box.height/2;
	
	for(var i = 0; i < blocks.length; i++) {
		if(blocks[i].background == this) {
			var areas = computeAreas(mainLine, [356, 91, 384, 502]);
			mainLine = considerBlockInLine(blocks[i], mainLine, areas[0], areas[1]);  
			emptyPad();
			drawIt(getPadBox(), mainLine);
			removeBlock(blocks[i]);
			serializeDesign();
			break;
		}
	}
}

var chooseBlock = function() {
	for(var i = 0; i < scorecardBlocks.length; i++) {
		var sc = scorecardBlocks[i];
		
		if(sc.background != this) sc.background.animate({"fill": "#fff", "fill-opacity": 1},500);
	}
	this.animate({"fill": "#66f", "fill-opacity": .2},500);
	selectedBlock = this;
}

function computeAreas(line, rect) {
	var scale = line.width/1000;
	
	if(line.kind == "column") return [[rect[0], rect[1], scale * rect[2], rect[3]],[rect[0] + scale * rect[2], rect[1], (1 - scale) * rect[2], rect[3]]];
	else return [[rect[0], rect[1], rect[2], scale * rect[3]],[rect[0], rect[1] + scale * rect[3], rect[2], (1- scale) * rect[3]]];
}

function considerBlockInLine(block, line, first, second) {
	
	var box = block.background.getBBox();
	var xMedian = box.x + box.width/2;
	var yMedian = box.y + box.height/2;
	var hitsFirst = false;
	var hitsSecond = false;
	
	if(first[0] < xMedian && first[1] < yMedian && first[0] + first[2] > xMedian && first[1] + first[3] > yMedian) hitsFirst = true;
	if(second[0] < xMedian && second[1] < yMedian && second[0] + second[2] > xMedian && second[1] + second[3] > yMedian) hitsSecond = true;
	
	if(hitsFirst && !line.first) {
		line.first = designPaper.scLine("block", block.blockId, 0, null, null);
	}
	else if(hitsFirst) {
		var areas = computeAreas(line, first);
		line.first = considerBlockInLine(block, line.first,  areas[0], areas[1]);
	}
	
	if(hitsSecond && !line.second) line.second = designPaper.scLine("block", block.blockId, 0, null, null);
	else if(hitsSecond) {
		var areas = computeAreas(line, second);
		line.second = considerBlockInLine(block, line.second,  areas[0], areas[1]);
	}
	
	return line;
}

function showScorecard() {
	paper = Raphael(728, 60, 548, 684);
	background = paper.rect(82,91, 384, 502);
	background.attr("fill", "#eee");
	background.attr("stroke", "#eee");
}

function rotateScorecard() {
	background.rotate(90);
}

function rotateDesign() {	
	designBackground.rotate(90);
	designCanvas.rotate(90);
	if(orientation == "portrait") orientation = "landscape"; else orientation = "portrait";
	emptyPad();
	drawIt(getPadBox(), mainLine);

}

function showDesignCanvas() {
	
	designPaper = Raphael(180, 60, 1096, 684);
	var bigCanvas = designPaper.rect(0,0, 1096, 684);
	bigCanvas.attr("fill", "#ffa");
	bigCanvas.attr("stroke", "#000");
	
	var leftCanvas = designPaper.rect(5, 5, 274, 674);
	leftCanvas.attr("fill", "#ffd");
	leftCanvas.attr("stroke", "#000");
	
	var drillCanvas = designPaper.rect(817, 5, 274, 674);
	drillCanvas.attr("fill", "#ffd");
	drillCanvas.attr("stroke", "#000");
	
	designBackground = designPaper.rect(346, 81, 404, 522);
	designBackground.attr("fill", "#000");
	designBackground.attr("stroke", "#000");
	designBackground.drag(drawLine, startLineDrawing, stopLineDrawing);
	
	designCanvas = designPaper.rect(356, 91, 384, 502);
	designCanvas.attr("fill", "#eee");
	designCanvas.attr("stroke", "#eee");
	
	mainLine = parse($('#designContainer frame *:nth-child(1)'));
	
	$('#designContainer blocks block').each(function() {
		if($(this).is("block")) parseBlock($(this));
	});
	
	for(var i = 0; i < modelBlocks.length; i++) {
		blocks.push(designPaper.block(modelBlocks[i].presentationType, modelBlocks[i].presentationDetail, 52, 12 + i * 192, 180, 180, modelBlocks[i].attributes, modelBlocks[i].attributes, modelBlocks[i].title, null, true, 0));
	}
	
	drawIt(getPadBox(), mainLine);
}

Raphael.fn.scLine = function(kind, blockId, width, first, second) {
	
	return {
		kind: kind,
		blockId: blockId,
		width: width,
		first: first,
		second: second
	};
}

function drawIt(rect, lineElement) {

	var x1, x2, y1, y2;
	var line;
	var lineAttrs = {"stroke": "#111", "stroke-dasharray": ".", "stroke-width": 2};
	
	if(lineElement.kind == "column") {
		var newWidth = rect[2] * lineElement.width/1000;
		x1 = rect[0] + newWidth;
		x2 = x1;
		y1 = rect[1];
		y2 = rect[1] + rect[3];
		line = designPaper.path("M" + x1 + "," + y1 + "L" + x2 + "," + y2);
		line.attr(lineAttrs);
		
		if(lineElement.first) drawIt([rect[0], rect[1], newWidth, rect[3]], lineElement.first);
		if(lineElement.second) drawIt([x1, rect[1], rect[2] - newWidth, rect[3]], lineElement.second);
			
		lines.push(line);
	}
	else if(lineElement.kind == "row") {
		
		var newHeight = rect[3] * lineElement.width/1000;
		x1 = rect[0];
		x2 = rect[0] + rect[2];
		y1 = rect[1] + newHeight;
		y2 = y1;
		line = designPaper.path("M" + x1 + "," + y1 + "L" + x2 + "," + y2);
		line.attr(lineAttrs);
		
		if(lineElement.first) drawIt([rect[0], rect[1], rect[2], newHeight], lineElement.first);
		if(lineElement.second) drawIt([rect[0], y1, rect[2], rect[3] - newHeight], lineElement.second);
		
		lines.push(line);
	}
	else if(lineElement.kind == "block") {
		
		for(var i = 0; i < modelBlocks.length; i++) {
			
			if(modelBlocks[i].blockId == lineElement.blockId) {
				scorecardBlocks.push(designPaper.block(modelBlocks[i].presentationType, modelBlocks[i].presentationDetail, rect[0] + 2, rect[1] + 2, rect[2] - 4, rect[3] - 4, modelBlocks[i].attributes, modelBlocks[i].attributes, modelBlocks[i].title, null, true, 0));
			}
		}
	}
}

function parseBlock(block) { 
	var blockId = block.attr("id");
	var presentationType = block.find("presentationType").text();
	var presentationDetail =  block.find("presentationDetail").text();
	var title =  block.find("title").text();
	var showTitle = block.find("showTitle").text();	
	var columnCount = block.find("columnCount").text();
	
	var attrs = [];
	
	block.filter("attribute").each(function() {
		attrs.push($(this).text());
	});
	
	var msrs = [];
	
	block.filter("measures").each(function() {
		msrs.push($(this).text());
	});
	
	var prms = [];
	
	block.filter("parameter").each(function() {
		prms.push($(this).text());
	});
	
	modelBlocks.push(designPaper.modelBlock(blockId, presentationType, presentationDetail, title, showTitle, columnCount, attrs, msrs, prms));
}

Raphael.fn.modelBlock = function(blockId, presentationType, presentationDetail, title, showTitle, columnCount, attrs, msrs, prms) {
	
	return {
		blockId: blockId,
		presentationType: presentationType, 
		presentationDetail: presentationDetail, 
		title: title, 
		showTitle: showTitle, 
		columnCount: columnCount,
		attrs: attrs, 
		msrs: msrs, 
		prms: prms
	};
}

function parse(elem) {
	
	var kind, w, blockId, first, second;
	
	if(elem.is("column")) {
		w = elem.attr('width');
		kind = "column";
	}
	else if(elem.is("row")) {
		w = elem.attr('height');
		kind = "row";
	}
	else if(elem.is("block")) {
		kind = "block";
		blockId = elem.attr('ref');
	}
	
	if(elem.children().size() == 2) {
		if(!elem.children().eq(0).is("empty")) first = parse(elem.children().eq(0));
		if(!elem.children().eq(1).is("empty")) second = parse(elem.children().eq(1));
	}
	
	return designPaper.scLine(kind, blockId, w, first, second);
}

var drawLine = function(dx, dy) {
	
	var lx = this.ox;
	var ly = this.oy;
	var lx2 = this.ox + dx;
	var ly2 = this.oy + dy;
	
	if(isVertical) {
		lineToDraw.attr("path", "M" + lx2 + "," + ly + "L" + lx2 + "," + ly2);
	}
	else {
		lineToDraw.attr("path", "M" + lx + "," + ly2 + "L" + lx2 + "," + ly2);
	}
};

var startLineDrawing = function(x, y, event) {
	var lx, ly;
	var relX = x - 180;
	var relY = y - 60;
	var isMatch = false;
	
	if(orientation == "portrait" &&  relX >= 356 && relX <= 356 + 384 && relY >= 81 && relY <= 91) {
		lx = relX;
		ly = 91;
		isMatch = true;
		isVertical = true;
	} 
	else if(orientation == "portrait" &&  relX >= 356 && relX <= 356 + 384 && relY >= 91 + 502 && relY <= 91 + 512) {
		lx = relX;
		ly = 91 + 502;
		isMatch = true;
		isVertical = true;
	} 
	else if (orientation == "portrait" && relX >= 346 && relX <= 356 && relY >= 91  && relY <= 91 + 502) {
		lx = 356;
		ly = relY;
		isMatch = true;
		isVertical = false;
	}
	else if(orientation == "portrait" &&  relX >= 356 + 384 && relX <= 356 + 394 && relY >= 91 && relY <= 91 + 502) {
		lx = 356 + 384;
		ly = relY;
		isMatch = true;
		isVertical = false;
	} 
	
	if(isMatch) {
		
		lineToDraw  = designPaper.path("M" + lx + "," + ly + "L" + lx + "," + ly);
		this.ox = lx;
		this.oy = ly;
		
	} 
}

function updateLine(x1, y1, x2, y2, lineElement, cX, cY, cWidth, cHeight) {
	var newKind;
	var cX2, cY2, cWidth2, cHeight2, ratio;
	
	var hitsFirst = false;
	var hitsSecond = false;
	var isFirstCandidate, isSecondCandidate;
	
	if(x1 == x2) {
		newKind = "column";
		ratio = (x1 - cX)/cWidth * 1000;
	}
	else {
		newKind = "row";
		ratio = (y1 - cY)/cHeight * 1000;
	}
	
	if(x1 == cX || x2 == cX || y1 == cY || y2 == cY) isFirstCandidate = true; 
	else isSecondCandidate = true;
	
	if(lineElement.kind == "column") {
	
		var newWidth = cWidth * lineElement.width/1000;
		
		if (x1 < cX + newWidth || x2 < cX + newWidth) hitsFirst = true;
		if (x1 > cX + newWidth || x2 > cX + newWidth) hitsSecond = true;
	}
	else if(lineElement.kind == "row") {
		
		var newHeight = cHeight * lineElement.height/1000;
		
		if (y1 < cY + newHeight || y2 < cY + newHeight) hitsFirst = true;
		if (y1 > cY + newHeight || y2 > cY + newHeight) hitsSecond = true;
	}
	else if(lineElement.kind == "block") {
		return lineElement;
	}
	
	if(isFirstCandidate) {
		
		cX2 = cX; 
		cY2 = cY;
		
		if(lineElement.kind == "column") { 
			
			cWidth2 = newWidth;
			cHeight2 = cHeight;
		}
		else {
			
			cWidth2 = cWidth;
			cHeight2 = newHeight;		
		}
		
		if(lineElement.first) {
			
			return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, updateLine(x1, y1, x2, y2, lineElement.first, cX2, cY2, cWidth2, cHeight2), lineElement.second);
		}
		else {
			if(hitsSecond) {
				
				var newLine = designPaper.scLine(newKind, 0, ratio, null, null);
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, newLine, lineElement.second);
			}
			else {
				return lineElement;
			}
		}
	}
	else {
		
		if(lineElement.kind == "column") { 
			
			cX2 = cX + newWidth; 
			cY2 = cY;
			cWidth2 = cWidth - newWidth;
			cHeight2 = cHeight;
		}
		else {
			
			cX2 = cX; 
			cY2 = cY + newHeight;
			cWidth2 = cWidth;
			cHeight2 = cHeight - newHeight;		
		}
		
		if(lineElement.second) {
			return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, lineElement.first, updateLine(x1, y1, x2, y2, lineElement.second, cX2, cY2, cWidth2, cHeight2));
		}
		else {
			if(hitsFirst) {
				
				var newLine = designPaper.scLine(newKind, 0, ratio, null, null);
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, lineElement.first, newLine);
			}
			else {
				return lineElement;
			}
		}
		
	}
}

var stopLineDrawing = function() {
	
	var x1, x2, y1, y2;
	
	var box = lineToDraw.getBBox();
	x1 = box.x;
	y1 = box.y;
	
	if(isVertical) {
		x2 = x1;
		y2 = y1 + box.height;
	}
	else {
		x2 = x1 + box.width;
		y2 = box.y;	
	}
	
	lineToDraw.remove();
	
	while(lines.length > 0) {
		lines.pop().remove();
	} 
	
	mainLine = updateLine(x1, y1, x2, y2, mainLine, 356, 91, 384, 502);
	drawIt(getPadBox(), mainLine);
	serializeDesign();
}  

function serializeLine(line) {
	
	if(!line) return "<empty />";
	else if (line.kind == "column") return "<column width='" + line.width +"'>" + serializeLine(line.first) + serializeLine(line.second) + "</column>";
	else if (line.kind == "row") return "<row height='" + line.width +"'>" + serializeLine(line.first) + serializeLine(line.second) + "</row>";
	else return "<block ref='" + line.blockId +"' />";	
}

function serializeDesign() {
	$('#designContainer frame').replaceWith("<frame>" + serializeLine(mainLine) + "</frame>");
}

function movePath(path, dx, dy) {

	var text = String(path);
	var parts = text.trim().split(/(\D)/);
	var i = 0;
	var k = 0;
	var leastLetter;
	
	for(var j = 0; j < parts.length; j++) {
		if(parts[j] == ',') { 
			i = 1; 
			k++;
		}
		else if(parts[j].match(/\d+/) && (leastLetter != 'A' || k > 4)) {
			
			var help = Number(parts[j]);
			if (i == 0) { parts[j] = help + dx;} else { parts[j] = help + dy; i = 0;}
		}
		else if(parts[j].match(/[MLA]/)) {
			leastLetter = parts[j];
			k = 0;
		}
	}
	
	return parts.join('');
}