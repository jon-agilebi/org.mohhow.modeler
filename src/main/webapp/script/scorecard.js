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
var drillBlocks = [];
var lines = [];
var mainLine;
var originalBlock;
var selectedBlock;
var backgrounds = [];
var portraitFrame;
var landscapeFrame;
var jsonBlock;

var presentationType;
var presentationDetail;
var blockAttributeNames = [];
var blockAttributeValues = [];
var title;
var blockId;
var countAttributesInBlock = 0;
var countMeasuresInBlock = 0;
var scrollPosition = 0;
var scrollStart = 0;
var scrollEnd = 0;
var scrollSpan;
var blockHeights = [];

var scorecardStandardText;
var scorecardTitle;
var scorecardFrame;
var scorecardHorizontal;
var scorecardVertical;
var showcasePie;
var showcaseBar;
var ruler = [];
         
function initializeBlocks(frames, blocks) {
	portraitFrame = frames;
	jsonBlock = blocks;
}

function drawSinglePie(raphael, x, y, width, height) {
	
	var pieElements = [];
	
	var cx = x + width/2;
	var cy = y + height/2;
	var r = width/4;
	
	if(height < width) r = height/4;
	
	var y1 = cy - r; 
	var x2 = cx + r/Math.sqrt(2);
	var y2 = cy + r/Math.sqrt(2);
	var x3 = cx - r;
	var y3 = cy;
	var rExtended = r + width/10;
	var x2Extended = cx + rExtended/Math.sqrt(2);
	var y2Extended = cy + rExtended/Math.sqrt(2);
	var x3Extended = cx - rExtended;
	var y3Extended = cy;
	
	var pieBackground = raphael.circle(x + width/2, y + height/2, width/4);
	pieElements.push(pieBackground);
	var pie1 = raphael.path("M" + cx + ", "  + cy + "L" + cx + ", " + y1 + "A" +  r + "," + r + ",0,0,1," + x2 + "," + y2 + "L" + cx + " " + cy);
	pie1.attr("fill", "#aaa");
	pieElements.push(pie1);
	var pie2 = raphael.path("M" + cx + ", "  + cy + "L" + x2 + ", " + y2 + "A" +  r + "," + r + ",0,0,1," + x3 + "," + y3 + "L" + cx + " " + cy);
	pie2.attr("fill", "#ccc");
	pieElements.push(pie2);
	
	return pieElements;
}

function drawPie(raphael, presentationDetail, x, y, width, height) {
	
	var allPieElements = [];
	
	if(presentationDetail == "mirror") {
		
		allPieElements = drawSinglePie(raphael, x, y + height/3, width/3, height/3);
		
		var textElements = ['A', 'B', 'C'];
		
		for(var i = 0; i < textElements.length; i++) {
			var text = raphael.text(x + width/2, y + (i + 1) * height/4, textElements[i]);
			allPieElements.push(text);
		}
		
		pie2 = drawSinglePie(raphael, x + 2 * width/3, y + height/3, width/3, height/3);
		for(var i = 0; i < pie2.length; i++) allPieElements.push(pie2[i]); 
	}
	else if (presentationDetail == "separated") {
		
		allPieElements = drawSinglePie(raphael, x, y + height/10, width/3, height/3);
		
		pie2 = drawSinglePie(raphael, x + 0.7 * width, y + height/10, width/3, height/3);
		for(var i = 0; i < pie2.length; i++) allPieElements.push(pie2[i]); 
		
		pie3 = drawSinglePie(raphael, x, y + 0.6 * height, width/3, height/3);
		for(var i = 0; i < pie3.length; i++) allPieElements.push(pie3[i]); 
		
		pie4 = drawSinglePie(raphael, x + 0.7 * width, y + 0.6 * height, width/3, height/3);
		for(var i = 0; i < pie4.length; i++) allPieElements.push(pie4[i]); 
		
	}
	else {
		allPieElements = drawSinglePie(raphael, x, y, width, height);
	}

	return allPieElements;
}

function drawBar(raphael, presentationDetail, x, y, width, height) {
	
	var barElements = [];
	var x1, x2, x3, x4;
	var textElements = ['A', 'B', 'C', 'D'];
	
	if(presentationDetail == "deviation" || presentationDetail == "sliding") {
		x1 = x + width/2; 
	}
	else {
		x1 = x + width/3;
	}
	
	var y1 = y + height/10;
	var y2 = y + 0.9 * height;
	var atomHeight = (y2 - y1)/21;
	
	barElements.push(raphael.path("M" + x1 + "," + y1 + "L" + x1 + ", " + y2));
	
	if(presentationDetail == "paired") {
		var xPaired = x + 2 * width/3;
		barElements.push(raphael.path("M" + xPaired + "," + y1 + "L" + xPaired + ", " + y2));
	}
	
	for(var i = 0; i < 4; i++) {
		if(presentationDetail == "range") {
			x2 = x1 + Math.random() * (x + 0.9 * width - x1);
			x3 = x2 + Math.random() * (x + 0.9 * width - x2);
		}
		else if(presentationDetail == "deviation" && i > 1) {
			x2 = x1 - Math.random() * (x + 0.9 * width - x1);
		}
		else if(presentationDetail == "paired") {
			x2 = x + 2 * width/3;
		}
		else {
			x2 = x1;
		}
		
		if(presentationDetail == "deviation" && i > 1) {
			x3 = x1 - x2;
		}
		else {
			x3 = Math.random() * (x + 0.9 * width - x2);
		}
		
		var bar = raphael.rect(x2, y1 + (1 + 5 * i) * atomHeight, x3, 4 * atomHeight);
		
		if(presentationDetail == "grouped") {
			var bar2 = raphael.rect(x2, y1 + (3 + 5 * i) * atomHeight, 0.8 * x3, 4 * atomHeight);
			bar2.attr("fill", "#000");
		}
		else if(presentationDetail == "sliding" || presentationDetail == "paired") {
			var xSliding = x1 - Math.random() * (x + 0.9 * width - x1);
			if(presentationDetail == "paired") xSliding =  x1 - Math.random() * width/3;
			var bar2 = raphael.rect(xSliding, y1 + (1 + 5 * i) * atomHeight, x1 - xSliding, 4 * atomHeight);
			bar2.attr("fill", "#ddd");
			barElements.push(bar2);
			
			bar.attr("fill", "#000");
		}
		else if(presentationDetail == "subdivided") {
			var xSubdivided = Math.random() * x3;
			var bar2 = raphael.rect(x2, y1 + (1 + 5 * i) * atomHeight, xSubdivided, 4 * atomHeight);
			bar2.attr("fill", "#000");
		}
		else {
			if(i == 3) bar.attr("fill", "#000"); else bar.attr("fill", "#ddd");
		}
		
		barElements.push(bar);
		
		if(presentationDetail != "sliding") {
			
			var xText;
			
			if(presentationDetail == "deviation" && i > 1) xText = x1 + width/6;
			else if (presentationDetail == "paired") xText = x + width/2;
			else xText = x + width/6;
			
			var text = raphael.text(xText, y1 + (3 + 5 * i) * atomHeight, textElements[i]);
			barElements.push(text);
			
		}	
	}
	
	return barElements;	
}

function drawColumn(raphael, presentationDetail, x, y, width, height) {
	
	var columnElements = [];
	var column;
	var y1, y2;
	
	var stepXs = [1,5,9,13];
	var defaultXs = [1, 6, 11, 16];
	var groupedXs = [2,6,12,16];
	var xs;
	var atomWidth;
	
	if(presentationDetail == "deviation") {
		y1 = y + height/2; 
	}
	else {
		y1 = y + 0.9 * height;
	}
	
	var x1 = x + width/10;
	var x2 = x + 0.9 * width;
	
	columnElements.push(raphael.path("M" + x1 + "," + y1 + "L" + x2 + ", " + y1));
	
	if(presentationDetail == "step") {
		xs = stepXs;
		atomWidth = (x2 - x1)/18;
	}
	else if(presentationDetail == "grouped") {
		xs = groupedXs;
		atomWidth = (x2 - x1)/22;
	}
	else {
		xs = defaultXs;
		atomWidth = (x2 - x1)/21;
	}
	
	for(var i = 0; i < 4; i++) {
		
		if(presentationDetail == "range") y2 = (1 - Math.random()) * y1; else y2 = y1;
		
		y3 = height/10 + (1 - Math.random()) * (y2 - height/10);
		
		if(presentationDetail == "deviation" && i > 1) column = raphael.rect(x1 + xs[i] * atomWidth, y2, 4 * atomWidth, y3);
		else column = raphael.rect(x1 + xs[i] * atomWidth, y3, 4 * atomWidth, y2 - y3);
		
		if(presentationDetail == "range" || (presentationDetail == "deviation" && i > 1) || (presentationDetail == "grouped" && i % 2 == 0)) {
			column.attr("fill", "#000");
		} else column.attr("fill", "#aaa");
		
		if(presentationDetail == "subdivided") {
			var y4 = y3 + Math.random() * (y2 - y3); 
			var column2 = raphael.rect(x1 + xs[i] * atomWidth, y4, 4 * atomWidth, y2 - y4);
			column2.attr("fill", "#000");
			columnElements.push(column2);
		}
		
		columnElements.push(column);
	}
	
	return columnElements;	
}

function drawCorrelation(raphael, presentationDetail, x, y, width, height) {
	
	var correlationElements = [];
	var normedLength;
	
	if(width > height) normedLength = height/20; else normedLength = width/20;
	
	for(var i = 1; i < 9; i++) {
		var xIth = x + 2 * normedLength + normedLength * 16 *  Math.random(); 
		var yIth = y + 2 * normedLength + normedLength * 16 *  Math.random();
		var dot = raphael.circle(xIth, yIth, normedLength);
		if(i == 5) dot.attr("fill", "#f00"); else dot.attr("fill", "#000");
		correlationElements.push(dot);
	}
	
	return correlationElements;
}

function drawLineChart(raphael, presentationDetail, x, y, width, height) {
	
	var lineElements = [];
	var cellWidth = (width - 8)/4;
	var cellHeight = (height - 28)/4;
	var stepWidth = (width - 8)/20;
	
	var x1 = x + 4;
	var x2 = x + width - 4;
	var y1 = y + height/10;
	var y2 = y + 0.9 * height;
	var path;
	
	lineElements.push(raphael.rect(x1, y1, width - 8, 0.8 * height));
	
	for(var i = 1; i < 5; i++) {
		var y3 = y1 + i* cellHeight;
		var x3 = x1 + i*cellWidth;
		lineElements.push(raphael.path("M" + x1 + "," + y3 + "L" + x2 + ", " + y3));
		lineElements.push(raphael.path("M" + x3 + "," + y1 + "L" + x3 + ", " + y2)); 
	}
	
	var yStart = y1 + Math.random() * (height - 28);
	path = "M" + x1 + ", " + y1;
	for(var i = 1; i < 21; i++) {
		var xIth = x1 + i * stepWidth; 
		var yIth = y1 + Math.random() * (height - 28);
		path = path + "L" + xIth + ", " + yIth;
	}
	
	var theLine = raphael.path(path);
	theLine.attr("stroke-width", 2);
	lineElements.push(theLine);
	
	return lineElements;	
}

function drawTable(raphael, presentationDetail, x, y, width, height) {
	
	var tableElements = [];
	var cellWidth = (width - 8)/4;
	var cellHeight = (height - 28)/4;
	var stepWidth = (width - 8)/20;
	
	var x1 = x + 4;
	var x2 = x + width - 4;
	var y1 = y + height/10;
	var y2 = y + 0.9 * height;
	var path;
	
	tableElements.push(raphael.rect(x1, y1, width - 8, 0.8 * height));
	
	for(var i = 1; i < 5; i++) {
		var y3 = y1 + i* cellHeight;
		var x3 = x1 + i*cellWidth;
		tableElements.push(raphael.path("M" + x1 + "," + y3 + "L" + x2 + ", " + y3));
		tableElements.push(raphael.path("M" + x3 + "," + y1 + "L" + x3 + ", " + y2)); 
	}
	
	return tableElements;
}

function drawIndicator(raphael, presentationDetail, x, y, width, height) {
	
	var indicatorElements = [];
	var normedLength;
	
	if(width > height) normedLength = height/8; else normedLength = width/8;
	
	if(presentationDetail == "circle" || presentationDetail == "circleAndArrow") {
		
		var circle = raphael.circle(x + width/2, y + height/2, normedLength * 3.5);
		if(presentationDetail == "circle") circle.attr("fill", "#ff0"); else circle.attr("fill", "#aaa");
		indicatorElements.push(circle);
	}
	
	if(presentationDetail == "square") {
		
		var square = raphael.rect(x, y, width, height);
		square.attr("fill", "#f00");
		indicatorElements.push(square);
		
	}
	else if(presentationDetail == "arrow" || presentationDetail == "circleAndArrow") {
		
		var x0 = x + normedLength;
		var y0 = y + height/2 - normedLength;
		var x1 = x + width - 3* normedLength;
		var y2 = y + height/2 - 2 * normedLength;
		var x3 = x + width - normedLength;
		var y3 = y + height/2;
		var y4 = y + height/2 + 2 * normedLength;
		var y5 = y + height/2 + normedLength;
		
		var arrow = raphael.path("M"  + x0 + "," + y0 + "L"  + x1 + "," + y0 + "L" + x1 + "," + y2 + "L" + x3 + "," + y3 + "L" + x1 + "," + y4 + "L" + x1 + "," + y5 + "L" + x0 + "," + y5 + "Z");
		arrow.attr("fill", "#000");
		indicatorElements.push(arrow);
	}
	
	return indicatorElements;
}

function drawPlain(raphael, presentationDetail, x, y, width, height) {	
	var plainElements = [];
	var text = raphael.text(x + width/2, y + height/2, "12 $");
	plainElements.push(text);
	
	return plainElements;
}

function getPadBox() {
	if(orientation == "portrait") return [356, 91, 384, 502]; else return [297, 150, 502, 384];
}

function drawRuler(padbox) {
	var topX = padbox[0] + padbox[2]/2;
	var topY = padbox[1] + 10;
	var top = designPaper.path("M" + topX + "," + padbox[1] + "L" + topX + ", " + topY);
	
	top.attr("stroke", "#f00");
	ruler.push(top);
	
	var leftX = padbox[0] + 10;
	var leftY = padbox[1] + padbox[3]/2;
	var left = designPaper.path("M" + padbox[0] + "," + leftY + "L" + leftX + ", " + leftY);
	
	left.attr("stroke", "#f00");
	ruler.push(left);
	
	var bottomY = padbox[1] + padbox[3] - 10;
	var bottomY2 = padbox[1] + padbox[3];
	var bottom = designPaper.path("M" + topX + "," + bottomY + "L" + topX + ", " + bottomY2);
	
	bottom.attr("stroke", "#f00");
	ruler.push(bottom);
	
	var rightX = padbox[0] + padbox[2] -10;
	var rightX2 = padbox[0] + padbox[2];
	var right = designPaper.path("M" + rightX + "," + leftY + "L" + rightX2 + ", " + leftY);
	
	right.attr("stroke", "#f00");
	ruler.push(right);
}

function showDesignCanvas() {
	
	designPaper = Raphael(180, 60, 1096, 684);
	
	// complete background
	
	var bigCanvas = designPaper.rect(0,0, 1096, 684);
	bigCanvas.attr("fill", "#ffa");
	bigCanvas.attr("stroke", "#000");
	
	// background for blocks to choose
	var leftCanvas = designPaper.rect(5, 5, 274, 674);
	leftCanvas.attr("fill", "#ffd");
	leftCanvas.attr("stroke", "#000");
	leftCanvas.drag(scroll, startScrolling, stopScrolling);
	
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
	
	drawRuler(getPadBox());
	
	mainLine = parse($("#designBlockInformation design fr[orientation='portrait'] *:nth-child(1)"));
	
	while(modelBlocks.length > 0) modelBlocks.pop();
	
	$('#designBlockInformation design block').each(function() {
		if($(this).is("block")) parseBlock($(this));
	});
	
	for(var i = 0; i < modelBlocks.length; i++) {
		if(modelBlocks[i].presentationType) blocks.push(designPaper.block(modelBlocks[i].presentationType, modelBlocks[i].presentationDetail, 52, 12 + i * 192, 180, 180, modelBlocks[i].attributes, modelBlocks[i].attributes, modelBlocks[i].title, null, true, modelBlocks[i].blockId));
	}
	
	blockReference = blocks;
	scrollSpan = 12 + modelBlocks.length * 192 - 647;
	
	drawIt(getPadBox(), mainLine);
}

function removeBlock(block) {
	block.background.remove();
	block.title.remove();
	
	while(block.drawing.length > 0) block.drawing.pop().remove();
}

function emptyPad() {
	while(lines.length > 0) lines.pop().remove();
	while(scorecardBlocks.length > 0) removeBlock(scorecardBlocks.pop());
	while(ruler.length > 0) ruler.pop().remove();
}

function clean() {
	$("#designBlockInformation fr[orientation='" + orientation + "']").empty();
	mainLine = parse($("#designBlockInformation fr[orientation='" + orientation + "'] *:nth-child(1)"));
	emptyPad();
}

function rotateDesign() {	
	designBackground.rotate(90);
	designCanvas.rotate(90);
	if(orientation == "portrait") orientation = "landscape"; else orientation = "portrait";
	emptyPad();
	mainLine = parse($("#designBlockInformation fr[orientation='" + orientation + "'] *:nth-child(1)"));
	
	drawIt(getPadBox(), mainLine);
	drawRuler(getPadBox());
}

/**
 * generic presentation of a block
 */

Raphael.fn.block = function(presentationType, presentationDetail, x, y, width, height, attributes, measures, title, titlePosition, draggable, blockId) {
	
	var blockBackground = designPaper.rect(x, y, width, height);
	blockBackground.attr("fill", "#fff");
	
	if(draggable) blockBackground.drag(moveBlock, dragBlock, dropBlock);
	else blockBackground.click(chooseBlock)
	
	var titleElement = designPaper.text(x, y, title);
	titleElement.attr({"font": "Arial", "font-size": 16});
	titleElement.translate(width/2, 12);
	
	var drawing = [];
	
	if(presentationType == "pie") {
		drawing = drawPie(designPaper, presentationDetail, x, y + 12, width, height - 12);
	}
	else if (presentationType == "bar") {
		drawing = drawBar(designPaper, presentationDetail, x, y + 12, width, height - 12);
	}
	else if (presentationType == "line") {
		drawing = drawLineChart(designPaper, presentationDetail, x, y + 12, width, height - 12);
	}
	else if (presentationType == "column") {
		drawing = drawBar(designPaper, presentationDetail, x, y + 12, width, height - 12);
	} 
	else if (presentationType == "indicator") {
		drawing = drawIndicator(designPaper, presentationDetail, x, y + 12, width, height - 12);
	}
	else if (presentationType == "text") {
		drawing = drawPlain(designPaper, presentationDetail, x, y + 12, width, height - 12);
	}
	else {
		drawing = drawTable(designPaper, presentationDetail, x, y + 12, width, height - 12);
	} 
	
	return {
		background: blockBackground,
		title: titleElement,
		drawing: drawing,
		status: "prototype",
		presentationType: presentationType,
		presentationDetail: presentationDetail,
		attributes: attributes,
		measures: measures,
		titlePosition: titlePosition,
		blockId: blockId
	};
}

var scroll = function(dx, dy) {
	scrollEnd = dy;
	
	var help =  Number(scrollPosition) - Number(scrollEnd);
	
	if(help > 0 && help < scrollSpan) {
		scrollPosition = help;
	
		for(var i = 0; i < blocks.length; i++) {
			
			var newY = Number(blockReference[i].background.attr("y")) + Number(dy);
			var att = {y: newY};
			blocks[i].background.attr(att);
			
			var newTitleY = Number(blockReference[i].title.attr("y")) + Number(dy);
			var attTitle = {y: newTitleY};
			blocks[i].title.attr(attTitle);
			blocks[i].title.toFront();
			
			for(var j = 0; j < blocks[i].drawing.length; j++) {
				var elem = blockReference[i].drawing[j];
				var path = elem.attr("path");
				
				if(path) {
					att = {path: movePath(path, 0, Number(dy))}; 
					blocks[i].drawing[j].attr(att);
				}
				
				if(elem.attr("x")) {
					var att = {y: Number(elem.attr("y")) + Number(dy)};
					blocks[i].drawing[j].attr(att);
				}
				
				if(elem.attr("cx")) {
					var att = {cy: Number(elem.attr("cy")) + Number(dy)};
					blocks[i].drawing[j].attr(att);
				}
				
				blocks[i].drawing[j].toFront();
			} 
		}
	}
	/*
	var help = Number(scrollEnd) + Number(scrollPosition);// - Number(scrollStart);
	if(help > scrollSpan) scrollPosition = scrollSpan;
	else if(help < 0) scrollPosition = 0;
	else scrollPosition = help;
	*/
}

var startScrolling = function() {
	scrollStart = this.attr("y");	
}

var stopScrolling = function() {
	// no action required here
}

function parseBlock(block) { 
	var blockId = block.attr("blockId");
	var presentationType = block.find("presentationType").text();
	var presentationDetail =  block.find("presentationDetail").text();
	var title =  block.find("title").text();
	
	var attrs = [];
	
	block.filter("attribute name").each(function() {
		attrs.push($(this).text());
	});
	
	var msrs = [];
	
	block.filter("measure").each(function() {
		msrs.push($(this).text());
	});
	
	var prms = [];
	/*
	 TODO
	block.filter("parameter").each(function() {
		prms.push($(this).text());
	}); */
	
	modelBlocks.push(designPaper.modelBlock(blockId, presentationType, presentationDetail, title, null, null, attrs, msrs, prms));
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

var dragBlock = function() {
	
	this.ox = this.attr("x");
	this.oy = this.attr("y");
	
	for(var i = 0; i < blocks.length; i++) {
		if(blocks[i].background == this) {
			var box = blocks[i].background.getBBox();
			originalBlock = designPaper.block(blocks[i].presentationType, blocks[i].presentationDetail, box.x, box.y, box.width, box.height, blocks[i].attributes, blocks[i].measures, blocks[i].title.attr('text'), blocks[i].titlePosition, false, blocks[i].blockId);		
			blocks.push(originalBlock);		
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

function moveDrawing(parts, dx, dy) {
	var att;
	
	for(var j = 0; j < parts.length; j++) {
		var elem = originalBlock.drawing[j];
		var path = elem.attr("path");
		
		if(path) {
			att = {path: movePath(path, dx, dy)}; 
			parts[j].attr(att);
		}
		
		if(elem.attr("x")) {
			var att = {x:Number(elem.attr("x"))  + Number(dx), y: Number(elem.attr("y")) + Number(dy)};
			parts[j].attr(att);
			
		}
		
		if(elem.attr("cx")) {
			var att = {cx:Number(elem.attr("cx"))  + Number(dx), cy: Number(elem.attr("cy")) + Number(dy)};
			parts[j].attr(att);
			
		}
	} 
}

var moveBlock = function(dx, dy) {
	
	var newX = Number(this.ox) + Number(dx);
	var newY = Number(this.oy) + Number(dy);
	
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
			
			if(xMedian > 817) { 
				if(selectedBlock) {
					var thisBlock = blockToModelBlock(blocks[i]);
					
					var predecessorId;
					
					if(drillBlocks.length > 0) predecessorId = drillBlocks[drillBlocks.length - 1].blockId; 
					else predecessorId = selectedBlock.blockId;
					
					$("#designBlockInformation successors").append("<successor blockId='" + predecessorId + "' successorId='" + thisBlock.blockId + "' />");
					
					drillBlocks.push(designPaper.block(thisBlock.presentationType, thisBlock.presentationDetail, 875, 12 + drillBlocks.length * 192, 180, 180, thisBlock.attributes, thisBlock.attributes, thisBlock.title, null, true, thisBlock.blockId));
				}
				
				removeBlock(blocks[i]);
			}
			else {
				
				if(!mainLine.kind) {
					var box = getPadBox();
					if(box[0] <= xMedian && box[0] + box[2] >= xMedian && box[1] <= yMedian && box[1] + box[3] >= yMedian) {
						mainLine = designPaper.scLine("block", blocks[i].blockId, 0, null, null);
						modelBlocks.push(blockToModelBlock(blocks[i]));
					}
				}
				else {
					
					var areas = computeAreas(mainLine, getPadBox());
					mainLine = considerBlockInLine(blocks[i], mainLine, areas[0], areas[1]);
					modelBlocks.push(blockToModelBlock(blocks[i]));
				}
				
				emptyPad();
				drawIt(getPadBox(), mainLine);
				removeBlock(blocks[i]);
				serializeDesign();			
			}
			
			break;
		} 
	}  
}
	
function blockToModelBlock(block) {
	return designPaper.modelBlock(block.blockId, block.presentationType, block.presentationDetail, block.title.attr('text'));	
}

function addToDrillStack(block, n) {
	
	var successorId = $("#designBlockInformation successor[blockId='" + block.blockId + "']").attr('successorId');
	
	if(successorId) {
		
		for(var i = 0; i < modelBlocks.length; i++) {
			
			if(modelBlocks[i].blockId == successorId) {
				var succBlock = designPaper.block(modelBlocks[i].presentationType, modelBlocks[i].presentationDetail, 875, 12 + n * 192, 180, 180, modelBlocks[i].attributes, modelBlocks[i].attributes, modelBlocks[i].title, null, false, modelBlocks[i].blockId);
				drillBlocks.push(succBlock);
				addToDrillStack(succBlock, n + 1);
			}
		}
		
	}
}

function createDrillStack(block) {
	while(drillBlocks.length > 0) removeBlock(drillBlocks.pop());
	addToDrillStack(block, 0)
}

var chooseBlock = function() {
	for(var i = 0; i < scorecardBlocks.length; i++) {
		var sc = scorecardBlocks[i];
		
		if(sc.background != this) sc.background.animate({"fill": "#fff", "fill-opacity": 1},500);
		if(sc.background == this) selectedBlock = sc;
	}
	
	this.animate({"fill": "#66f", "fill-opacity": .2},500);
	createDrillStack(selectedBlock);
}

/**
 * separates a rectangle into two rectangles according to the type and the position of the given line
 * 
 * @param line
 * @param rect
 * @returns {Array}
 */

function computeAreas(line, rect) {
	
	if(!line) {
		return  [[rect[0], rect[1], rect[2], rect[3]],[0,0,0,0]];
	}
	else {
		var scale = line.width/1000;
		if(line.kind == "column") return [[rect[0], rect[1], scale * rect[2], rect[3]],[rect[0] + scale * rect[2], rect[1], (1 - scale) * rect[2], rect[3]]];
		else return [[rect[0], rect[1], rect[2], scale * rect[3]],[rect[0], rect[1] + scale * rect[3], rect[2], (1 - scale) * rect[3]]];
	}
}

function considerBlockInLine(block, line, first, second) {
	
	var box = block.background.getBBox();
	var xMedian = box.x + box.width/2;
	var yMedian = box.y + box.height/2;
	var hitsFirst = false;
	var hitsSecond = false;
	
	if(first[0] < xMedian && first[1] < yMedian && first[0] + first[2] > xMedian && first[1] + first[3] > yMedian) hitsFirst = true;
	if(second[0] < xMedian && second[1] < yMedian && second[0] + second[2] > xMedian && second[1] + second[3] > yMedian) hitsSecond = true;
	
	if(hitsFirst && !line.first) line.first = designPaper.scLine("block", block.blockId, 0, null, null);
	else if(hitsFirst) {
		var areas = computeAreas(line.first, first);
		line.first = considerBlockInLine(block, line.first,  areas[0], areas[1]);
	}
	
	if(hitsSecond && !line.second) line.second = designPaper.scLine("block", block.blockId, 0, null, null);
	else if(hitsSecond) {
		var areas = computeAreas(line.second, second);
		line.second = considerBlockInLine(block, line.second,  areas[0], areas[1]);
	}
	
	return line;
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
	
	if(lineElement != null) {
		
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
					scorecardBlocks.push(designPaper.block(modelBlocks[i].presentationType, modelBlocks[i].presentationDetail, rect[0] + 2, rect[1] + 2, rect[2] - 4, rect[3] - 4, modelBlocks[i].attributes, modelBlocks[i].attributes, modelBlocks[i].title, null, false, modelBlocks[i].blockId));
				}
			}
		}
	}
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
		// portrait top
		lx = relX;
		ly = 91;
		isMatch = true;
		isVertical = true;
	} 
	else if(orientation == "portrait" &&  relX >= 356 && relX <= 356 + 384 && relY >= 91 + 502 && relY <= 91 + 512) {
		// portrait bottom
		lx = relX;
		ly = 91 + 502;
		isMatch = true;
		isVertical = true;
	} 
	else if (orientation == "portrait" && relX >= 346 && relX <= 356 && relY >= 91  && relY <= 91 + 502) {
		// portrait left
		lx = 356;
		ly = relY;
		isMatch = true;
		isVertical = false;
	}
	else if(orientation == "portrait" &&  relX >= 356 + 384 && relX <= 356 + 394 && relY >= 91 && relY <= 91 + 502) {
		//portrait right
		lx = 356 + 384;
		ly = relY;
		isMatch = true;
		isVertical = false;
	} 
	else if(orientation == "landscape" &&  relX >= 297 && relX <= 297 + 502 && relY >= 140 && relY <= 150) {
		// landscape top
		lx = relX;
		ly = 150;
		isMatch = true;
		isVertical = true;
	}
	else if(orientation == "landscape" &&  relX >= 297 && relX <= 297 + 502 && relY >= 150 + 384 && relY <= 150 + 394) {
		// landscape bottom
		lx = relX;
		ly = 150 + 384;
		isMatch = true;
		isVertical = true;
	}
	else if(orientation == "landscape" &&  relX >= 287 && relX <= 297 && relY >= 150 && relY <= 150 + 384) {
		// landscape left
		lx = 297;
		ly = relY;
		isMatch = true;
		isVertical = false;
	}
	else if(orientation == "landscape" &&  relX >= 297 + 502 && relX <= 297 + 512 && relY >= 150 && relY <= 150 + 384) {
		// landscape right
		lx = 297 + 502;
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

function drawInitialLine(x1, y1, x2, y2) {
	var padBox = getPadBox();
	if(x1 == x2) return designPaper.scLine("column", 0,  (x1 - padBox[0]) /padBox[2]*1000, null, null);
	else return designPaper.scLine("row", 0,  (y1 - padBox[1])/padBox[3]*1000, null, null);
}

function tuneRatio(givenValue) {
	if(givenValue >= 497 && givenValue <= 503) return 500; else return givenValue;
}

function updateLine(x1, y1, x2, y2, lineElement, cX, cY, cWidth, cHeight) {
	var newKind;
	var ratio, newWidth, newHeight, cXFirst, cXSecond, cYFirst, cYSecond, cHeightFirst, cHeightSecond, cWidthFirst, cWidthSecond;
	
	var hitsFirst = false;
	var hitsSecond = false;
	var isFirstCandidate, isSecondCandidate;
	
	if(!lineElement || (lineElement.kind != "row" && lineElement.kind != "column" && lineElement.kind != "block")) {
		return drawInitialLine(x1, y1, x2, y2);
	}
	else {	
		
		if(x1 == x2) {
			newKind = "column";
			//ratio = (x1 - cX)/cWidth * 1000;
		}
		else {
			newKind = "row";
			//ratio = (y1 - cY)/cHeight * 1000;
		}
		
		if(x1 == cX || x2 == cX || y1 == cY || y2 == cY) isFirstCandidate = true; 
		else isSecondCandidate = true;
		
		if(lineElement.kind == "column") {
		
			newWidth = cWidth * lineElement.width/1000;
			
			if (x1 < cX + newWidth || x2 < cX + newWidth) hitsFirst = true;
			if (x1 > cX + newWidth || x2 > cX + newWidth) hitsSecond = true;
			
			if(newKind == "column") {
				if(hitsFirst) ratio = (x1 - cX)/newWidth * 1000;
				else ratio = (x1 - cX - newWidth)/(cWidth - newWidth) * 1000;
			}
			else ratio = (y1 - cY)/cHeight * 1000;
		}
		else if(lineElement.kind == "row") {
			
			newHeight = cHeight * lineElement.width/1000;
			
			if (y1 < cY + newHeight || y2 < cY + newHeight) hitsFirst = true;
			if (y1 > cY + newHeight || y2 > cY + newHeight) hitsSecond = true;
			
			if(newKind == "row") {
				if(hitsFirst) ratio = (y1 - cY)/newHeight * 1000;
				else ratio = (y1 - cY - newHeight)/(cHeight - newHeight) * 1000;
			}
			else ratio = (x1 - cX)/cWidth * 1000;
		}
		else if(lineElement.kind == "block") {
			return lineElement;
		}
		
		if(ratio >= 495 && ratio <= 505) {
			ratio = 500;
			if(lineElement.kind == "column") newWidth = lineElement.width/2;
			else newHeight = lineElement.width/2;
		}
		
		if(lineElement.kind == "column") { 
			
			cWidthFirst = newWidth;
			cWidthSecond = cWidth - newWidth;
			cHeightFirst = cHeight;
			cHeightSecond = cHeight;
			cXFirst = cX;
			cXSecond = cX + newWidth;
			cYFirst = cY;
			cYSecond = cY;
		}
		else {
			
			cWidthFirst = cWidth;
			cWidthSecond = cWidth;
			cHeightFirst = newHeight;
			cHeightSecond = cHeight - newHeight;
			cXFirst = cX;
			cXSecond = cX;
			cYFirst = cY;
			cYSecond = cY + newHeight;	
		}
		
		if(hitsFirst && hitsSecond) {
			
			if(lineElement.first && isFirstCandidate) {
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, updateLine(x1, y1, x2, y2, lineElement.first, cXFirst, cYFirst, cWidthFirst, cHeightFirst), lineElement.second);
			}
			else if(lineElement.second && isSecondCandidate) {
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, lineElement.first, updateLine(x1, y1, x2, y2, lineElement.second, cXSecond, cYSecond, cWidthSecond, cHeightSecond));
			}
			if(isFirstCandidate) {
				var newLine = designPaper.scLine(newKind, 0, ratio, null, null);
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, newLine, lineElement.second);
			}
			else {
				var newLine = designPaper.scLine(newKind, 0, ratio, null, null);
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, lineElement.first, newLine);
			}
			
		}
		else if(hitsFirst) {
			
			if(lineElement.first) {
				
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, updateLine(x1, y1, x2, y2, lineElement.first, cXFirst, cYFirst, cWidthFirst, cHeightFirst), lineElement.second);
			}
			else if(newKind == lineElement.kind) {
				
				var newLine = designPaper.scLine(newKind, 0, ratio, null, null);
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, newLine, lineElement.second);
			}
			else return lineElement;
			
		}
		else if(hitsSecond) {
			
			if(lineElement.second) {
				
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, lineElement.first, updateLine(x1, y1, x2, y2, lineElement.second, cXSecond, cYSecond, cWidthSecond, cHeightSecond));
			}
			else if(newKind == lineElement.kind) {
			
				var newLine = designPaper.scLine(newKind, 0, ratio, null, null);
				return designPaper.scLine(lineElement.kind, lineElement.blockId, lineElement.width, lineElement.first, newLine);
			}
			else return lineElement;	
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
	
	var padBox = getPadBox();
	mainLine = updateLine(x1, y1, x2, y2, mainLine, padBox[0], padBox[1], padBox[2], padBox[3]);
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
	$("#designBlockInformation design fr[orientation='" + orientation + "']").empty();
	$("#designBlockInformation design fr[orientation='" + orientation + "']").append(serializeLine(mainLine));
}

/**
 * next what is needed for block.html
 */

var choosePresentationType = function(typeToChoose) {
	
	presentationType = this.data("presentationType");
	presentationDetail = this.data("presentationDetail");
	
	serializeChoice(blockId, presentationType, presentationDetail, title, blockAttributeNames, blockAttributeValues);
	
	for(var j = 0; j < backgrounds.length; j++) {
		if(backgrounds[j].attr("fill") != "grey") backgrounds[j].attr("fill", "cornsilk");
		backgrounds[j].attr("fill-opacity", "1");
	}
	
	this.animate({"fill": "#66f", "fill-opacity": .2},500);
	
	// show the details
	
	$('.detailThumbnailRow').hide();
	$(".detailThumbnailRow[presentationType='" + presentationType + "']").show();
	
	$('.additionalAttributeForm').hide();
	
	if(presentationDetail && presentationDetail != "unknown") {
		$(".additionalAttributeForm[presentationType='" + presentationType + "'][presentationDetail='" + presentationDetail + "']").show();
	}
};

function drawBlockChoice(containerId, presentationType, presentationDetail, isDetail, isSelected, isActive) {
	
	var thumbnail = Raphael(containerId, 80, 80);
	var thBackground = thumbnail.rect(2,2, 76, 76);
	thBackground.attr("fill", "cornsilk");
	thBackground.data("presentationType", presentationType);
	
	if(isDetail) thBackground.data("presentationDetail", presentationDetail); else thBackground.data("presentationDetail", "unknown");
	
	if(isActive) {
		thBackground.click(choosePresentationType);
		thBackground.attr("fill", "cornsilk");
		
		if(isSelected) {
			thBackground.attr("fill", "#66f");
			thBackground.attr("fill-opacity", ".2");
		}
		else thBackground.attr("fill", "cornsilk");
	}
	else thBackground.attr("fill", "grey");
	
	backgrounds.push(thBackground);
	
	if(presentationType == "pie") drawPie(thumbnail, presentationDetail, 2, 2, 76, 76);
	else if (presentationType == "bar") drawBar(thumbnail, presentationDetail, 2, 2, 76, 76);  
	else if (presentationType == "line") drawLineChart(thumbnail, presentationDetail, 2, 2, 76, 76);
	else if (presentationType == "correlation") drawCorrelation(thumbnail, presentationDetail, 2, 2, 76, 76);
	else if (presentationType == "column") drawColumn(thumbnail, presentationDetail, 2, 2, 76, 76);
	else if (presentationType == "table") drawTable(thumbnail, presentationDetail, 2, 2, 76, 76);
	else if (presentationType == "indicator") drawIndicator(thumbnail, presentationDetail, 2, 2, 76, 76);
	else if (presentationType == "text") drawPlain(thumbnail, presentationDetail, 2, 2, 76, 76);
}  

function serializeChoice(blockId, presentationType, presentationDetail, title, attributeNames, attributeValues) {
	
	var block = "<block blockId='" + blockId + "'><presentationType>" + presentationType + "</presentationType>";
	block = block + "<presentationDetail>" + presentationDetail + "</presentationDetail>";
	block = block + "<title>" + title + "</title>";
	
	for(var i= 0; i < attributeNames.length; i++) {
		block = block + "<" + attributeNames[i] +">" + attributeValues[i] + "</" + attributeNames[i] + ">";
	}
	
	block = block + "<structure /></block>"

	$('#serializedBlock').data('serializedBlock', block);
}

function takeAttribute(attributeName, attributeValue) {
	
	if(attributeName != "presentationType" && attributeName != "presentationDetail" 
        && attributeName != "title"
        && attributeName != "structure") {
		
		for(i=0; i< blockAttributeNames.length; i++) {
				if(blockAttributeNames[i] == attributeName){
						blockAttributeValues[i] = attributeValue;
						$(".blockAttributeInput[inputFor='" + attributeName + "']").val(attributeValue);
				}
		} 
	} 	
}

function readBlockInformation() {
	// choose at first presentation type, presentation detail, title and block id
	//alert('hello world');
	var block = $('#readBlockFirstTime block');
	presentationType = block.find("presentationType").text();
	presentationDetail = block.find("presentationDetail").text();
	title = block.find("title").text();
	blockId = block.attr("blockId");
	//alert(' and the block id is ' + blockId);
	// initialize arrays for additional attributes
	
	blockAttributeNames = [];
	blockAttributeValues = [];
	
	$(".blockAttributeInput").each(function(){
		if($(this).parent().parent().attr("presentationType") == presentationType && $(this).parent().parent().attr("presentationDetail") == presentationDetail) {
			var attrName = $(this).attr('inputFor');
			blockAttributeNames.push(attrName);
			blockAttributeValues.push("");
		}
	}); 
	
	//block.children().map(takeAttribute);
	
	block.children().each(function() {
		takeAttribute($(this).tagName, $(this).text());
	})
	
	// determine the number of attributes and measures
	
	countAttributesInBlock = block.find("measure").size();
	countMeasuresInBlock = block.find("attribute").size();
	
	// store the data
	
	serializeChoice(blockId, presentationType, presentationDetail, title, blockAttributeNames, blockAttributeValues);
}

function saveAdditionalAttributes() {
	$(".blockAttributeInput").each(function(){
		if($(this).parent().parent().attr("presentationType") == presentationType && $(this).parent().parent().attr("presentationDetail") == presentationDetail) {
			var attrName = $(this).attr('inputFor');
			var attrValue = $(this).val();
			for(i=0; i< blockAttributeNames.length; i++) if(blockAttributeNames[i] == attrName) blockAttributeValues[i] = attrValue;
		}
	});
	
	serializeChoice(blockId, presentationType, presentationDetail, title, blockAttributeNames, blockAttributeValues);
}

/**
 * functions for scorecard layout 
 */

var blindText = "Lorem ipsum dolor sit amet,\n consetetur sadipscing elitr,\n sed diam nonumy eirmod tempor\n invidunt ut labore et dolore magna\n aliquyam erat, sed diam voluptua.\n At vero eos et accusam et justo\n duo dolores et ea rebum.\n Stet clita kasd gubergren,\n no sea takimata sanctus\n est Lorem ipsum dolor sit amet.\n Lorem ipsum dolor sit amet,\n consetetur sadipscing elitr,\n sed diam nonumy eirmod tempor\n invidunt ut labore et dolore magna\n aliquyam erat, sed diam voluptua."

function showScorecard() {
	paper = Raphael(728, 60, 548, 684);
	
	background = paper.rect(82,91, 384, 502);
	background.attr("fill", "#eee");
	background.attr("stroke", "#eee");
	
	scorecardFrame = paper.rect(82,91, 384, 502);
	
	scorecardTitle = paper.text(82 + 384/2, 100, "Scorecard Title");
	scorecardHorizontal = paper.path("M82,320L466,320");
	scorecardVertical = paper.path("M274,320L274,593"); 
	var standardTextBackground = Raphael(810, 380, 60, 60);
	scorecardStandardText = paper.text(178,456, blindText);  
	showcasePie = drawPie(paper, "mirror", 275, 320, 200 ,273);
	showcaseBar = drawBar(paper, "plain", 82, 120, 380, 120);
	
}

function rgb(red, green, blue) {
	return "rgb(" + red + ", " + green + ", " + blue + ")"
}

function dash(style) {
	if(style == "solid") return "";
	else if(style == "dotted") return ".";
	else return "--";
}

function updateScorecard(id, value) {
	if(id == "standardText") {
		var l = value.split(";");
		scorecardStandardText.attr('font-family', l[0]);
		scorecardStandardText.attr('font-size', l[1]);
		scorecardStandardText.attr('stroke', rgb(l[2], l[3], l[4]));
	}
	else if(id == "scorecard_style_margin") {
		scorecardFrame.attr("x", 82 + value);
		scorecardFrame.attr("y", 91 + value); 
		scorecardFrame.attr("width", 384 - 2 * value);
		scorecardFrame.attr("height", 502 - 2 * value);
	}
	else if(id == "scorecard_style_border_width") scorecardFrame.attr("stroke-width", value);
	else if(id == "line_style_dash_style") {
		scorecardHorizontal.attr("stroke-dasharray", dash(value));
		scorecardVertical.attr("stroke-dasharray", dash(value));
	}
	else if(id == "scorecard_style_border_color") {
		var colors = value.split(";");
		scorecardFrame.attr("stroke", rgb(colors[0], colors[1], colors[2]));
	}
	else if(id == "scorecard_style_title_position") {
		var w = scorecardTitle.getBBox().width;
		
		if(value == "left") scorecardTitle.attr("x", 82 + w/2);
		else if(value ="right") scorecardTitle.attr("x", 82 + 384 - w);
		else scorecardTitle.attr("x", 82 + 384/2 - w/2);
	}
	else if(id == "scorecard_style_border_dash_style") scorecardFrame.attr("stroke-dasharray", dash(value));
	else if(id == "scorecard_style_round_the_edge") scorecardFrame.attr("r", value);
	else if(id == "scorecard_style_background") {
		var colors = value.split(";");
		background.attr("fill", rgb(colors[0], colors[1], colors[2]));
	}
	else if(id == "line_style_width") {
		scorecardHorizontal.attr("stroke-width", value);
		scorecardVertical.attr("stroke-width", value);
	}
	else if(id == "line_style_color") {
		var colors = value.split(";");
		scorecardHorizontal.attr("stroke", rgb(colors[0], colors[1], colors[2]));
		scorecardVertical.attr("stroke", rgb(colors[0], colors[1], colors[2]));
	}
	
	/*
	 * The following attributes are not considered in the update function above: chart_style_axis_color, chart_style_axis_dash_style, chart_style_grid_width,
	 * chart_style_grid_color, chart_style_grid_dash_style, chart_style_chart_border_width, chart_style_chart_border_color, chart_style_chart_border_dash_style,
	 * chart_style_label_padding, chart_style_label_text, chart_style_axis_text, chart_style_axis_width, block_style_title_style, block_style_background, scorecard_style_padding
	 * block_style_round_the_edge, scorecard_style_title_style, block_style_margin, block_style_padding
	 */
}
