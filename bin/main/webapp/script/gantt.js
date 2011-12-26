var ids = [];
var durations = [];
var names = [];
var predecessors = [];
var starts = [];
var ends = [];
var maxDuration = 0;
var maxTextWidth = 0;
var maxTextHeight = 0;
var textBoxes = [];


function showBurndown() {
	var burndownPaper = Raphael(192, 514, 520, 225);
	var bCanvas = burndownPaper.rect(0,0, 520, 225);
	bCanvas.attr("fill", "#ffa");
	bCanvas.attr("stroke", "#000");
	
}


/*
function showGantt() {
	
	// create the canvas
	
	paper = Raphael(40, 40, 1200, 700);
	var canvas = paper.rect(0,0, 1200, 700);
	canvas.attr("fill", "#ffa");
	canvas.attr("stroke", "#000");
	
	// parse xml
	
	$('#actions action').each(function() {
		var action = $(this);
		ids.push(action.children().eq(0).text());
		names.push(action.children().eq(1).text());
		durations.push(action.children().eq(2).text());
		predecessors.push(action.children().eq(3).text());
	});
	
	// compute begin and end for each action, furthermore compute the maximal duration
	
	for(var i = 0; i < ids.length; i++) {
		
		if(predecessors[i]) {
			var start = Math.abs(findEnd(predecessors[i]));
			starts.push(start);
			ends.push(start + Math.abs(durations[i]));
			
			if(start + Math.abs(durations[i]) > maxDuration) maxDuration = start + Math.abs(durations[i]);
		}
		else {
			starts.push(0);
			ends.push(Math.abs(durations[i]));
			
			if(Math.abs(durations[i]) > maxDuration) maxDuration = Math.abs(durations[i]);
		}
		
	}
	
	// create text boxes and compute maximal text width and height
	
	for(var i = 0; i < ids.length; i++) {
		var text = paper.text(0,0, names[i]);
		if(text.getBBox().width > maxTextWidth) maxTextWidth = text.getBBox().width;
		if(text.getBBox().height > maxTextHeight) maxTextHeight = text.getBBox().height;
		
		textBoxes.push(text);
	}
	
	// draw frame and line
	
	var ganttBackground = paper.rect(10,10, 1180, (2 * ids.length - 1) * maxTextHeight + 10);
	ganttBackground.attr("fill", "#fff");
	ganttBackground.attr("stroke", "#000");
	var verticalX = Math.ceil(maxTextWidth) + 30;
	var verticalY2 = (2 * ids.length - 1) * maxTextHeight + 20;
	
	var vertical = paper.path("M" + verticalX + ",10L" + verticalX + "," + verticalY2);
	vertical.attr("stroke-dasharray", ".");
	
	// move text elements and draw gantt chart
	
	for(var i = 0; i < ids.length; i++) {
		textBoxes[i].translate(25 + textBoxes[i].getBBox().width/2, 2 * i * maxTextHeight + 20).toFront();
		
		var blockX = 30 + maxTextWidth + (1150 - 30 - maxTextWidth) * starts[i] /maxDuration;
		var blockY = 2 * i  * maxTextHeight + 15;
		
		if(durations[i] > 0) {
			var blockWidth = (1150 - 30 - maxTextWidth) * durations[i] / maxDuration;
			
			var block = paper.rect(blockX, blockY, blockWidth, maxTextHeight);
			block.attr("fill", "#bbd");
			block.attr("stroke", "#000");
		}
		else {
			var diamondMidHeight = blockY + maxTextHeight/2;
			var diamondHeight = blockY + maxTextHeight;
			var diamondMidWidth = blockX + maxTextHeight/2;
			var diamondWidth = blockX + maxTextHeight;
			
			var diamond = paper.path("M" + blockX + "," + diamondMidHeight + "L" + diamondMidWidth + "," + blockY + "L" + diamondWidth + "," + diamondMidHeight + "L" + diamondMidWidth + "," + diamondHeight + "Z");
			diamond.attr("fill", "#000");
		}
		
	}
	
}

function findEnd(id)  {
	for(var i = 0; i < ids.length; i++) {
		
		if(id == ids[i]) return ends[i];
		
	}
	
	return 0;
} 
*/