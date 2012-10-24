var ids = [];
var durations = [];
var absoluteDurations = [];
var names = [];
var predecessors = [];
var starts = [];
var ends = [];
var earliestBegin;
var latestEnd;
var copmpleteDuration;
var maxDuration = 0;
var maxTextWidth = 0;
var maxTextHeight = 0;
var textBoxes = [];
var kinds = [];

function showBurndown() {
	var burndownPaper = Raphael(740, 514, 520, 225);
	var bCanvas = burndownPaper.rect(0,0, 520, 225);
	bCanvas.attr("fill", "#ffa");
	bCanvas.attr("stroke", "#000");
	
	var burndownAsXml = $('#burndownData burndown');
	var points = burndownAsXml.find("points").text();
	var pointText = burndownPaper.text(5, 5, points);
	var deltaXPoints = pointText.getBBox().width/2;
	var deltaYPoints = pointText.getBBox().height/2;
	pointText.translate(deltaXPoints, deltaYPoints);
	
	var sprintBegin = burndownAsXml.find("begin").text();
	var sprintEnd = burndownAsXml.find("end").text();
	var beginText = burndownPaper.text(5,220, sprintBegin);
	var endText = burndownPaper.text(515,220, sprintEnd);
	var deltaXBegin = beginText.getBBox().width/2;
	var deltaYBegin = -1 * beginText.getBBox().height/2;
	var deltaXEnd = -1 * endText.getBBox().width/2;
	beginText.translate(deltaXBegin + 2 * deltaXPoints, deltaYBegin);
	endText.translate(deltaXEnd, deltaYBegin);
	
	var vStartX = 10 + 2 * deltaXPoints;
    var vEndY = 210 + deltaYBegin;
	var vertical = burndownPaper.path("M" + vStartX + ",5L" + vStartX + "," + vEndY);
	
	var hY = 220 + deltaYBegin;
	var horizontal = burndownPaper.path("M" + vStartX + "," + vEndY + "L515," + vEndY);
	
	var duration =  burndownAsXml.find("duration").text();
	var width = 515 - vStartX;
	var stepWidth = width / (5 * duration + 1);
	
	for(var i = 0; i < duration; i++) {
		burndownAsXml.find("item").each(function() {
			var snapshot = $(this);
			var dateNumber = Number(snapshot.children().eq(0).text());
			var remainder = Number(snapshot.children().eq(1).text());
			
			if(dateNumber == i && Number(points) > 0) {
				
				var columnHeight = (vEndY - 5) * remainder/Number(points) ;
				var column = burndownPaper.rect(vStartX + stepWidth * (5 * i + 1), vEndY - columnHeight, 4*stepWidth, columnHeight);
				column.attr("fill", "#bbd");
				column.attr("stroke", "#000");
				
			}
			
		});
		
	}
}

function showReleasePlan() {
	var planPaper = Raphael(192, 514, 520, 225);
	var planCanvas = planPaper.rect(0,0, 520, 225);
	planCanvas.attr("fill", "#ffa");
	planCanvas.attr("stroke", "#000");
	readPlan();
	showGantt(planPaper);
}

function readPlan() {
	var planAsXml = $('#releasePlanData plan');
	
	planAsXml.find("row").each(function() {
		var action = $(this);
		ids.push('x');
		kinds.push(action.children().eq(0).text());
		names.push(action.children().eq(1).text());
		starts.push(action.children().eq(2).text());
		ends.push(action.children().eq(3).text());
		durations.push(action.children().eq(4).text());
		absoluteDurations.push(action.children().eq(5).text());
	});
	
	earliestBegin = planAsXml.find("earliestBegin").text();
	latestEnd = planAsXml.find("latestEnd").text();
	completeDuration = planAsXml.find("completeDuration").text();
}

function showGantt(paper) {
	
	// create text boxes and compute maximal text width and height
	
	for(var i = 0; i < ids.length; i++) {
		var text = paper.text(0,0, names[i]);
		if(text.getBBox().width > maxTextWidth) maxTextWidth = text.getBBox().width;
		if(text.getBBox().height > maxTextHeight) maxTextHeight = text.getBBox().height;
		textBoxes.push(text);
	}
	
	// draw frame and line
	
	var verticalX = Math.ceil(maxTextWidth) + 30;
	var vertical = paper.path("M" + verticalX + ",10L" + verticalX + ",210");
	vertical.attr("stroke-dasharray", ".");
	
	// draw earliest begin and latest end
	
	var showEarliestBegin = paper.text(verticalX, 225, earliestBegin);
	var showLatestEnd = paper.text(520, 225, latestEnd);
	var deltaXEarliestBegin = showEarliestBegin.getBBox().width/2;
	var deltaYEarliestBegin = showEarliestBegin.getBBox().height * (-1);
	showEarliestBegin.translate(deltaXEarliestBegin, deltaYEarliestBegin);
	var deltaXLatestEnd = showLatestEnd.getBBox().width/2 * (-1);
	var deltaYLatestEnd = showLatestEnd.getBBox().height * (-1);
	showLatestEnd.translate(deltaXLatestEnd, deltaYLatestEnd);
	
	
	// move text elements and draw gantt chart
	
	var i = 0;
	
	while(i < ids.length) {
		
		var blockX = 30 + maxTextWidth;
		if(completeDuration) blockX = blockX + (500 - 30 - maxTextWidth) * (absoluteDurations[i] - durations[i]) /completeDuration;
		
		var blockY = 2 * i  * maxTextHeight + 15;
		
		if(blockY + maxTextHeight < 210) {
			textBoxes[i].translate(25 + textBoxes[i].getBBox().width/2, 2 * i * maxTextHeight + 20).toFront();
			
			
			if(kinds[i] == 'sprint') {
				var blockWidth = 0;
				if(completeDuration) blockWidth = (500 - 30 - maxTextWidth) * (durations[i] / completeDuration);
				var block = paper.rect(blockX, blockY, blockWidth, maxTextHeight);
				block.attr("fill", "#bbd");
				block.attr("stroke", "#000");
			}
			else {
				if(completeDuration) blockX = 30 + maxTextWidth + (500 - 30 - maxTextWidth) * (absoluteDurations[i]) /completeDuration;
				
				var diamondMidHeight = blockY + maxTextHeight/2;
				var diamondHeight = blockY + maxTextHeight;
				var diamondMidWidth = blockX + maxTextHeight/2;
				var diamondWidth = blockX + maxTextHeight;
				var diamond = paper.path("M" + blockX + "," + diamondMidHeight + "L" + diamondMidWidth + "," + blockY + "L" + diamondWidth + "," + diamondMidHeight + "L" + diamondMidWidth + "," + diamondHeight + "Z");
				diamond.attr("fill", "#000");
			}
		}
		else {
			textBoxes[i].remove();
		}
		
		i++; 
	} 
}
