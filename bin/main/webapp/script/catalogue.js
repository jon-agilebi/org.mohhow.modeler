var measures = [];
var attributes = [];
var actualText;
var myCompletion;
var active = false;
var markerMeasure = 0;

function initialize(msrs, attrs) {
	measures = msrs;
	attributes = attrs;
}

function getMeasures() {
	return measures;
}

function getAttributes() {
	return attributes;
}

function focusTermEditor(element) {
	actualText = element.val();
	element.bind("keyup", manageCompletion);
}

var manageCompletion = function (evt) {
	var newText = $(this).val();
	var index = findPosition(newText, actualText);
	var textBefore = newText.substring(0, index);
	markerMeasure = countDollar(textBefore);
	var markerAttribute = analyseBrackets(textBefore);
	var indexStart = 0;
	
	if((markerMeasure % 2) == 1 && markerAttribute == 0 && !active) {
		myCompletion = $(this).autocomplete({
			source: measures,
			disabled:false
		}); 
		
		active = true;
		 
	    $(this).autocomplete("search", newText.substring(index, index + 1)); 
	    $(this).one("autocompleteselect", function(event, ui) {
	    	event.preventDefault();
	    	$(this).val(textBefore + ui.item.value);
	    });
	}
	else if ((markerMeasure % 2) == 0 && markerAttribute == 1 && !active) {
		
		
		myCompletion = $(this).autocomplete({
			source: attributes,
			disabled:false
		}); 
		
		active = true;
		
	    $(this).autocomplete("search", newText.substring(index, index + 1)); 
	    $(this).one("autocompleteselect", function(event, ui) {
	    	event.preventDefault();
	    	$(this).val(textBefore + ui.item.value);
	    });
	}
	else if(active && ((markerMeasure % 2) == 0 || markerAttribute != 1)) active = false;
	actualText = newText;
}

function findPosition(left, right) {
	var n = left.length;
	if(right.length < left.length) n = right.length;
	
	for(var i = 0; i < n; i++) {
		if(left[i] != right[i]) return i;
	}
	
	return n;
}

function countDollar(text) {
 var result = 0;
 
 for(var i=0; i < text.length; i++) {
	if(text[i] == "$") result++;
 }
 
 return result;
}

function analyseBrackets(text) {
	
	var result = 0;
	 
	 for(var i=0; i < text.length; i++) {
		if(text[i] == "[" && result == 0) result = 1;
		else if(text[i] == "[" && result == 1) result = -1;
		else if(text[i] == "]" && result == 1) result = 0;
		else if(text[i] == "]" && result == 0) result = -1;
	 }
	 
	 return result;
}

function maxSign(text, sign) {
	
	var result = 0;
	 
	 for(var i=0; i < text.length; i++) {
		if(text[i] == sign) result = i;
	 }
	 
	 return result;
}