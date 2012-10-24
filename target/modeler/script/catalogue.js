var measures=[];
var attributes=[];
var actualText;
var myCompletion;
var active=false;
var markerMeasure=0;
function initialize(B,A){measures=B;
attributes=A
}function getMeasures(){return measures
}function getAttributes(){return attributes
}function focusTermEditor(A){actualText=A.val();
A.bind("keyup",manageCompletion)
}var manageCompletion=function(A){var F=$(this).val();
var C=findPosition(F,actualText);
var E=F.substring(0,C);
markerMeasure=countDollar(E);
var D=analyseBrackets(E);
var B=0;
if((markerMeasure%2)==1&&D==0&&!active){myCompletion=$(this).autocomplete({source:measures,disabled:false});
active=true;
$(this).autocomplete("search",F.substring(C,C+1));
$(this).one("autocompleteselect",function(G,H){G.preventDefault();
$(this).val(E+H.item.value)
})
}else{if((markerMeasure%2)==0&&D==1&&!active){myCompletion=$(this).autocomplete({source:attributes,disabled:false});
active=true;
$(this).autocomplete("search",F.substring(C,C+1));
$(this).one("autocompleteselect",function(G,H){G.preventDefault();
$(this).val(E+H.item.value)
})
}else{if(active&&((markerMeasure%2)==0||D!=1)){active=false
}}}actualText=F
};
function findPosition(C,B){var D=C.length;
if(B.length<C.length){D=B.length
}for(var A=0;
A<D;
A++){if(C[A]!=B[A]){return A
}}return D
}function countDollar(C){var A=0;
for(var B=0;
B<C.length;
B++){if(C[B]=="$"){A++
}}return A
}function analyseBrackets(C){var A=0;
for(var B=0;
B<C.length;
B++){if(C[B]=="["&&A==0){A=1
}else{if(C[B]=="["&&A==1){A=-1
}else{if(C[B]=="]"&&A==1){A=0
}else{if(C[B]=="]"&&A==0){A=-1
}}}}}return A
}function maxSign(D,B){var A=0;
for(var C=0;
C<D.length;
C++){if(D[C]==B){A=C
}}return A
};