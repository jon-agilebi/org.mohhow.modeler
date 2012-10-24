function initializeBlockInformation(){$("#dropResults").data("blockInformation",[])
}function sort(D,C,E){for(var B=0;
B<D.length;
B++){if(D[B]==C&&B>0&&E=="down"){var A=D[B];
D[B]=D[B-1];
D[B-1]=A;
return D
}else{if(D[B]==C&&B<D.length-1&&E=="up"){var A=D[B];
D[B]=D[B+1];
D[B+1]=A;
return D
}}}return D
}function changeOrder(B,C){for(var A=0;
A<B.length;
A++){alert(B[A].name);
if(B[A].name==C){B[A].order=(B[A].order+1)%3;
return B
}}return B
}function removeElement(B,C){for(var A=0;
A<B.length;
A++){if(B[A]==C){B.splice(A,A);
return B
}}return B
}function changeSomeBlock(B,D,A,C){switch(B){case"addMeasure":C.measures.push(D);
break;
case"addAttribute":C.attributes.push({name:D,order:0});
break;
case"up":if(A=="measure"){C.measures=sort(C.measures,D,"up")
}else{C.attributes=sort(C.attributes,D,"up")
}break;
case"down":if(A=="measure"){C.measures=sort(C.measures,D,"down")
}else{C.attributes=sort(C.attributes,D,"down")
}break;
case"changeOrder":changeOrder(C.attributes,D);
break;
case"editFilter":C.filter=D;
break;
case"remove":if(A=="measure"){removeElement(C.measures,D)
}else{removeElement(C.attributes,D)
}break;
default:break
}return C
}function changeBlockInformation(A,E,G,D){var C=false;
var F=$("#dropResults").data("blockInformation");
for(var B=0;
B<F.length;
B++){if(F[B].blockId==A){F[B]=changeSomeBlock(E,G,D,F[B]);
C=true;
break
}}if(!C){var H={blockId:A,measures:[],attributes:[],filter:""};
F.push(changeSomeBlock(E,G,D,H))
}$("#dropResults").data("blockInformation",F)
}function activateSorting(){$(".upButton").live("click",up);
$(".downButton").live("click",down);
$(".removeButton").live("click",removeIt);
$(".orderAttribute").live("click",swapOrdering)
}var up=function(){if($(this).parent().find(".emphasized")){var A=$(this).parent().find(".emphasized");
A.insertBefore(A.prev("li"));
if($(this).is(".measureRelevant")){alert($(this).parent().parent().attr("blockId"));
changeBlockInformation($(this).parent().parent().attr("blockId"),"up",A.text(),"measure")
}if($(this).is(".attributeRelevant")){changeBlockInformation($(this).parent().parent().attr("blockId"),"up",A.text(),"attribute")
}}};
var down=function(){if($(this).parent().find(".emphasized")){var A=$(this).parent().find(".emphasized");
A.insertAfter(A.next("li"));
if($(this).is(".measureRelevant")){changeBlockInformation($(this).parent().parent().attr("blockId"),"down",A.text(),"measure")
}if($(this).is(".attributeRelevant")){changeBlockInformation($(this).parent().parent().attr("blockId"),"down",A.text(),"attribute")
}}};
var swapOrdering=function(){var B=$(this).text();
var A;
if(B=="no order"){A="Ascending"
}else{if(B=="Ascending"){A="Descending"
}else{A="no order"
}}$(this).text(A);
changeBlockInformation($(this).parent().parent().attr("blockId"),"changeOrder",$(this).attr("orderFor"),"measure")
};
var removeIt=function(){if($(this).parent().find(".emphasized")){var A=$(this).parent().find(".emphasized");
if($(this).is(".measureRelevant")){changeBlockInformation($(this).parent().parent().attr("blockId"),"remove",A.text(),"measure")
}if($(this).is(".attributeRelevant")){changeBlockInformation($(this).parent().parent().attr("blockId"),"remove",A.text(),"attribute")
}A.remove()
}};
function appendValue(C,B){var A=C.val();
C.val(A+B)
};