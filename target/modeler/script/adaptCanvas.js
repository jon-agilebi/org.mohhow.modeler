var paper;
var editPaper;
var symbols=[];
var elementTitles=[];
var fourMarker=[];
var frames=[];
var connections=[];
var textToEdit;
var lineToDraw;
var source;
var focus;
var elementToRemove;
var removeIcon=[];
var someoneHasFocus=false;
var hasSource=false;
var superfluousClones=[];
var justALineToRemove=false;
function setSymbolAttributes(A){A.attr("fill","lightgray");
A.attr("stroke","black")
}function setFrameAttributes(A){A.attr("fill","#fff");
A.attr("stroke","#000")
}function updateElements(A,F,D,E){var B;
if(D=="hierarchy"){B=[{cx:16+A,cy:18+F},{cx:8+A,cy:32+F},{cx:24+A,cy:32+F},{path:movePath("M16,18L8,32M16,18L24,32",A,F)}]
}else{if(D=="cube"){B=[{path:movePath("M6,11L14,6L22,11L14,16L6,11L6,23M14,16L14,28M22,11L22,23M6,23L14,28L22,23",A,F)}]
}else{if(D=="attribute"){B=[{path:movePath("M7,20L14,13L21,20L14,27Z",A,F)}]
}else{if(D=="dimension"){B=[{x:6+A,y:9+F},{x:18+A,y:F+4},{x:23+A,y:9+F},{x:4+A,y:23+F},{path:movePath("M6,9L18,5.5M18,9L23,9M6,21L6,23",A,F)}]
}else{if(D=="level"){B=[{path:movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",A,F)},{path:movePath("M12,16L8,24L16,24Z",A,F)},{path:movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",A,F)}]
}else{if(D=="member"){B=[{path:movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",A,F)},{cx:12+A,cy:20+F},{path:movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",A,F)}]
}else{if(D=="scope"){B=[{path:movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",A,F)},{path:movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",A,F)}]
}else{if(D=="context"){B=[{path:movePath("M6,11L14,6L22,11L14,16L6,11L6,23M14,16L14,28M22,11L22,23M6,23L14,28L22,23",A,F)},{path:movePath("M6,6A11,11,0,1,1,20,20",A,F)}]
}}}}}}}}for(var C=0;
C<E.length;
C++){E[C].attr(B[C])
}}function computeElements(G,F,E,B){if(E=="hierarchy"){var A=B.path(movePath("M16,18L8,32M16,18L24,32",G,F));
setSymbolAttributes(A);
var K=B.circle(16+G,18+F,4);
setSymbolAttributes(K);
var J=B.circle(8+G,32+F,4);
setSymbolAttributes(J);
var H=B.circle(24+G,32+F,4);
setSymbolAttributes(H);
return[K,J,H,A]
}else{if(E=="cube"){var I=B.path(movePath("M6,11L14,6L22,11L14,16L6,11L6,23M14,16L14,28M22,11L22,23M6,23L14,28L22,23",G,F));
setSymbolAttributes(I);
return[I]
}else{if(E=="attribute"){var I=B.path(movePath("M7,20L14,13L21,20L14,27Z",G,F));
setSymbolAttributes(I);
return[I]
}else{if(E=="dimension"){var P=B.rect(6+G,9+F,12,12);
setSymbolAttributes(P);
var N=B.rect(18+G,4+F,3,3);
setSymbolAttributes(N);
var M=B.rect(23+G,9+F,3,3);
setSymbolAttributes(M);
var L=B.rect(4+G,23+F,3,3);
setSymbolAttributes(L);
var I=B.path(movePath("M6,9L18,5.5M18,9L23,9M6,21L6,23",G,F));
setSymbolAttributes(I);
return[P,N,M,L,I]
}else{if(E=="level"){var D=B.path(movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",G,F));
D.attr("stroke","black");
D.attr("stroke-width",1.5);
var O=B.path(movePath("M12,16L8,24L16,24Z",G,F));
setSymbolAttributes(O);
var Q=B.path(movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",G,F));
Q.attr("stroke","black");
Q.attr("stroke-width",1.5);
return[D,O,Q]
}else{if(E=="member"){var D=B.path(movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",G,F));
D.attr("stroke","black");
D.attr("stroke-width",1.5);
var C=B.circle(12+G,20+F,4);
setSymbolAttributes(C);
var Q=B.path(movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",G,F));
Q.attr("stroke","black");
Q.attr("stroke-width",1.5);
return[D,C,Q]
}else{if(E=="scope"){var D=B.path(movePath("M6,10T4,12T6,18L4,20L6,22T4,24T6,30",G,F));
D.attr("stroke","black");
D.attr("stroke-width",1.5);
var Q=B.path(movePath("M18,10T20,12T18,18L20,20L18,22T20,24T18,30",G,F));
Q.attr("stroke","black");
Q.attr("stroke-width",1.5);
return[D,Q]
}else{if(E=="context"){var I=B.path(movePath("M6,11L14,6L22,11L14,16L6,11L6,23M14,16L14,28M22,11L22,23M6,23L14,28L22,23",G,F));
setSymbolAttributes(I);
var R=B.path(movePath("M6,6A11,11,0,1,1,20,20",G,F));
R.attr("stroke-dasharry","-");
return[I,R]
}else{return[]
}}}}}}}}}Raphael.el.getStatus=function(){for(var A=0;
A<elementTitles.length;
A++){if(elementTitles[A].frame==this){return elementTitles[A].status
}}return"unknown"
};
Raphael.el.getElementType=function(){for(var A=0;
A<elementTitles.length;
A++){if(elementTitles[A].frame==this){return elementTitles[A].elementType
}}return"unknown"
};
Raphael.el.getVertexId=function(){for(var A=0;
A<elementTitles.length;
A++){if(elementTitles[A].frame==this){return elementTitles[A].vertexId
}}return null
};
Raphael.el.setStatus=function(A){for(var B=0;
B<elementTitles.length;
B++){if(elementTitles[B].frame==this){elementTitles[B].status=A
}}};
Raphael.el.setVertexId=function(A){for(var B=0;
B<elementTitles.length;
B++){if(elementTitles[B].frame==this){elementTitles[B].vertexId=A
}}};
Raphael.el.isInBox=function(A,C){var B=this.getBBox();
return B.x<A&&A<B.x+B.width&&B.y<C&&C<B.y+B.height
};
Raphael.fn.symbol=function(B,D){var A=D.getBBox().x;
var E=D.getBBox().y;
var C=computeElements(Number(A),Number(E),B,this);
return{kind:B,elements:C,frame:D}
};
Raphael.fn.connectionMarker=function(A,E,D){var C={fill:"#00d","fill-opacity":0.4};
var B=this.rect(A,E,4,4);
B.attr(C);
return{marker:B,frame:D}
};
function getModelConnection(K,S){var H=K.getBBox(),G=S.getBBox(),J=[{x:H.x+H.width/2,y:H.y-1},{x:H.x+H.width/2,y:H.y+H.height+1},{x:H.x-1,y:H.y+H.height/2},{x:H.x+H.width+1,y:H.y+H.height/2},{x:G.x+G.width/2,y:G.y-1},{x:G.x+G.width/2,y:G.y+G.height+1},{x:G.x-1,y:G.y+G.height/2},{x:G.x+G.width+1,y:G.y+G.height/2}],R={},I=[];
for(var O=0;
O<4;
O++){for(var M=4;
M<8;
M++){var F=Math.abs(J[O].x-J[M].x),E=Math.abs(J[O].y-J[M].y);
if((O==M-4)||(((O!=3&&M!=6)||J[O].x<J[M].x)&&((O!=2&&M!=7)||J[O].x>J[M].x)&&((O!=0&&M!=5)||J[O].y>J[M].y)&&((O!=1&&M!=4)||J[O].y<J[M].y))){I.push(F+E);
R[I[I.length-1]]=[O,M]
}}}if(I.length==0){var T=[0,4]
}else{T=R[Math.min.apply(Math,I)]
}var Q=J[T[0]].x,D=J[T[0]].y,L=J[T[1]].x,A=J[T[1]].y;
F=Math.max(Math.abs(Q-L)/2,10);
E=Math.max(Math.abs(D-A)/2,10);
var P=[Q,Q,Q-F,Q+F][T[0]].toFixed(3),C=[D-E,D+E,D,D][T[0]].toFixed(3),N=[0,0,0,0,L,L,L-F,L+F][T[1]].toFixed(3),B=[0,0,0,0,D+E,D-E,A,A][T[1]].toFixed(3);
return path=["M",Q.toFixed(3),D.toFixed(3),"C",P,C,N,B,L.toFixed(3),A.toFixed(3)].join(",")
}Raphael.fn.connection=function(A,C){var B=getModelConnection(A,C);
return{line:this.path(B).attr({stroke:"#000",fill:"none"}).hover(markLine,unmarkLine),from:A,to:C}
};
function updateModelConnection(A){var B=getModelConnection(A.from,A.to);
A.line.attr({path:B})
}function moveSymbol(B){var A=B.frame.getBBox().x;
var C=B.frame.getBBox().y;
updateElements(Number(A),Number(C),B.kind,B.elements)
}Raphael.fn.elementTitle=function(F,E,C,D,B){var A=Number(E.getBBox().x);
var H=Number(E.getBBox().y);
var G=this.text(A+60,H+20,F);
G.dblclick(showDialog);
return{element:G,frame:E,status:"prototype",text:F,elementType:C,detail:D,vertexId:B}
};
var showDialog=function(){var K="prototype";
var G;
var B;
var C;
for(var F=0;
F<elementTitles.length;
F++){if(elementTitles[F].element==this){K=elementTitles[F].status;
G=elementTitles[F].elementType;
B=elementTitles[F].detail;
C=elementTitles[F].usage
}}if(K!="prototype"){textToEdit=this;
var N="";
if(!(this.attr("text").substring(0,2)=="<<")){N=this.attr("text")
}var D="<label>Name</label>";
var O="<input type='text' id= 'modalModelTextInput' value='";
var M="' />";
var P="<input type='button' value='Cancel' class='standardButton' style='float: right' onclick='$.unblockUI();' />";
var J="<input type='button' value='Save' class='standardButton' style='float: right' onclick='saveText()' />";
if(G=="attribute"){var I;
var A;
if(B){I="<input type='text' id= 'modalModelAttributePatternInput' value='"+B+"'/>"
}else{I="<input type='text' id= 'modalModelAttributePatternInput' />"
}if(C){A="<input type='text' id= 'modalModelAttributeUsageInput' value='"+C+"'/>"
}else{A="<input type='text' id= 'modalModelAttributeUsageInput' />"
}$.blockUI({message:"<div class='blockDialogue'><table><tr><td><label>Name</label></td><td>"+O+N+M+"</td></tr><tr><td><label>Pattern</label></td><td>"+I+"</td></tr><tr><td><label>Realises</label></td><td>"+A+"</td></tr></table><br />"+P+J+"</div>"})
}else{if(G=="dimension"||G=="cube"){var E;
var H;
if(B&&B.split(";").length==2){var L=B.split(";");
E="<input type='text' id= 'modalModelAttributePatternInput' value='"+L[0]+"'/>";
H="<input type='text' id= 'modalModelAttributeAdditionalPatternInput' value='"+L[1]+"'/>"
}else{E="<input type='text' id= 'modalModelAttributePatternInput' />";
H="<input type='text' id= 'modalModelAttributeAdditionalPatternInput'/>"
}$.blockUI({message:"<div class='blockDialogue'><table><tr><td><label>Name</label></td><td>"+O+N+M+"</td></tr><tr><td><label>Estimated initial size</label></td><td>"+E+"</td></tr><tr><td><label>Estimated growth per load</label></td><td>"+H+"</td></tr></table><br />"+P+J+"</div>"})
}else{$.blockUI({message:"<div class='blockDialogue'>"+D+O+N+M+"<br />"+P+J+"</div>"})
}}}};
function saveText(){var G=null;
var A=0;
for(var D=0;
D<elementTitles.length;
D++){if(elementTitles[D].element==textToEdit){G=elementTitles[D].frame.id;
A=elementTitles[D].vertexId
}}var F=$("#modalModelTextInput").val();
var C=$("#modalModelAttributePatternInput").val();
var E=$("#modalModelAttributeAdditionalPatternInput").val();
var B=$("#modalModelAttributeUsageInput").val();
if(E&&E.length>0){C=C+";"+E
}if(A>0){textToEdit.attr("text",F);
textToEdit.attr("detail",C);
$("#logicalModelVertices v[vertexId='"+A+"'] elementName").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] elementName").append(F);
$("#logicalModelVertices v[vertexId='"+A+"'] detail").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] detail").append(C);
$("#logicalModelVertices v[vertexId='"+A+"'] usage").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] usage").append(B);
$("#logicalModelVertices v[vertexId='"+A+"'] status").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] status").append("changed");
$.unblockUI()
}else{if(G){textToEdit.attr("text",F);
textToEdit.attr("detail",C);
$("#logicalModelVertices v[modelId='"+G+"'] elementName").empty();
$("#logicalModelVertices v[modelId='"+G+"'] elementName").append(F);
$("#logicalModelVertices v[modelId='"+G+"'] detail").empty();
$("#logicalModelVertices v[modelId='"+G+"'] detail").append(C);
$("#logicalModelVertices v[vertexId='"+A+"'] usage").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] usage").append(B);
$("#logicalModelVertices v[modelId='"+G+"'] status").empty();
$("#logicalModelVertices v[modelId='"+G+"'] status").append("changed");
$.unblockUI()
}}}function moveTitle(C){var A=C.frame.getBBox().x;
var D=C.frame.getBBox().y;
var B={x:Number(A)+60,y:Number(D)+20};
C.element.attr(B)
}window.onload=function(){paper=Raphael(460,80,800,650);
createCanvas()
};
function movePath(H,I,G){var E=String(H);
var C=E.trim().split(/(\D)/);
var A=0;
var F;
for(var D=0;
D<C.length;
D++){if(C[D]==","){A++
}else{if(C[D].match(/\d+/)){var B=Number(C[D]);
if(F!="A"){if(A==0){C[D]=B+Number(I)
}else{C[D]=B+Number(G)
}}else{if(A==5){C[D]=B+Number(I)
}else{if(A==6){C[D]=B+Number(G)
}}}}else{if(C[D].match(/[MTLA]/)){F=C[D];
A=0
}}}}return C.join("")
}function createCanvas(){var A=paper.rect(0,0,800,650);
A.attr("fill","#ffa");
A.attr("stroke","#000")
}function createEditCanvas(){paper.clear();
editPaper=Raphael(180,60,1096,684);
var B=editPaper.rect(0,0,1096,684);
B.attr("fill","#ffa");
B.attr("stroke","#000");
var A=editPaper.rect(5,5,274,674);
A.attr("fill","#ffd");
A.attr("stroke","#000")
}var startMove=function(){if(this.getStatus()=="prototype"){var C=this.clone();
C.drag(move,startMove,stopMove);
for(var A=0;
A<symbols.length;
A++){if(symbols[A].frame==this){symbols.push(editPaper.symbol(symbols[A].kind,C))
}}for(var A=0;
A<elementTitles.length;
A++){if(elementTitles[A].frame==this){var B=editPaper.elementTitle(elementTitles[A].text,C,elementTitles[A].elementType,null,"m"+this.id);
elementTitles.push(B)
}}}if(this.type=="path"){this.opath=this.attr("path")
}else{this.ox=this.attr("x");
this.oy=this.attr("y")
}};
var move=function(D,A){removeConnectionMarker();
if(this.type=="path"){var C={path:movePath(this.opath,D,A)};
this.attr(C)
}else{var C={x:Number(this.ox)+Number(D),y:Number(this.oy)+Number(A)};
this.attr(C)
}for(var B=0;
B<connections.length;
B++){updateModelConnection(connections[B])
}for(var E=0;
E<elementTitles.length;
E++){if(elementTitles[E].frame==this){moveTitle(elementTitles[E])
}}for(var F=0;
F<symbols.length;
F++){if(symbols[F].frame==this){moveSymbol(symbols[F])
}}};
function determineModelOrVertexId(E,C){var D=-1;
var A=-1;
for(var B=0;
B<elementTitles.length;
B++){if(elementTitles[B].frame==E){A=elementTitles[B].vertexId;
D=elementTitles[B].modelId;
break
}}if(C){return A
}else{return D
}}var stopMove=function(){var B=this.getBBox().x;
var E=this.getBBox().y;
var A=-1;
var D=-1;
if(B<280){this.hide()
}else{for(var C=0;
C<elementTitles.length;
C++){if(elementTitles[C].frame==this){A=elementTitles[C].vertexId;
D=elementTitles[C].modelId;
break
}}if(this.getStatus()=="prototype"){this.setStatus("initial");
this.setVertexId("m"+this.id);
this.hover(getFocus,leaveFocus);
frames.push(this);
$("#logicalModelVertices").append("<v modelId='"+this.id+"' vertexId='m"+this.id+"'><elementType>"+this.getElementType()+"</elementType><elementName></elementName><x>"+B+"</x><y>"+E+"</y><detail></detail><scale>1</scale><usage>0</usage><status>new</status></v>")
}else{if(D>0){$("#logicalModelVertices v[modelId='"+this.id+"'] x").empty();
$("#logicalModelVertices v[modelId='"+this.id+"'] x").append(B);
$("#logicalModelVertices v[modelId='"+this.id+"'] y").empty();
$("#logicalModelVertices v[modelId='"+this.id+"'] y").append(E);
$("#logicalModelVertices v[modelId='"+this.id+"'] status").empty();
$("#logicalModelVertices v[modelId='"+this.id+"'] status").append("changed")
}else{$("#logicalModelVertices v[vertexId='"+A+"'] x").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] x").append(B);
$("#logicalModelVertices v[vertexId='"+A+"'] y").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] y").append(E);
$("#logicalModelVertices v[vertexId='"+A+"'] status").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] status").append("changed")
}}}};
var getFocus=function(){if(!hasSource){this.animate({fill:"#f23","fill-opacity":0.2},500);
removeConnectionMarker();
createConnectionMarker(this);
someoneHasFocus=true;
focus=this;
justALineToRemove=false
}};
var drawLine=function(D,B){var C={x:this.ox+D,y:this.oy+B};
this.attr(C);
var F=this.ox+2;
var E=this.oy+2;
var G=this.ox+D;
var A=this.oy+B;
lineToDraw.attr("path","M"+F+","+E+"L"+G+","+A)
};
var finishLineDrawing=function(){lineToDraw.remove();
var A=this.getBBox().x+2;
var D=this.getBBox().y+2;
if(hasSource){for(var B=0;
B<frames.length;
B++){if(frames[B].isInBox(A,D)){var C=editPaper.connection(source,frames[B]);
connections.push(C);
$("#logicalModelEdges").append("<e modelId='"+C.line.id+"' edgeId='m"+C.line.id+"'><h>"+source.getVertexId()+"</h><t>"+frames[B].getVertexId()+"</t><status>new</status></e>")
}}}hasSource=false
};
var initConnection=function(){var D=this.clone();
D.drag(drawLine,initConnection,finishLineDrawing);
superfluousClones.push(D);
this.ox=this.attr("x");
this.oy=this.attr("y");
this.toFront();
var C=this.ox+2;
var B=this.oy+2;
lineToDraw=editPaper.path("M"+C+","+B+"L"+C+","+B);
for(var A=0;
A<fourMarker.length;
A++){if(fourMarker[A].marker==this){source=fourMarker[A].frame;
hasSource=true
}}};
function createConnectionMarker(C){var I=C.getBBox().x;
var H=C.getBBox().y;
var B=C.getBBox().width;
var J=C.getBBox().height;
var F=editPaper.connectionMarker(I-2,H+J/2-2,C);
fourMarker.push(F);
var E=editPaper.connectionMarker(I+B/2-2,H+J-2,C);
fourMarker.push(E);
var D=editPaper.connectionMarker(I+B/2,H-2,C);
fourMarker.push(D);
var A=editPaper.connectionMarker(I+B-2,H+J/2-2,C);
fourMarker.push(A);
for(var G=0;
G<fourMarker.length;
G++){fourMarker[G].marker.drag(drawLine,initConnection,finishLineDrawing)
}createRemoveIcon(I+B-8,H+8,C)
}function removeConnectionMarker(){while(fourMarker.length>0){var C=fourMarker.pop();
C.marker.remove()
}while(removeIcon.length>0){var B=removeIcon.pop();
B.remove()
}while(superfluousClones.length>0){var A=superfluousClones.pop();
A.remove()
}}var leaveFocus=function(){this.animate({fill:"#fff","fill-opacity":1},500);
someoneHasFocus=false
};
var removeElement=function(){var B="<input type='button' value='Cancel' class='standardButton' onclick='$.unblockUI();' />";
var A="<input type='button' value='OK' class='standardButton' onclick='removeFinally()' />";
$.blockUI({message:"<div>Really wanna destroy?<br />"+B+A+"</div>"})
};
function removeFinally(){removeConnectionMarker();
if(!justALineToRemove){var D=determineModelOrVertexId(elementToRemove,false);
var A=determineModelOrVertexId(elementToRemove,true);
if(D>0){$("#logicalModelVertices v[modelId='"+D+"'] status").empty();
$("#logicalModelVertices v[modelId='"+D+"'] status").append("removed")
}else{$("#logicalModelVertices v[vertexId='"+A+"'] status").empty();
$("#logicalModelVertices v[vertexId='"+A+"'] status").append("removed")
}for(var C=0;
C<connections.length;
C++){if(connections[C].from==elementToRemove||connections[C].to==elementToRemove){$("#logicalModelEdges e[modelId='"+connections[C].line.id+"'] status").empty();
$("#logicalModelEdges e[modelId='"+connections[C].line.id+"'] status").append("removed");
connections[C].line.remove()
}}for(var C=0;
C<elementTitles.length;
C++){if(elementTitles[C].frame==elementToRemove){elementTitles[C].element.remove()
}}for(var C=0;
C<symbols.length;
C++){if(symbols[C].frame==elementToRemove){for(var B=0;
B<symbols[C].elements.length;
B++){symbols[C].elements[B].remove()
}}}}elementToRemove.remove();
$.unblockUI()
}function createRemoveIcon(H,E,D){var A=editPaper.circle(H,E,8);
A.attr("fill","#f00");
A.attr("stroke","#fff");
A.attr("stroke-width",2);
removeIcon.push(A);
var C=H-6,G=E-6,B=H+6,F=E+6;
var I=editPaper.path("M"+C+","+G+"L"+B+","+F+"M"+C+","+F+"L"+B+","+G);
I.attr("stroke-width",2);
I.attr("stroke","#fff");
removeIcon.push(I);
A.dblclick(removeElement);
I.dblclick(removeElement);
elementToRemove=D
}var markLine=function(){removeConnectionMarker();
var A=this.getPointAtLength(1);
createRemoveIcon(A.x,A.y,this);
justALineToRemove=true
};
var unmarkLine=function(){};
function drawModel(A){elementTitles=[];
frames=[];
symbols=[];
if(A){paper.clear();
createCanvas()
}else{createEditCanvas()
}$("#logicalModelVertices v").each(function(){var D=$(this).attr("vertexId");
var E=$(this).find("elementType").text();
var C=$(this).find("elementName").text();
var F=$(this).find("detail").text();
var B=$(this).find("x").text();
var G=$(this).find("y").text();
drawElement(E,A,Number(B),Number(G),C,D,F)
});
$("#logicalModelEdges e").each(function(E){var B=$(this).find("h").text();
var F=$(this).find("t").text();
var D=getVertexById(B);
var C=getVertexById(F);
if(D&&C){if(A){paper.connection(D,C)
}else{connections.push(editPaper.connection(D,C))
}}})
}function getVertexById(A){for(var B=0;
B<elementTitles.length;
B++){if(elementTitles[B].vertexId==A){return elementTitles[B].frame
}}}function drawFrame(C,G,B,H,D,A,E,F){if(C){if(G){return paper.path(F)
}else{return paper.rect(B,H,D,A,E)
}}else{if(G){return editPaper.path(F)
}else{return editPaper.rect(B,H,D,A,E)
}}}function drawElement(E,D,C,F,B,A){drawElement(E,D,C,F,B,A,null)
}function drawElement(C,D,H,G,J,F,E){var B;
var A,I;
if(D){A=H-280;
I=G-20
}else{A=H;
I=G
}if(C=="hierarchy"){B=drawFrame(D,true,0,0,0,0,0,movePath("M0,12L24,0L96,0L120,12L120,40L0,40Z",A,I))
}if(C=="level"||C=="member"||C=="scope"||C=="dimension"){B=drawFrame(D,false,A,I,120,40,10,"")
}if(C=="attribute"){B=drawFrame(D,true,0,0,0,0,0,movePath("M12,0L120,0L120,40L12,40L0,30L0,10Z",A,I))
}if(C=="cube"||C=="context"){B=drawFrame(D,true,0,0,0,0,0,movePath("M0,0L120,0L120,80L0,80ZM0,30L120,30",A,I))
}setFrameAttributes(B);
frames.push(B);
if(D){symbols.push(paper.symbol(C,B));
elementTitles.push(paper.elementTitle(J,B,C,E,F))
}else{symbols.push(editPaper.symbol(C,B));
elementTitles.push(editPaper.elementTitle(J,B,C,E,F));
B.drag(move,startMove,stopMove);
if(B.getVertexId()>=0||(B.getVertexId().length>0&&B.getVertexId().substring(0,1)=="m")){B.setStatus("sync");
B.hover(getFocus,leaveFocus)
}}}function createPalette(A){if(A=="dimension"){drawElement("hierarchy",false,83,40,"<<hierarchy>>",-1);
drawElement("level",false,83,90,"<<level>>",-1);
drawElement("member",false,83,140,"<<member>>",-1);
drawElement("attribute",false,83,190,"<<attribute>>",-1);
drawElement("scope",false,83,240,"<<scope>>",-1)
}else{drawElement("cube",false,83,40,"<<cube>>",-1);
drawElement("dimension",false,83,130,"<<dimension>>",-1);
drawElement("level",false,83,180,"<<level>>",-1)
}};