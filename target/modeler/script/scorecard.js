var designPaper;
var paper;
var background;
var lineToDraw;
var isVertical;
var lineOrigin;
var orientation="portrait";
var blocks=[];
var scorecardBlocks=[];
var modelBlocks=[];
var drillBlocks=[];
var lines=[];
var mainLine;
var originalBlock;
var selectedBlock;
var backgrounds=[];
var portraitFrame;
var landscapeFrame;
var jsonBlock;
var presentationType;
var presentationDetail;
var blockAttributeNames=[];
var blockAttributeValues=[];
var title;
var blockId;
var countAttributesInBlock=0;
var countMeasuresInBlock=0;
var scrollPosition=0;
var scrollStart=0;
var scrollEnd=0;
var scorecardStandardText;
var scorecardTitle;
var scorecardFrame;
var scorecardHorizontal;
var scorecardVertical;
function initializeBlocks(B,A){portraitFrame=B;
jsonBlock=A
}function drawSinglePie(J,M,L,R,Q){var A=[];
var H=M+R/2;
var F=L+Q/2;
var O=R/4;
if(Q<R){O=Q/4
}var G=F-O;
var U=H+O/Math.sqrt(2);
var E=F+O/Math.sqrt(2);
var T=H-O;
var D=F;
var N=O+R/10;
var K=H+N/Math.sqrt(2);
var S=F+N/Math.sqrt(2);
var V=H-N;
var I=F;
var P=J.circle(M+R/2,L+Q/2,R/4);
A.push(P);
var C=J.path("M"+H+", "+F+"L"+H+", "+G+"A"+O+","+O+",0,0,1,"+U+","+E+"L"+H+" "+F);
C.attr("fill","#aaa");
A.push(C);
var B=J.path("M"+H+", "+F+"L"+U+", "+E+"A"+O+","+O+",0,0,1,"+T+","+D+"L"+H+" "+F);
B.attr("fill","#ccc");
A.push(B);
return A
}function drawPie(I,J,F,E,A,H){var B=[];
if(J=="mirror"){B=drawSinglePie(I,F,E+H/3,A/3,H/3);
var D=["A","B","C"];
for(var C=0;
C<D.length;
C++){var G=I.text(F+A/2,E+(C+1)*H/4,D[C]);
B.push(G)
}pie2=drawSinglePie(I,F+2*A/3,E+H/3,A/3,H/3);
for(var C=0;
C<pie2.length;
C++){B.push(pie2[C])
}}else{if(J=="separated"){B=drawSinglePie(I,F,E+H/10,A/3,H/3);
pie2=drawSinglePie(I,F+0.7*A,E+H/10,A/3,H/3);
for(var C=0;
C<pie2.length;
C++){B.push(pie2[C])
}pie3=drawSinglePie(I,F,E+0.6*H,A/3,H/3);
for(var C=0;
C<pie3.length;
C++){B.push(pie3[C])
}pie4=drawSinglePie(I,F+0.7*A,E+0.6*H,A/3,H/3);
for(var C=0;
C<pie4.length;
C++){B.push(pie4[C])
}}else{B=drawSinglePie(I,F,E,A,H)
}}return B
}function drawBar(E,I,G,F,K,J){var V=[];
var T,P,O,L;
var M=["A","B","C","D"];
if(I=="deviation"||I=="sliding"){T=G+K/2
}else{T=G+K/3
}var C=F+J/10;
var B=F+0.9*J;
var U=(B-C)/21;
V.push(E.path("M"+T+","+C+"L"+T+", "+B));
if(I=="paired"){var D=G+2*K/3;
V.push(E.path("M"+D+","+C+"L"+D+", "+B))
}for(var N=0;
N<4;
N++){if(I=="range"){P=T+Math.random()*(G+0.9*K-T);
O=P+Math.random()*(G+0.9*K-P)
}else{if(I=="deviation"&&N>1){P=T-Math.random()*(G+0.9*K-T)
}else{if(I=="paired"){P=G+2*K/3
}else{P=T
}}}if(I=="deviation"&&N>1){O=T-P
}else{O=Math.random()*(G+0.9*K-P)
}var S=E.rect(P,C+(1+5*N)*U,O,4*U);
if(I=="grouped"){var W=E.rect(P,C+(3+5*N)*U,0.8*O,4*U);
W.attr("fill","#000")
}else{if(I=="sliding"||I=="paired"){var R=T-Math.random()*(G+0.9*K-T);
if(I=="paired"){R=T-Math.random()*K/3
}var W=E.rect(R,C+(1+5*N)*U,T-R,4*U);
W.attr("fill","#ddd");
V.push(W);
S.attr("fill","#000")
}else{if(I=="subdivided"){var A=Math.random()*O;
var W=E.rect(P,C+(1+5*N)*U,A,4*U);
W.attr("fill","#000")
}else{if(N==3){S.attr("fill","#000")
}else{S.attr("fill","#ddd")
}}}}V.push(S);
if(I!="sliding"){var Q;
if(I=="deviation"&&N>1){Q=T+K/6
}else{if(I=="paired"){Q=G+K/2
}else{Q=G+K/6
}}var H=E.text(Q,C+(3+5*N)*U,M[N]);
V.push(H)
}}return V
}function drawColumn(I,O,M,L,Q,P){var J=[];
var D;
var C,B;
var G=[1,5,9,13];
var H=[1,6,11,16];
var E=[2,6,12,16];
var K;
var F;
if(O=="deviation"){C=L+P/2
}else{C=L+0.9*P
}var T=M+Q/10;
var S=M+0.9*Q;
J.push(I.path("M"+T+","+C+"L"+S+", "+C));
if(O=="step"){K=G;
F=(S-T)/18
}else{if(O=="grouped"){K=E;
F=(S-T)/22
}else{K=H;
F=(S-T)/21
}}for(var R=0;
R<4;
R++){if(O=="range"){B=(1-Math.random())*C
}else{B=C
}y3=P/10+(1-Math.random())*(B-P/10);
if(O=="deviation"&&R>1){D=I.rect(T+K[R]*F,B,4*F,y3)
}else{D=I.rect(T+K[R]*F,y3,4*F,B-y3)
}if(O=="range"||(O=="deviation"&&R>1)||(O=="grouped"&&R%2==0)){D.attr("fill","#000")
}else{D.attr("fill","#aaa")
}if(O=="subdivided"){var A=y3+Math.random()*(B-y3);
var N=I.rect(T+K[R]*F,A,4*F,B-A);
N.attr("fill","#000");
J.push(N)
}J.push(D)
}return J
}function drawCorrelation(K,L,I,G,B,J){var H=[];
var E;
if(B>J){E=J/20
}else{E=B/20
}for(var D=1;
D<9;
D++){var F=I+2*E+E*16*Math.random();
var C=G+2*E+E*16*Math.random();
var A=K.circle(F,C,E);
if(D==5){A.attr("fill","#f00")
}else{A.attr("fill","#000")
}H.push(A)
}return H
}function drawLineChart(F,M,K,J,P,N){var G=[];
var R=(P-8)/4;
var A=(N-28)/4;
var I=(P-8)/20;
var V=K+4;
var U=K+P-4;
var D=J+N/10;
var C=J+0.9*N;
var O;
G.push(F.rect(V,D,P-8,0.8*N));
for(var T=1;
T<5;
T++){var B=D+T*A;
var S=V+T*R;
G.push(F.path("M"+V+","+B+"L"+U+", "+B));
G.push(F.path("M"+S+","+D+"L"+S+", "+C))
}var H=D+Math.random()*(N-28);
O="M"+V+", "+D;
for(var T=1;
T<21;
T++){var L=V+T*I;
var Q=D+Math.random()*(N-28);
O=O+"L"+L+", "+Q
}var E=F.path(O);
E.attr("stroke-width",2);
G.push(E);
return G
}function drawTable(E,J,H,G,M,K){var I=[];
var N=(M-8)/4;
var A=(K-28)/4;
var F=(M-8)/20;
var R=H+4;
var Q=H+M-4;
var D=G+K/10;
var C=G+0.9*K;
var L;
I.push(E.rect(R,D,M-8,0.8*K));
for(var P=1;
P<5;
P++){var B=D+P*A;
var O=R+P*N;
I.push(E.path("M"+R+","+B+"L"+Q+", "+B));
I.push(E.path("M"+O+","+D+"L"+O+", "+C))
}return I
}function drawIndicator(H,L,K,J,O,N){var P=[];
var I;
if(O>N){I=N/8
}else{I=O/8
}if(L=="circle"||L=="circleAndArrow"){var G=H.circle(K+O/2,J+N/2,I*3.5);
if(L=="circle"){G.attr("fill","#ff0")
}else{G.attr("fill","#aaa")
}P.push(G)
}if(L=="square"){var M=H.rect(K,J,O,N);
M.attr("fill","#f00");
P.push(M)
}else{if(L=="arrow"||L=="circleAndArrow"){var S=K+I;
var F=J+N/2-I;
var R=K+O-3*I;
var D=J+N/2-2*I;
var Q=K+O-I;
var C=J+N/2;
var B=J+N/2+2*I;
var A=J+N/2+I;
var E=H.path("M"+S+","+F+"L"+R+","+F+"L"+R+","+D+"L"+Q+","+C+"L"+R+","+B+"L"+R+","+A+"L"+S+","+A+"Z");
E.attr("fill","#000");
P.push(E)
}}return P
}function getPadBox(){if(orientation=="portrait"){return[356,91,384,502]
}else{return[297,150,502,384]
}}function showDesignCanvas(){designPaper=Raphael(180,60,1096,684);
var D=designPaper.rect(0,0,1096,684);
D.attr("fill","#ffa");
D.attr("stroke","#000");
var A=designPaper.rect(5,5,274,674);
A.attr("fill","#ffd");
A.attr("stroke","#000");
A.drag(scroll,startScrolling,stopScrolling);
var B=designPaper.rect(817,5,274,674);
B.attr("fill","#ffd");
B.attr("stroke","#000");
designBackground=designPaper.rect(346,81,404,522);
designBackground.attr("fill","#000");
designBackground.attr("stroke","#000");
designBackground.drag(drawLine,startLineDrawing,stopLineDrawing);
designCanvas=designPaper.rect(356,91,384,502);
designCanvas.attr("fill","#eee");
designCanvas.attr("stroke","#eee");
mainLine=parse($("#designBlockInformation design fr[orientation='portrait'] *:nth-child(1)"));
$("#designBlockInformation design block").each(function(){if($(this).is("block")){parseBlock($(this))
}});
for(var C=0;
C<modelBlocks.length;
C++){blocks.push(designPaper.block(modelBlocks[C].presentationType,modelBlocks[C].presentationDetail,52,12+C*192,180,180,modelBlocks[C].attributes,modelBlocks[C].attributes,modelBlocks[C].title,null,true,modelBlocks[C].blockId))
}drawIt(getPadBox(),mainLine)
}function removeBlock(A){A.background.remove();
A.title.remove();
while(A.drawing.length>0){A.drawing.pop().remove()
}}function emptyPad(){while(lines.length>0){lines.pop().remove()
}while(scorecardBlocks.length>0){removeBlock(scorecardBlocks.pop())
}}function clean(){$("#designBlockInformation fr[orientation='"+orientation+"']").empty();
emptyPad()
}function rotateDesign(){designBackground.rotate(90);
designCanvas.rotate(90);
if(orientation=="portrait"){orientation="landscape"
}else{orientation="portrait"
}emptyPad();
mainLine=parse($("#designBlockInformation fr[orientation='"+orientation+"'] *:nth-child(1)"));
drawIt(getPadBox(),mainLine)
}Raphael.fn.block=function(M,O,K,I,C,L,E,B,J,A,N,D){var G=designPaper.rect(K,I,C,L);
G.attr("fill","#fff");
if(N){G.drag(moveBlock,dragBlock,dropBlock)
}else{G.click(chooseBlock)
}var F=designPaper.text(K,I,J);
F.attr({font:"Arial","font-size":16});
F.translate(C/2,12);
var H=[];
if(M=="pie"){H=drawPie(designPaper,O,K,I+12,C,L-12)
}else{if(M=="bar"){H=drawBar(designPaper,O,K,I+12,C,L-12)
}else{if(M=="line"){H=drawLineChart(designPaper,O,K,I+12,C,L-12)
}else{if(M=="column"){H=drawBar(designPaper,O,K,I+12,C,L-12)
}else{if(M=="indicator"){H=drawIndicator(designPaper,O,K,I+12,C,L-12)
}else{H=drawTable(designPaper,O,K,I+12,C,L-12)
}}}}}return{background:G,title:F,drawing:H,status:"prototype",presentationType:M,presentationDetail:O,attributes:E,measures:B,titlePosition:A,blockId:D}
};
var scroll=function(B,A){scrollEnd=A
};
var startScrolling=function(){scrollStart=this.attr("y")
};
var stopScrolling=function(){var A=Number(scrollEnd)+Number(scrollPosition);
if(A>674){scrollPosition=674
}else{if(A<0){scrollPosition=0
}else{scrollPosition=A
}}alert(scrollPosition)
};
function parseBlock(H){var A=H.attr("blockId");
var F=H.find("presentationType").text();
var D=H.find("presentationDetail").text();
var G=H.find("title").text();
var C=[];
H.filter("attribute name").each(function(){C.push($(this).text())
});
var E=[];
H.filter("measure").each(function(){E.push($(this).text())
});
var B=[];
modelBlocks.push(designPaper.modelBlock(A,F,D,G,null,null,C,E,B))
}Raphael.fn.modelBlock=function(A,H,I,E,B,C,F,G,D){return{blockId:A,presentationType:H,presentationDetail:I,title:E,showTitle:B,columnCount:C,attrs:F,msrs:G,prms:D}
};
function parse(E){var D,A,B,F,C;
if(E.is("column")){A=E.attr("width");
D="column"
}else{if(E.is("row")){A=E.attr("height");
D="row"
}else{if(E.is("block")){D="block";
B=E.attr("ref")
}}}if(E.children().size()==2){if(!E.children().eq(0).is("empty")){F=parse(E.children().eq(0))
}if(!E.children().eq(1).is("empty")){C=parse(E.children().eq(1))
}}return designPaper.scLine(D,B,A,F,C)
}var dragBlock=function(){this.ox=this.attr("x");
this.oy=this.attr("y");
for(var B=0;
B<blocks.length;
B++){if(blocks[B].background==this){var C=blocks[B].background.getBBox();
originalBlock=designPaper.block(blocks[B].presentationType,blocks[B].presentationDetail,C.x,C.y,C.width,C.height,blocks[B].attributes,blocks[B].measures,blocks[B].title.attr("text"),blocks[B].titlePosition,false,blocks[B].blockId);
blocks.push(originalBlock);
this.toFront();
blocks[B].title.toFront();
for(var A=0;
A<blocks[B].drawing.length;
A++){blocks[B].drawing[A].toFront()
}}}};
function moveTitle(D,C,B){var A={x:C,y:B};
D.attr(A)
}function movePath(H,I,G){var E=String(H);
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
}function moveDrawing(G,C,A){var B;
for(var D=0;
D<G.length;
D++){var E=originalBlock.drawing[D];
var F=E.attr("path");
if(F){B={path:movePath(F,C,A)};
G[D].attr(B)
}if(E.attr("x")){var B={x:Number(E.attr("x"))+Number(C),y:Number(E.attr("y"))+Number(A)};
G[D].attr(B)
}if(E.attr("cx")){var B={cx:Number(E.attr("cx"))+Number(C),cy:Number(E.attr("cy"))+Number(A)};
G[D].attr(B)
}}}var moveBlock=function(C,A){var F=Number(this.ox)+Number(C);
var E=Number(this.oy)+Number(A);
var B={x:F,y:E};
this.attr(B);
for(var D=0;
D<blocks.length;
D++){if(blocks[D].background==this){moveDrawing(blocks[D].drawing,C,A);
moveTitle(blocks[D].title,F,E)
}}};
var dropBlock=function(){var G=this.getBBox();
var A=G.x+G.width/2;
var F=G.y+G.height/2;
for(var E=0;
E<blocks.length;
E++){if(blocks[E].background==this){if(A>817){if(selectedBlock){var D=blockToModelBlock(blocks[E]);
var C;
if(drillBlocks.length>0){C=drillBlocks[drillBlocks.length-1].blockId
}else{C=selectedBlock.blockId
}$("#designBlockInformation successors").append("<successor blockId='"+C+"' successorId='"+D.blockId+"' />");
drillBlocks.push(designPaper.block(D.presentationType,D.presentationDetail,875,12+drillBlocks.length*192,180,180,D.attributes,D.attributes,D.title,null,true,D.blockId))
}removeBlock(blocks[E])
}else{if(!mainLine.kind){var G=getPadBox();
if(G[0]<=A&&G[0]+G[2]>=A&&G[1]<=F&&G[1]+G[3]>=F){mainLine=designPaper.scLine("block",blocks[E].blockId,0,null,null);
modelBlocks.push(blockToModelBlock(blocks[E]))
}}else{var B=computeAreas(mainLine,getPadBox());
mainLine=considerBlockInLine(blocks[E],mainLine,B[0],B[1]);
modelBlocks.push(blockToModelBlock(blocks[E]))
}emptyPad();
drawIt(getPadBox(),mainLine);
removeBlock(blocks[E]);
serializeDesign()
}break
}}};
function blockToModelBlock(A){return designPaper.modelBlock(A.blockId,A.presentationType,A.presentationDetail,A.title.attr("text"))
}function addToDrillStack(C){var B=$("#designBlockInformation successor[blockId='"+C.blockId+"']").attr("successorId");
if(B){for(var A=0;
A<modelBlocks.length;
A++){if(modelBlocks[A].blockId==B){drillBlocks.push(designPaper.block(modelBlocks[A].presentationType,modelBlocks[A].presentationDetail,875,12+drillBlocks.length*192,180,180,modelBlocks[A].attributes,modelBlocks[A].attributes,modelBlocks[A].title,null,false,modelBlocks[A].blockId))
}}}}function createDrillStack(A){while(drillBlocks.length>0){removeBlock(drillBlocks.pop())
}addToDrillStack(A)
}var chooseBlock=function(){for(var A=0;
A<scorecardBlocks.length;
A++){var B=scorecardBlocks[A];
if(B.background!=this){B.background.animate({fill:"#fff","fill-opacity":1},500)
}}this.animate({fill:"#66f","fill-opacity":0.2},500);
selectedBlock=B;
createDrillStack(selectedBlock)
};
function computeAreas(A,B){if(!A){return[[B[0],B[1],B[2],B[3]],[0,0,0,0]]
}else{var C=A.width/1000;
if(A.kind=="column"){return[[B[0],B[1],C*B[2],B[3]],[B[0]+C*B[2],B[1],(1-C)*B[2],B[3]]]
}else{return[[B[0],B[1],B[2],C*B[3]],[B[0],B[1]+C*B[3],B[2],(1-C)*B[3]]]
}}}function considerBlockInLine(D,J,E,C){var F=D.background.getBBox();
var B=F.x+F.width/2;
var H=F.y+F.height/2;
var G=false;
var I=false;
if(E[0]<B&&E[1]<H&&E[0]+E[2]>B&&E[1]+E[3]>H){G=true
}if(C[0]<B&&C[1]<H&&C[0]+C[2]>B&&C[1]+C[3]>H){I=true
}if(G&&!J.first){J.first=designPaper.scLine("block",D.blockId,0,null,null)
}else{if(G){var A=computeAreas(J,E);
J.first=considerBlockInLine(D,J.first,A[0],A[1])
}}if(I&&!J.second){J.second=designPaper.scLine("block",D.blockId,0,null,null)
}else{if(I){var A=computeAreas(J,C);
J.second=considerBlockInLine(D,J.second,A[0],A[1])
}}return J
}Raphael.fn.scLine=function(D,A,C,E,B){return{kind:D,blockId:A,width:C,first:E,second:B}
};
function drawIt(H,E){var C,B,J,I;
var K;
var D={stroke:"#111","stroke-dasharray":".","stroke-width":2};
if(E!=null){if(E.kind=="column"){var G=H[2]*E.width/1000;
C=H[0]+G;
B=C;
J=H[1];
I=H[1]+H[3];
K=designPaper.path("M"+C+","+J+"L"+B+","+I);
K.attr(D);
if(E.first){drawIt([H[0],H[1],G,H[3]],E.first)
}if(E.second){drawIt([C,H[1],H[2]-G,H[3]],E.second)
}lines.push(K)
}else{if(E.kind=="row"){var A=H[3]*E.width/1000;
C=H[0];
B=H[0]+H[2];
J=H[1]+A;
I=J;
K=designPaper.path("M"+C+","+J+"L"+B+","+I);
K.attr(D);
if(E.first){drawIt([H[0],H[1],H[2],A],E.first)
}if(E.second){drawIt([H[0],J,H[2],H[3]-A],E.second)
}lines.push(K)
}else{if(E.kind=="block"){for(var F=0;
F<modelBlocks.length;
F++){if(modelBlocks[F].blockId==E.blockId){scorecardBlocks.push(designPaper.block(modelBlocks[F].presentationType,modelBlocks[F].presentationDetail,H[0]+2,H[1]+2,H[2]-4,H[3]-4,modelBlocks[F].attributes,modelBlocks[F].attributes,modelBlocks[F].title,null,false,modelBlocks[F].blockId))
}}}}}}}var drawLine=function(C,B){var E=this.ox;
var D=this.oy;
var F=this.ox+C;
var A=this.oy+B;
if(isVertical){lineToDraw.attr("path","M"+F+","+D+"L"+F+","+A)
}else{lineToDraw.attr("path","M"+E+","+A+"L"+F+","+A)
}};
var startLineDrawing=function(A,H,D){var F,C;
var G=A-180;
var E=H-60;
var B=false;
if(orientation=="portrait"&&G>=356&&G<=356+384&&E>=81&&E<=91){F=G;
C=91;
B=true;
isVertical=true
}else{if(orientation=="portrait"&&G>=356&&G<=356+384&&E>=91+502&&E<=91+512){F=G;
C=91+502;
B=true;
isVertical=true
}else{if(orientation=="portrait"&&G>=346&&G<=356&&E>=91&&E<=91+502){F=356;
C=E;
B=true;
isVertical=false
}else{if(orientation=="portrait"&&G>=356+384&&G<=356+394&&E>=91&&E<=91+502){F=356+384;
C=E;
B=true;
isVertical=false
}else{if(orientation=="landscape"&&G>=297&&G<=297+502&&E>=140&&E<=150){F=G;
C=150;
B=true;
isVertical=true
}else{if(orientation=="landscape"&&G>=297&&G<=297+502&&E>=150+384&&E<=150+394){F=G;
C=150+384;
B=true;
isVertical=true
}else{if(orientation=="landscape"&&G>=287&&G<=297&&E>=150&&E<=150+384){F=297;
C=E;
B=true;
isVertical=false
}else{if(orientation=="landscape"&&G>=297+502&&G<=297+512&&E>=150&&E<=150+384){F=297+502;
C=E;
B=true;
isVertical=false
}}}}}}}}if(B){lineToDraw=designPaper.path("M"+F+","+C+"L"+F+","+C);
this.ox=F;
this.oy=C
}};
function drawInitialLine(B,D,A,C){var E=getPadBox();
if(B==A){return designPaper.scLine("column",0,(B-E[0])/E[2]*1000,null,null)
}else{return designPaper.scLine("row",0,(D-E[1])/E[3]*1000,null,null)
}}function updateLine(Q,C,O,A,L,R,P,V,D){var N;
var J,U,S,M,E,G,K;
var B=false;
var F=false;
var H,I;
if(!L||(L.kind!="row"&&L.kind!="column"&&L.kind!="block")){return drawInitialLine(Q,C,O,A)
}else{if(Q==O){N="column";
E=(Q-R)/V*1000
}else{N="row";
E=(C-P)/D*1000
}if(Q==R||O==R||C==P||A==P){H=true
}else{I=true
}if(L.kind=="column"){G=V*L.width/1000;
if(Q<R+G||O<R+G){B=true
}if(Q>R+G||O>R+G){F=true
}}else{if(L.kind=="row"){K=D*L.width/1000;
if(C<P+K||A<P+K){B=true
}if(C>P+K||A>P+K){F=true
}}else{if(L.kind=="block"){return L
}}}if(H){J=R;
U=P;
if(L.kind=="column"){S=G;
M=D
}else{S=V;
M=K
}if(L.first){return designPaper.scLine(L.kind,L.blockId,L.width,updateLine(Q,C,O,A,L.first,J,U,S,M),L.second)
}else{if(F){var T=designPaper.scLine(N,0,E,null,null);
return designPaper.scLine(L.kind,L.blockId,L.width,T,L.second)
}else{return L
}}}else{if(L.kind=="column"){J=R+G;
U=P;
S=V-G;
M=D
}else{J=R;
U=P+K;
S=V;
M=D-K
}if(L.second){return designPaper.scLine(L.kind,L.blockId,L.width,L.first,updateLine(Q,C,O,A,L.second,J,U,S,M))
}else{if(B){var T=designPaper.scLine(N,0,E,null,null);
return designPaper.scLine(L.kind,L.blockId,L.width,L.first,T)
}else{return L
}}}}}var stopLineDrawing=function(){var B,A,D,C;
var E=lineToDraw.getBBox();
B=E.x;
D=E.y;
if(isVertical){A=B;
C=D+E.height
}else{A=B+E.width;
C=E.y
}lineToDraw.remove();
while(lines.length>0){lines.pop().remove()
}var F=getPadBox();
mainLine=updateLine(B,D,A,C,mainLine,F[0],F[1],F[2],F[3]);
drawIt(getPadBox(),mainLine);
serializeDesign()
};
function serializeLine(A){if(!A){return"<empty />"
}else{if(A.kind=="column"){return"<column width='"+A.width+"'>"+serializeLine(A.first)+serializeLine(A.second)+"</column>"
}else{if(A.kind=="row"){return"<row height='"+A.width+"'>"+serializeLine(A.first)+serializeLine(A.second)+"</row>"
}else{return"<block ref='"+A.blockId+"' />"
}}}}function serializeDesign(){$("#designBlockInformation design fr[orientation='"+orientation+"']").empty();
$("#designBlockInformation design fr[orientation='"+orientation+"']").append(serializeLine(mainLine))
}var choosePresentationType=function(B){presentationType=this.data("presentationType");
presentationDetail=this.data("presentationDetail");
serializeChoice(blockId,presentationType,presentationDetail,title,blockAttributeNames,blockAttributeValues);
for(var A=0;
A<backgrounds.length;
A++){backgrounds[A].attr("fill","cornsilk")
}this.animate({fill:"#66f","fill-opacity":0.2},500);
$(".detailThumbnailRow").hide();
$(".detailThumbnailRow[presentationType='"+presentationType+"']").show();
$(".additionalAttributeForm").hide();
if(presentationDetail&&presentationDetail!="unknown"){$(".additionalAttributeForm[presentationType='"+presentationType+"'][presentationDetail='"+presentationDetail+"']").show()
}};
function drawBlockChoice(A,D,C,B){var F=Raphael(A,80,80);
var E=F.rect(2,2,76,76);
E.attr("fill","cornsilk");
E.data("presentationType",D);
if(B){E.data("presentationDetail",C)
}else{E.data("presentationDetail","unknown")
}E.click(choosePresentationType);
backgrounds.push(E);
if(D=="pie"){drawPie(F,C,2,2,76,76)
}else{if(D=="bar"){drawBar(F,C,2,2,76,76)
}else{if(D=="line"){drawLineChart(F,C,2,2,76,76)
}else{if(D=="correlation"){drawCorrelation(F,C,2,2,76,76)
}else{if(D=="column"){drawColumn(F,C,2,2,76,76)
}else{if(D=="table"){drawTable(F,C,2,2,76,76)
}else{if(D=="trendIndicator"){drawIndicator(F,C,2,2,76,76)
}else{if(D=="statusIndicator"){drawIndicator(F,C,2,2,76,76)
}}}}}}}}}function serializeChoice(A,E,C,H,D,F){var G="<block blockId='"+A+"'><presentationType>"+E+"</presentationType>";
G=G+"<presentationDetail>"+C+"</presentationDetail>";
G=G+"<title>"+H+"</title>";
for(var B=0;
B<D.length;
B++){G=G+"<"+D[B]+">"+F[B]+"</"+D[B]+">"
}G=G+"<structure /></block>";
$("#serializedBlock").data("serializedBlock",G)
}function takeAttribute(A,B){if(A!="presentationType"&&A!="presentationDetail"&&A!="title"&&A!="structure"){for(i=0;
i<blockAttributeNames.length;
i++){if(blockAttributeNames[i]==A){blockAttributeValues[i]=B;
$(".blockAttributeInput[inputFor='"+A+"']").val(B)
}}}}function readBlockInformation(){var A=$("#readBlockFirstTime block");
presentationType=A.find("presentationType").text();
presentationDetail=A.find("presentationDetail").text();
title=A.find("title").text();
blockId=A.attr("blockId");
blockAttributeNames=[];
blockAttributeValues=[];
$(".blockAttributeInput").each(function(){if($(this).parent().parent().attr("presentationType")==presentationType&&$(this).parent().parent().attr("presentationDetail")==presentationDetail){var B=$(this).attr("inputFor");
blockAttributeNames.push(B);
blockAttributeValues.push("")
}});
A.children().each(function(){takeAttribute($(this).tagName,$(this).text())
});
countAttributesInBlock=A.find("measure").size();
countMeasuresInBlock=A.find("attribute").size();
serializeChoice(blockId,presentationType,presentationDetail,title,blockAttributeNames,blockAttributeValues)
}function saveAdditionalAttributes(){$(".blockAttributeInput").each(function(){if($(this).parent().parent().attr("presentationType")==presentationType&&$(this).parent().parent().attr("presentationDetail")==presentationDetail){var A=$(this).attr("inputFor");
var B=$(this).val();
for(i=0;
i<blockAttributeNames.length;
i++){if(blockAttributeNames[i]==A){blockAttributeValues[i]=B
}}}});
serializeChoice(blockId,presentationType,presentationDetail,title,blockAttributeNames,blockAttributeValues)
}var blindText="Lorem ipsum dolor sit amet,\n consetetur sadipscing elitr,\n sed diam nonumy eirmod tempor\n invidunt ut labore et dolore magna\n aliquyam erat, sed diam voluptua.\n At vero eos et accusam et justo\n duo dolores et ea rebum.\n Stet clita kasd gubergren,\n no sea takimata sanctus\n est Lorem ipsum dolor sit amet.\n Lorem ipsum dolor sit amet,\n consetetur sadipscing elitr,\n sed diam nonumy eirmod tempor\n invidunt ut labore et dolore magna\n aliquyam erat, sed diam voluptua.";
function showScorecard(){paper=Raphael(728,60,548,684);
background=paper.rect(82,91,384,502);
background.attr("fill","#eee");
background.attr("stroke","#eee");
scorecardFrame=paper.rect(82,91,384,502);
scorecardTitle=paper.text(82+384/2,91,"Scorecard Title");
scorecardHorizontal=paper.path("M82,320L466,320");
scorecardVertical=paper.path("M274,320L274,593");
var A=Raphael(810,380,60,60);
scorecardStandardText=paper.text(178,456,blindText);
drawPie(paper,"mirror",275,320,200,273);
drawBar(paper,"plain",82,120,380,120)
}function updateScorecard(C,A){if(C=="standardText"){}else{if(C=="scorecard_style_margin"){scorecardFrame.attr("x",82+A);
scorecardFrame.attr("y",91+A);
scorecardFrame.attr("width",384-2*A);
scorecardFrame.attr("height",502-2*A)
}else{if(C=="scorecard_style_padding"){}else{if(C=="scorecard_style_border_width"){scorecardFrame.attr("stroke-width",A)
}else{if(C=="line_style_dash_style"){var B;
if(A=="solid"){B=""
}else{if(A=="dotted"){B="."
}else{B="--"
}}scorecardHorizontal.attr("stroke-dasharray",B);
scorecardVertical.attr("stroke-dasharray",B)
}}}}}};