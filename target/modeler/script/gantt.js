var ids=[];
var durations=[];
var absoluteDurations=[];
var names=[];
var predecessors=[];
var starts=[];
var ends=[];
var earliestBegin;
var latestEnd;
var copmpleteDuration;
var maxDuration=0;
var maxTextWidth=0;
var maxTextHeight=0;
var textBoxes=[];
var kinds=[];
function showBurndown(){var T=Raphael(740,514,520,225);
var S=T.rect(0,0,520,225);
S.attr("fill","#ffa");
S.attr("stroke","#000");
var U=$("#burndownData burndown");
var R=U.find("points").text();
var J=T.text(5,5,R);
var F=J.getBBox().width/2;
var B=J.getBBox().height/2;
J.translate(F,B);
var M=U.find("begin").text();
var L=U.find("end").text();
var W=T.text(5,220,M);
var N=T.text(515,220,L);
var C=W.getBBox().width/2;
var H=-1*W.getBBox().height/2;
var I=-1*N.getBBox().width/2;
W.translate(C+2*F,H);
N.translate(I,H);
var G=10+2*F;
var D=210+H;
var E=T.path("M"+G+",5L"+G+","+D);
var O=220+H;
var V=T.path("M"+G+","+D+"L515,"+D);
var A=U.find("duration").text();
var P=515-G;
var K=P/(5*A+1);
for(var Q=0;
Q<A;
Q++){U.find("item").each(function(){var X=$(this);
var b=Number(X.children().eq(0).text());
var Z=Number(X.children().eq(1).text());
if(b==Q&&Number(R)>0){var a=(D-5)*Z/Number(R);
var Y=T.rect(G+K*(5*Q+1),D-a,4*K,a);
Y.attr("fill","#bbd");
Y.attr("stroke","#000")
}})
}}function showReleasePlan(){var B=Raphael(192,514,520,225);
var A=B.rect(0,0,520,225);
A.attr("fill","#ffa");
A.attr("stroke","#000");
readPlan();
showGantt(B)
}function readPlan(){var A=$("#releasePlanData plan");
A.find("row").each(function(){var B=$(this);
ids.push("x");
kinds.push(B.children().eq(0).text());
names.push(B.children().eq(1).text());
starts.push(B.children().eq(2).text());
ends.push(B.children().eq(3).text());
durations.push(B.children().eq(4).text());
absoluteDurations.push(B.children().eq(5).text())
});
earliestBegin=A.find("earliestBegin").text();
latestEnd=A.find("latestEnd").text();
completeDuration=A.find("completeDuration").text()
}function showGantt(L){for(var Q=0;
Q<ids.length;
Q++){var M=L.text(0,0,names[Q]);
if(M.getBBox().width>maxTextWidth){maxTextWidth=M.getBBox().width
}if(M.getBBox().height>maxTextHeight){maxTextHeight=M.getBBox().height
}textBoxes.push(M)
}var N=Math.ceil(maxTextWidth)+30;
var B=L.path("M"+N+",10L"+N+",210");
B.attr("stroke-dasharray",".");
var F=L.text(N,225,earliestBegin);
var T=L.text(520,225,latestEnd);
var E=F.getBBox().width/2;
var S=F.getBBox().height*(-1);
F.translate(E,S);
var H=T.getBBox().width/2*(-1);
var R=T.getBBox().height*(-1);
T.translate(H,R);
var Q=0;
while(Q<ids.length){var J=30+maxTextWidth;
if(completeDuration){J=J+(500-30-maxTextWidth)*(absoluteDurations[Q]-durations[Q])/completeDuration
}var I=2*Q*maxTextHeight+15;
if(I+maxTextHeight<210){textBoxes[Q].translate(25+textBoxes[Q].getBBox().width/2,2*Q*maxTextHeight+20).toFront();
if(kinds[Q]=="sprint"){var C=0;
if(completeDuration){C=(500-30-maxTextWidth)*(durations[Q]/completeDuration)
}var G=L.rect(J,I,C,maxTextHeight);
G.attr("fill","#bbd");
G.attr("stroke","#000")
}else{if(completeDuration){J=30+maxTextWidth+(500-30-maxTextWidth)*(absoluteDurations[Q])/completeDuration
}var D=I+maxTextHeight/2;
var A=I+maxTextHeight;
var O=J+maxTextHeight/2;
var K=J+maxTextHeight;
var P=L.path("M"+J+","+D+"L"+O+","+I+"L"+K+","+D+"L"+O+","+A+"Z");
P.attr("fill","#000")
}}else{textBoxes[Q].remove()
}Q++
}};