(function(A){EYE.extend({getPosition:function(E,B){var I=0;
var G=0;
var J=E.style;
var K=false;
if(B&&jQuery.curCSS(E,"display")=="none"){var F=J.visibility;
var H=J.position;
K=true;
J.visibility="hidden";
J.display="block";
J.position="absolute"
}var C=E;
if(C.getBoundingClientRect){var D=C.getBoundingClientRect();
I=D.left+Math.max(document.documentElement.scrollLeft,document.body.scrollLeft)-2;
G=D.top+Math.max(document.documentElement.scrollTop,document.body.scrollTop)-2
}else{I=C.offsetLeft;
G=C.offsetTop;
C=C.offsetParent;
if(E!=C){while(C){I+=C.offsetLeft;
G+=C.offsetTop;
C=C.offsetParent
}}if(jQuery.browser.safari&&jQuery.curCSS(E,"position")=="absolute"){I-=document.body.offsetLeft;
G-=document.body.offsetTop
}C=E.parentNode;
while(C&&C.tagName.toUpperCase()!="BODY"&&C.tagName.toUpperCase()!="HTML"){if(jQuery.curCSS(C,"display")!="inline"){I-=C.scrollLeft;
G-=C.scrollTop
}C=C.parentNode
}}if(K==true){J.display="none";
J.position=H;
J.visibility=F
}return{x:I,y:G}
},getSize:function(G){var B=parseInt(jQuery.curCSS(G,"width"),10);
var E=parseInt(jQuery.curCSS(G,"height"),10);
var F=0;
var I=0;
if(jQuery.curCSS(G,"display")!="none"){F=G.offsetWidth;
I=G.offsetHeight
}else{var H=G.style;
var C=H.visibility;
var D=H.position;
H.visibility="hidden";
H.display="block";
H.position="absolute";
F=G.offsetWidth;
I=G.offsetHeight;
H.display="none";
H.position=D;
H.visibility=C
}return{w:B,h:E,wb:F,hb:I}
},getClient:function(D){var C,B;
if(D){B=D.clientWidth;
C=D.clientHeight
}else{var E=document.documentElement;
B=window.innerWidth||self.innerWidth||(E&&E.clientWidth)||document.body.clientWidth;
C=window.innerHeight||self.innerHeight||(E&&E.clientHeight)||document.body.clientHeight
}return{w:B,h:C}
},getScroll:function(H){var E=0,C=0,B=0,F=0,D=0,G=0;
if(H&&H.nodeName.toLowerCase()!="body"){E=H.scrollTop;
C=H.scrollLeft;
B=H.scrollWidth;
F=H.scrollHeight
}else{if(document.documentElement){E=document.documentElement.scrollTop;
C=document.documentElement.scrollLeft;
B=document.documentElement.scrollWidth;
F=document.documentElement.scrollHeight
}else{if(document.body){E=document.body.scrollTop;
C=document.body.scrollLeft;
B=document.body.scrollWidth;
F=document.body.scrollHeight
}}if(typeof pageYOffset!="undefined"){E=pageYOffset;
C=pageXOffset
}D=self.innerWidth||document.documentElement.clientWidth||document.body.clientWidth||0;
G=self.innerHeight||document.documentElement.clientHeight||document.body.clientHeight||0
}return{t:E,l:C,w:B,h:F,iw:D,ih:G}
},getMargins:function(G,D){var E=jQuery.curCSS(G,"marginTop")||"";
var F=jQuery.curCSS(G,"marginRight")||"";
var B=jQuery.curCSS(G,"marginBottom")||"";
var C=jQuery.curCSS(G,"marginLeft")||"";
if(D){return{t:parseInt(E,10)||0,r:parseInt(F,10)||0,b:parseInt(B,10)||0,l:parseInt(C,10)}
}else{return{t:E,r:F,b:B,l:C}
}},getPadding:function(G,D){var E=jQuery.curCSS(G,"paddingTop")||"";
var F=jQuery.curCSS(G,"paddingRight")||"";
var B=jQuery.curCSS(G,"paddingBottom")||"";
var C=jQuery.curCSS(G,"paddingLeft")||"";
if(D){return{t:parseInt(E,10)||0,r:parseInt(F,10)||0,b:parseInt(B,10)||0,l:parseInt(C,10)}
}else{return{t:E,r:F,b:B,l:C}
}},getBorder:function(G,D){var E=jQuery.curCSS(G,"borderTopWidth")||"";
var F=jQuery.curCSS(G,"borderRightWidth")||"";
var B=jQuery.curCSS(G,"borderBottomWidth")||"";
var C=jQuery.curCSS(G,"borderLeftWidth")||"";
if(D){return{t:parseInt(E,10)||0,r:parseInt(F,10)||0,b:parseInt(B,10)||0,l:parseInt(C,10)||0}
}else{return{t:E,r:F,b:B,l:C}
}},traverseDOM:function(B,C){C(B);
B=B.firstChild;
while(B){EYE.traverseDOM(B,C);
B=B.nextSibling
}},getInnerWidth:function(D,B){var C=D.offsetWidth;
return B?Math.max(D.scrollWidth,C)-C+D.clientWidth:D.clientWidth
},getInnerHeight:function(D,B){var C=D.offsetHeight;
return B?Math.max(D.scrollHeight,C)-C+D.clientHeight:D.clientHeight
},getExtraWidth:function(B){if(A.boxModel){return(parseInt(A.curCSS(B,"paddingLeft"))||0)+(parseInt(A.curCSS(B,"paddingRight"))||0)+(parseInt(A.curCSS(B,"borderLeftWidth"))||0)+(parseInt(A.curCSS(B,"borderRightWidth"))||0)
}return 0
},getExtraHeight:function(B){if(A.boxModel){return(parseInt(A.curCSS(B,"paddingTop"))||0)+(parseInt(A.curCSS(B,"paddingBottom"))||0)+(parseInt(A.curCSS(B,"borderTopWidth"))||0)+(parseInt(A.curCSS(B,"borderBottomWidth"))||0)
}return 0
},isChildOf:function(D,C,B){if(D==C){return true
}if(!C||!C.nodeType||C.nodeType!=1){return false
}if(D.contains&&!A.browser.safari){return D.contains(C)
}if(D.compareDocumentPosition){return !!(D.compareDocumentPosition(C)&16)
}var E=C.parentNode;
while(E&&E!=B){if(E==D){return true
}E=E.parentNode
}return false
},centerEl:function(E,D){var B=EYE.getScroll();
var C=EYE.getSize(E);
if(!D||D=="vertically"){A(E).css({top:B.t+((Math.min(B.h,B.ih)-C.hb)/2)+"px"})
}if(!D||D=="horizontally"){A(E).css({left:B.l+((Math.min(B.w,B.iw)-C.wb)/2)+"px"})
}}});
if(!A.easing.easeout){A.easing.easeout=function(D,F,B,E,C){return -E*((F=F/C-1)*F*F*F-1)+B
}
}})(jQuery);