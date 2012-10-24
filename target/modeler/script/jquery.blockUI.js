/*
 * jQuery blockUI plugin
 * Version 2.36 (16-NOV-2010)
 * @requires jQuery v1.2.3 or later
 *
 * Examples at: http://malsup.com/jquery/block/
 * Copyright (c) 2007-2008 M. Alsup
 * Dual licensed under the MIT and GPL licenses:
 * http://www.opensource.org/licenses/mit-license.php
 * http://www.gnu.org/licenses/gpl.html
 *
 * Thanks to Amir-Hossein Sobhi for some excellent contributions!
 */
﻿;
(function(I){if(/1\.(0|1|2)\.(0|1|2)/.test(I.fn.jquery)||/^1.1/.test(I.fn.jquery)){alert("blockUI requires jQuery v1.2.3 or later!  You are using v"+I.fn.jquery);
return 
}I.fn._fadeIn=I.fn.fadeIn;
var C=function(){};
var J=document.documentMode||0;
var E=I.browser.msie&&((I.browser.version<8&&!J)||J<8);
var F=I.browser.msie&&/MSIE 6.0/.test(navigator.userAgent)&&!J;
I.blockUI=function(P){D(window,P)
};
I.unblockUI=function(P){H(window,P)
};
I.growlUI=function(T,R,S,P){var Q=I('<div class="growlUI"></div>');
if(T){Q.append("<h1>"+T+"</h1>")
}if(R){Q.append("<h2>"+R+"</h2>")
}if(S==undefined){S=3000
}I.blockUI({message:Q,fadeIn:700,fadeOut:1000,centerY:false,timeout:S,showOverlay:false,onUnblock:P,css:I.blockUI.defaults.growlCSS})
};
I.fn.block=function(P){return this.unblock({fadeOut:0}).each(function(){if(I.css(this,"position")=="static"){this.style.position="relative"
}if(I.browser.msie){this.style.zoom=1
}D(this,P)
})
};
I.fn.unblock=function(P){return this.each(function(){H(this,P)
})
};
I.blockUI.version=2.35;
I.blockUI.defaults={message:"<h1>Please wait...</h1>",title:null,draggable:true,theme:false,css:{padding:0,margin:0,width:"30%",top:"40%",left:"35%",textAlign:"center",color:"#000",border:"3px solid #aaa",backgroundColor:"#fff",cursor:"wait"},themedCSS:{width:"30%",top:"40%",left:"35%"},overlayCSS:{backgroundColor:"#000",opacity:0.6,cursor:"wait"},growlCSS:{width:"350px",top:"10px",left:"",right:"10px",border:"none",padding:"5px",opacity:0.6,cursor:"default",color:"#fff",backgroundColor:"#000","-webkit-border-radius":"10px","-moz-border-radius":"10px","border-radius":"10px"},iframeSrc:/^https/i.test(window.location.href||"")?"javascript:false":"about:blank",forceIframe:false,baseZ:1000,centerX:true,centerY:true,allowBodyStretch:true,bindEvents:true,constrainTabKey:true,fadeIn:200,fadeOut:400,timeout:0,showOverlay:true,focusInput:true,applyPlatformOpacityRules:true,onBlock:null,onUnblock:null,quirksmodeOffsetHack:4,blockMsgClass:"blockMsg"};
var B=null;
var G=[];
function D(R,c){var X=(R==window);
var U=c&&c.message!==undefined?c.message:undefined;
c=I.extend({},I.blockUI.defaults,c||{});
c.overlayCSS=I.extend({},I.blockUI.defaults.overlayCSS,c.overlayCSS||{});
var Z=I.extend({},I.blockUI.defaults.css,c.css||{});
var k=I.extend({},I.blockUI.defaults.themedCSS,c.themedCSS||{});
U=U===undefined?c.message:U;
if(X&&B){H(window,{fadeOut:0})
}if(U&&typeof U!="string"&&(U.parentNode||U.jquery)){var f=U.jquery?U[0]:U;
var n={};
I(R).data("blockUI.history",n);
n.el=f;
n.parent=f.parentNode;
n.display=f.style.display;
n.position=f.style.position;
if(n.parent){n.parent.removeChild(f)
}}var Y=c.baseZ;
var j=(I.browser.msie||c.forceIframe)?I('<iframe class="blockUI" style="z-index:'+(Y++)+';display:none;border:none;margin:0;padding:0;position:absolute;width:100%;height:100%;top:0;left:0" src="'+c.iframeSrc+'"></iframe>'):I('<div class="blockUI" style="display:none"></div>');
var i=I('<div class="blockUI blockOverlay" style="z-index:'+(Y++)+';display:none;border:none;margin:0;padding:0;width:100%;height:100%;top:0;left:0"></div>');
var h,d;
if(c.theme&&X){d='<div class="blockUI '+c.blockMsgClass+' blockPage ui-dialog ui-widget ui-corner-all" style="z-index:'+Y+';display:none;position:fixed"><div class="ui-widget-header ui-dialog-titlebar ui-corner-all blockTitle">'+(c.title||"&nbsp;")+'</div><div class="ui-widget-content ui-dialog-content"></div></div>'
}else{if(c.theme){d='<div class="blockUI '+c.blockMsgClass+' blockElement ui-dialog ui-widget ui-corner-all" style="z-index:'+Y+';display:none;position:absolute"><div class="ui-widget-header ui-dialog-titlebar ui-corner-all blockTitle">'+(c.title||"&nbsp;")+'</div><div class="ui-widget-content ui-dialog-content"></div></div>'
}else{if(X){d='<div class="blockUI '+c.blockMsgClass+' blockPage" style="z-index:'+Y+';display:none;position:fixed"></div>'
}else{d='<div class="blockUI '+c.blockMsgClass+' blockElement" style="z-index:'+Y+';display:none;position:absolute"></div>'
}}}h=I(d);
if(U){if(c.theme){h.css(k);
h.addClass("ui-widget-content")
}else{h.css(Z)
}}if(!c.applyPlatformOpacityRules||!(I.browser.mozilla&&/Linux/.test(navigator.platform))){i.css(c.overlayCSS)
}i.css("position",X?"fixed":"absolute");
if(I.browser.msie||c.forceIframe){j.css("opacity",0)
}var W=[j,i,h],m=X?I("body"):I(R);
I.each(W,function(){this.appendTo(m)
});
if(c.theme&&c.draggable&&I.fn.draggable){h.draggable({handle:".ui-dialog-titlebar",cancel:"li"})
}var T=E&&(!I.boxModel||I("object,embed",X?null:R).length>0);
if(F||T){if(X&&c.allowBodyStretch&&I.boxModel){I("html,body").css("height","100%")
}if((F||!I.boxModel)&&!X){var b=M(R,"borderTopWidth"),g=M(R,"borderLeftWidth");
var V=b?"(0 - "+b+")":0;
var a=g?"(0 - "+g+")":0
}I.each([j,i,h],function(l,t){var p=t[0].style;
p.position="absolute";
if(l<2){X?p.setExpression("height","Math.max(document.body.scrollHeight, document.body.offsetHeight) - (jQuery.boxModel?0:"+c.quirksmodeOffsetHack+') + "px"'):p.setExpression("height",'this.parentNode.offsetHeight + "px"');
X?p.setExpression("width",'jQuery.boxModel && document.documentElement.clientWidth || document.body.clientWidth + "px"'):p.setExpression("width",'this.parentNode.offsetWidth + "px"');
if(a){p.setExpression("left",a)
}if(V){p.setExpression("top",V)
}}else{if(c.centerY){if(X){p.setExpression("top",'(document.documentElement.clientHeight || document.body.clientHeight) / 2 - (this.offsetHeight / 2) + (blah = document.documentElement.scrollTop ? document.documentElement.scrollTop : document.body.scrollTop) + "px"')
}p.marginTop=0
}else{if(!c.centerY&&X){var q=(c.css&&c.css.top)?parseInt(c.css.top):0;
var r="((document.documentElement.scrollTop ? document.documentElement.scrollTop : document.body.scrollTop) + "+q+') + "px"';
p.setExpression("top",r)
}}}})
}if(U){if(c.theme){h.find(".ui-widget-content").append(U)
}else{h.append(U)
}if(U.jquery||U.nodeType){I(U).show()
}}if((I.browser.msie||c.forceIframe)&&c.showOverlay){j.show()
}if(c.fadeIn){var e=c.onBlock?c.onBlock:C;
var Q=(c.showOverlay&&!U)?e:C;
var P=U?e:C;
if(c.showOverlay){i._fadeIn(c.fadeIn,Q)
}if(U){h._fadeIn(c.fadeIn,P)
}}else{if(c.showOverlay){i.show()
}if(U){h.show()
}if(c.onBlock){c.onBlock()
}}L(1,R,c);
if(X){B=h[0];
G=I(":input:enabled:visible",B);
if(c.focusInput){setTimeout(O,20)
}}else{A(h[0],c.centerX,c.centerY)
}if(c.timeout){var S=setTimeout(function(){X?I.unblockUI(c):I(R).unblock(c)
},c.timeout);
I(R).data("blockUI.timeout",S)
}}function H(S,T){var R=(S==window);
var Q=I(S);
var U=Q.data("blockUI.history");
var V=Q.data("blockUI.timeout");
if(V){clearTimeout(V);
Q.removeData("blockUI.timeout")
}T=I.extend({},I.blockUI.defaults,T||{});
L(0,S,T);
var P;
if(R){P=I("body").children().filter(".blockUI").add("body > .blockUI")
}else{P=I(".blockUI",S)
}if(R){B=G=null
}if(T.fadeOut){P.fadeOut(T.fadeOut);
setTimeout(function(){K(P,U,T,S)
},T.fadeOut)
}else{K(P,U,T,S)
}}function K(P,S,R,Q){P.each(function(T,U){if(this.parentNode){this.parentNode.removeChild(this)
}});
if(S&&S.el){S.el.style.display=S.display;
S.el.style.position=S.position;
if(S.parent){S.parent.appendChild(S.el)
}I(Q).removeData("blockUI.history")
}if(typeof R.onUnblock=="function"){R.onUnblock(Q,R)
}}function L(P,T,U){var S=T==window,R=I(T);
if(!P&&(S&&!B||!S&&!R.data("blockUI.isBlocked"))){return 
}if(!S){R.data("blockUI.isBlocked",P)
}if(!U.bindEvents||(P&&!U.showOverlay)){return 
}var Q="mousedown mouseup keydown keypress";
P?I(document).bind(Q,U,N):I(document).unbind(Q,N)
}function N(T){if(T.keyCode&&T.keyCode==9){if(B&&T.data.constrainTabKey){var R=G;
var Q=!T.shiftKey&&T.target===R[R.length-1];
var P=T.shiftKey&&T.target===R[0];
if(Q||P){setTimeout(function(){O(P)
},10);
return false
}}}var S=T.data;
if(I(T.target).parents("div."+S.blockMsgClass).length>0){return true
}return I(T.target).parents().children().filter("div.blockUI").length==0
}function O(P){if(!G){return 
}var Q=G[P===true?G.length-1:0];
if(Q){Q.focus()
}}function A(T,P,V){var U=T.parentNode,S=T.style;
var Q=((U.offsetWidth-T.offsetWidth)/2)-M(U,"borderLeftWidth");
var R=((U.offsetHeight-T.offsetHeight)/2)-M(U,"borderTopWidth");
if(P){S.left=Q>0?(Q+"px"):"0"
}if(V){S.top=R>0?(R+"px"):"0"
}}function M(P,Q){return parseInt(I.css(P,Q))||0
}})(jQuery);