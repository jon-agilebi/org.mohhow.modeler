(function(B){var A=function(){var s={},C,n=65,T,p='<div class="colorpicker"><div class="colorpicker_color"><div><div></div></div></div><div class="colorpicker_hue"><div></div></div><div class="colorpicker_new_color"></div><div class="colorpicker_current_color"></div><div class="colorpicker_hex"><input type="text" maxlength="6" size="6" /></div><div class="colorpicker_rgb_r colorpicker_field"><input type="text" maxlength="3" size="3" /><span></span></div><div class="colorpicker_rgb_g colorpicker_field"><input type="text" maxlength="3" size="3" /><span></span></div><div class="colorpicker_rgb_b colorpicker_field"><input type="text" maxlength="3" size="3" /><span></span></div><div class="colorpicker_hsb_h colorpicker_field"><input type="text" maxlength="3" size="3" /><span></span></div><div class="colorpicker_hsb_s colorpicker_field"><input type="text" maxlength="3" size="3" /><span></span></div><div class="colorpicker_hsb_b colorpicker_field"><input type="text" maxlength="3" size="3" /><span></span></div><div class="colorpicker_submit"></div></div>',b={eventName:"click",onShow:function(){},onBeforeShow:function(){},onHide:function(){},onChange:function(){},onSubmit:function(){},color:"ff0000",livePreview:true,flat:false},j=function(t,v){var u=J(t);
B(v).data("colorpicker").fields.eq(1).val(u.r).end().eq(2).val(u.g).end().eq(3).val(u.b).end()
},U=function(t,u){B(u).data("colorpicker").fields.eq(4).val(t.h).end().eq(5).val(t.s).end().eq(6).val(t.b).end()
},G=function(t,u){B(u).data("colorpicker").fields.eq(0).val(r(t)).end()
},L=function(t,u){B(u).data("colorpicker").selector.css("backgroundColor","#"+r({h:t.h,s:100,b:100}));
B(u).data("colorpicker").selectorIndic.css({left:parseInt(150*t.s/100,10),top:parseInt(150*(100-t.b)/100,10)})
},g=function(t,u){B(u).data("colorpicker").hue.css("top",parseInt(150-150*t.h/360,10))
},H=function(t,u){B(u).data("colorpicker").currentColor.css("backgroundColor","#"+r(t))
},e=function(t,u){B(u).data("colorpicker").newColor.css("backgroundColor","#"+r(t))
},N=function(t){var v=t.charCode||t.keyCode||-1;
if((v>n&&v<=90)||v==32){return false
}var u=B(this).parent().parent();
if(u.data("colorpicker").livePreview===true){E.apply(this)
}},E=function(u){var v=B(this).parent().parent(),t;
if(this.parentNode.className.indexOf("_hex")>0){v.data("colorpicker").color=t=M(Y(this.value))
}else{if(this.parentNode.className.indexOf("_hsb")>0){v.data("colorpicker").color=t=F({h:parseInt(v.data("colorpicker").fields.eq(4).val(),10),s:parseInt(v.data("colorpicker").fields.eq(5).val(),10),b:parseInt(v.data("colorpicker").fields.eq(6).val(),10)})
}else{v.data("colorpicker").color=t=I(m({r:parseInt(v.data("colorpicker").fields.eq(1).val(),10),g:parseInt(v.data("colorpicker").fields.eq(2).val(),10),b:parseInt(v.data("colorpicker").fields.eq(3).val(),10)}))
}}if(u){j(t,v.get(0));
G(t,v.get(0));
U(t,v.get(0))
}L(t,v.get(0));
g(t,v.get(0));
e(t,v.get(0));
v.data("colorpicker").onChange.apply(v,[t,r(t),J(t)])
},O=function(t){var u=B(this).parent().parent();
u.data("colorpicker").fields.parent().removeClass("colorpicker_focus")
},k=function(){n=this.parentNode.className.indexOf("_hex")>0?70:65;
B(this).parent().parent().data("colorpicker").fields.parent().removeClass("colorpicker_focus");
B(this).parent().addClass("colorpicker_focus")
},i=function(t){var v=B(this).parent().find("input").focus();
var u={el:B(this).parent().addClass("colorpicker_slider"),max:this.parentNode.className.indexOf("_hsb_h")>0?360:(this.parentNode.className.indexOf("_hsb")>0?100:255),y:t.pageY,field:v,val:parseInt(v.val(),10),preview:B(this).parent().parent().data("colorpicker").livePreview};
B(document).bind("mouseup",u,S);
B(document).bind("mousemove",u,l)
},l=function(t){t.data.field.val(Math.max(0,Math.min(t.data.max,parseInt(t.data.val+t.pageY-t.data.y,10))));
if(t.data.preview){E.apply(t.data.field.get(0),[true])
}return false
},S=function(t){E.apply(t.data.field.get(0),[true]);
t.data.el.removeClass("colorpicker_slider").find("input").focus();
B(document).unbind("mouseup",S);
B(document).unbind("mousemove",l);
return false
},W=function(t){var u={cal:B(this).parent(),y:B(this).offset().top};
u.preview=u.cal.data("colorpicker").livePreview;
B(document).bind("mouseup",u,R);
B(document).bind("mousemove",u,K)
},K=function(t){E.apply(t.data.cal.data("colorpicker").fields.eq(4).val(parseInt(360*(150-Math.max(0,Math.min(150,(t.pageY-t.data.y))))/150,10)).get(0),[t.data.preview]);
return false
},R=function(t){j(t.data.cal.data("colorpicker").color,t.data.cal.get(0));
G(t.data.cal.data("colorpicker").color,t.data.cal.get(0));
B(document).unbind("mouseup",R);
B(document).unbind("mousemove",K);
return false
},X=function(t){var u={cal:B(this).parent(),pos:B(this).offset()};
u.preview=u.cal.data("colorpicker").livePreview;
B(document).bind("mouseup",u,a);
B(document).bind("mousemove",u,Q)
},Q=function(t){E.apply(t.data.cal.data("colorpicker").fields.eq(6).val(parseInt(100*(150-Math.max(0,Math.min(150,(t.pageY-t.data.pos.top))))/150,10)).end().eq(5).val(parseInt(100*(Math.max(0,Math.min(150,(t.pageX-t.data.pos.left))))/150,10)).get(0),[t.data.preview]);
return false
},a=function(t){j(t.data.cal.data("colorpicker").color,t.data.cal.get(0));
G(t.data.cal.data("colorpicker").color,t.data.cal.get(0));
B(document).unbind("mouseup",a);
B(document).unbind("mousemove",Q);
return false
},V=function(t){B(this).addClass("colorpicker_focus")
},q=function(t){B(this).removeClass("colorpicker_focus")
},P=function(u){var v=B(this).parent();
var t=v.data("colorpicker").color;
v.data("colorpicker").origColor=t;
H(t,v.get(0));
v.data("colorpicker").onSubmit(t,r(t),J(t),v.data("colorpicker").el)
},d=function(t){var x=B("#"+B(this).data("colorpickerId"));
x.data("colorpicker").onBeforeShow.apply(this,[x.get(0)]);
var y=B(this).offset();
var w=Z();
var v=y.top+this.offsetHeight;
var u=y.left;
if(v+176>w.t+w.h){v-=this.offsetHeight+176
}if(u+356>w.l+w.w){u-=356
}x.css({left:u+"px",top:v+"px"});
if(x.data("colorpicker").onShow.apply(this,[x.get(0)])!=false){x.show()
}B(document).bind("mousedown",{cal:x},o);
return false
},o=function(t){if(!h(t.data.cal.get(0),t.target,t.data.cal.get(0))){if(t.data.cal.data("colorpicker").onHide.apply(this,[t.data.cal.get(0)])!=false){t.data.cal.hide()
}B(document).unbind("mousedown",o)
}},h=function(v,u,t){if(v==u){return true
}if(v.contains){return v.contains(u)
}if(v.compareDocumentPosition){return !!(v.compareDocumentPosition(u)&16)
}var w=u.parentNode;
while(w&&w!=t){if(w==v){return true
}w=w.parentNode
}return false
},Z=function(){var t=document.compatMode=="CSS1Compat";
return{l:window.pageXOffset||(t?document.documentElement.scrollLeft:document.body.scrollLeft),t:window.pageYOffset||(t?document.documentElement.scrollTop:document.body.scrollTop),w:window.innerWidth||(t?document.documentElement.clientWidth:document.body.clientWidth),h:window.innerHeight||(t?document.documentElement.clientHeight:document.body.clientHeight)}
},F=function(t){return{h:Math.min(360,Math.max(0,t.h)),s:Math.min(100,Math.max(0,t.s)),b:Math.min(100,Math.max(0,t.b))}
},m=function(t){return{r:Math.min(255,Math.max(0,t.r)),g:Math.min(255,Math.max(0,t.g)),b:Math.min(255,Math.max(0,t.b))}
},Y=function(v){var t=6-v.length;
if(t>0){var w=[];
for(var u=0;
u<t;
u++){w.push("0")
}w.push(v);
v=w.join("")
}return v
},D=function(t){var t=parseInt(((t.indexOf("#")>-1)?t.substring(1):t),16);
return{r:t>>16,g:(t&65280)>>8,b:(t&255)}
},M=function(t){return I(D(t))
},I=function(v){var u={h:0,s:0,b:0};
var w=Math.min(v.r,v.g,v.b);
var t=Math.max(v.r,v.g,v.b);
var x=t-w;
u.b=t;
if(t!=0){}u.s=t!=0?255*x/t:0;
if(u.s!=0){if(v.r==t){u.h=(v.g-v.b)/x
}else{if(v.g==t){u.h=2+(v.b-v.r)/x
}else{u.h=4+(v.r-v.g)/x
}}}else{u.h=-1
}u.h*=60;
if(u.h<0){u.h+=360
}u.s*=100/255;
u.b*=100/255;
return u
},J=function(t){var w={};
var AA=Math.round(t.h);
var z=Math.round(t.s*255/100);
var u=Math.round(t.b*255/100);
if(z==0){w.r=w.g=w.b=u
}else{var AB=u;
var y=(255-z)*u/255;
var x=(AB-y)*(AA%60)/60;
if(AA==360){AA=0
}if(AA<60){w.r=AB;
w.b=y;
w.g=y+x
}else{if(AA<120){w.g=AB;
w.b=y;
w.r=AB-x
}else{if(AA<180){w.g=AB;
w.r=y;
w.b=y+x
}else{if(AA<240){w.b=AB;
w.r=y;
w.g=AB-x
}else{if(AA<300){w.b=AB;
w.g=y;
w.r=y+x
}else{if(AA<360){w.r=AB;
w.g=y;
w.b=AB-x
}else{w.r=0;
w.g=0;
w.b=0
}}}}}}}return{r:Math.round(w.r),g:Math.round(w.g),b:Math.round(w.b)}
},c=function(t){var u=[t.r.toString(16),t.g.toString(16),t.b.toString(16)];
B.each(u,function(v,w){if(w.length==1){u[v]="0"+w
}});
return u.join("")
},r=function(t){return c(J(t))
},f=function(){var u=B(this).parent();
var t=u.data("colorpicker").origColor;
u.data("colorpicker").color=t;
j(t,u.get(0));
G(t,u.get(0));
U(t,u.get(0));
L(t,u.get(0));
g(t,u.get(0));
e(t,u.get(0))
};
return{init:function(t){t=B.extend({},b,t||{});
if(typeof t.color=="string"){t.color=M(t.color)
}else{if(t.color.r!=undefined&&t.color.g!=undefined&&t.color.b!=undefined){t.color=I(t.color)
}else{if(t.color.h!=undefined&&t.color.s!=undefined&&t.color.b!=undefined){t.color=F(t.color)
}else{return this
}}}return this.each(function(){if(!B(this).data("colorpickerId")){var u=B.extend({},t);
u.origColor=t.color;
var w="collorpicker_"+parseInt(Math.random()*1000);
B(this).data("colorpickerId",w);
var v=B(p).attr("id",w);
if(u.flat){v.appendTo(this).show()
}else{v.appendTo(document.body)
}u.fields=v.find("input").bind("keyup",N).bind("change",E).bind("blur",O).bind("focus",k);
v.find("span").bind("mousedown",i).end().find(">div.colorpicker_current_color").bind("click",f);
u.selector=v.find("div.colorpicker_color").bind("mousedown",X);
u.selectorIndic=u.selector.find("div div");
u.el=this;
u.hue=v.find("div.colorpicker_hue div");
v.find("div.colorpicker_hue").bind("mousedown",W);
u.newColor=v.find("div.colorpicker_new_color");
u.currentColor=v.find("div.colorpicker_current_color");
v.data("colorpicker",u);
v.find("div.colorpicker_submit").bind("mouseenter",V).bind("mouseleave",q).bind("click",P);
j(u.color,v.get(0));
U(u.color,v.get(0));
G(u.color,v.get(0));
g(u.color,v.get(0));
L(u.color,v.get(0));
H(u.color,v.get(0));
e(u.color,v.get(0));
if(u.flat){v.css({position:"relative",display:"block"})
}else{B(this).bind(u.eventName,d)
}}})
},showPicker:function(){return this.each(function(){if(B(this).data("colorpickerId")){d.apply(this)
}})
},hidePicker:function(){return this.each(function(){if(B(this).data("colorpickerId")){B("#"+B(this).data("colorpickerId")).hide()
}})
},setColor:function(t){if(typeof t=="string"){t=M(t)
}else{if(t.r!=undefined&&t.g!=undefined&&t.b!=undefined){t=I(t)
}else{if(t.h!=undefined&&t.s!=undefined&&t.b!=undefined){t=F(t)
}else{return this
}}}return this.each(function(){if(B(this).data("colorpickerId")){var u=B("#"+B(this).data("colorpickerId"));
u.data("colorpicker").color=t;
u.data("colorpicker").origColor=t;
j(t,u.get(0));
U(t,u.get(0));
G(t,u.get(0));
g(t,u.get(0));
L(t,u.get(0));
H(t,u.get(0));
e(t,u.get(0))
}})
}}
}();
B.fn.extend({ColorPicker:A.init,ColorPickerHide:A.hidePicker,ColorPickerShow:A.showPicker,ColorPickerSetColor:A.setColor})
})(jQuery);