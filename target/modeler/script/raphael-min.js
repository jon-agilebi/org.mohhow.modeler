(function(K){var J="0.3.2",I="hasOwnProperty",H=/[\.\/]/,G="*",F=function(){},E=function(M,L){return M-L
},D,C,B={n:{}},A=function(X,W){var V=B,U=C,T=Array.prototype.slice.call(arguments,2),S=A.listeners(X),R=0,Q=!1,P,O=[],N={},M=[],L=[];
D=X,C=0;
for(var Z=0,Y=S.length;
Z<Y;
Z++){"zIndex" in S[Z]&&(O.push(S[Z].zIndex),S[Z].zIndex<0&&(N[S[Z].zIndex]=S[Z]))
}O.sort(E);
while(O[R]<0){P=N[O[R++]],M.push(P.apply(W,T));
if(C){C=U;
return M
}}for(Z=0;
Z<Y;
Z++){P=S[Z];
if("zIndex" in P){if(P.zIndex==O[R]){M.push(P.apply(W,T));
if(C){C=U;
return M
}do{R++,P=N[O[R]],P&&M.push(P.apply(W,T));
if(C){C=U;
return M
}}while(P)
}else{N[P.zIndex]=P
}}else{M.push(P.apply(W,T));
if(C){C=U;
return M
}}}C=U;
return M.length?M:null
};
A.listeners=function(X){var W=X.split(H),V=B,U,T,S,R,Q,P,O,N,M=[V],L=[];
for(R=0,Q=W.length;
R<Q;
R++){N=[];
for(P=0,O=M.length;
P<O;
P++){V=M[P].n,T=[V[W[R]],V[G]],S=2;
while(S--){U=T[S],U&&(N.push(U),L=L.concat(U.f||[]))
}}M=N
}return L
},A.on=function(M,L){var Q=M.split(H),P=B;
for(var O=0,N=Q.length;
O<N;
O++){P=P.n,!P[Q[O]]&&(P[Q[O]]={n:{}}),P=P[Q[O]]
}P.f=P.f||[];
for(O=0,N=P.f.length;
O<N;
O++){if(P.f[O]==L){return F
}}P.f.push(L);
return function(R){+R==+R&&(L.zIndex=+R)
}
},A.stop=function(){C=1
},A.nt=function(L){if(L){return(new RegExp("(?:\\.|\\/|^)"+L+"(?:\\.|\\/|$)")).test(D)
}return D
},A.unbind=function(V,U){var T=V.split(H),S,R,Q,P=[B];
for(var O=0,N=T.length;
O<N;
O++){for(var M=0;
M<P.length;
M+=Q.length-2){Q=[M,1],S=P[M].n;
if(T[O]!=G){S[T[O]]&&Q.push(S[T[O]])
}else{for(R in S){S[I](R)&&Q.push(S[R])
}}P.splice.apply(P,Q)
}}for(O=0,N=P.length;
O<N;
O++){S=P[O];
while(S.n){if(U){if(S.f){for(M=0,jj=S.f.length;
M<jj;
M++){if(S.f[M]==U){S.f.splice(M,1);
break
}}!S.f.length&&delete S.f
}for(R in S.n){if(S.n[I](R)&&S.n[R].f){var L=S.n[R].f;
for(M=0,jj=L.length;
M<jj;
M++){if(L[M]==U){L.splice(M,1);
break
}}!L.length&&delete S.n[R].f
}}}else{delete S.f;
for(R in S.n){S.n[I](R)&&S.n[R].f&&delete S.n[R].f
}}S=S.n
}}},A.version=J,A.toString=function(){return"You are running Eve "+J
},typeof module!="undefined"&&module.exports?module.exports=A:K.eve=A
})(this),function(){function AD(CU,CT,CS,CR,CQ,CP){CS=BT(CS);
var CO,CN,CM,CL=[],CK,CJ,CH,s=CU.ms,n={},c={},Z={};
if(CR){for(V=0,T=AK.length;
V<T;
V++){var X=AK[V];
if(X.el.id==CT.id&&X.anim==CU){X.percent!=CS?(AK.splice(V,1),CM=1):CN=X,CT.attr(X.totalOrigin);
break
}}}else{CR=+c
}for(var V=0,T=CU.percents.length;
V<T;
V++){if(CU.percents[V]==CS||CU.percents[V]>CR*CU.top){CS=CU.percents[V],CJ=CU.percents[V-1]||0,s=s/CU.top*(CS-CJ),CK=CU.percents[V+1],CO=CU.anim[CS];
break
}CR&&CT.attr(CU.anim[CU.percents[V]])
}if(!!CO){if(!CN){for(attr in CO){if(CO[Aq](attr)){if(BL[Aq](attr)||CT.paper.customAttributes[Aq](attr)){n[attr]=CT.attr(attr),n[attr]==null&&(n[attr]=BN[attr]),c[attr]=CO[attr];
switch(BL[attr]){case Bq:Z[attr]=(c[attr]-n[attr])/s;
break;
case"colour":n[attr]=A1.getRGB(n[attr]);
var CI=A1.getRGB(c[attr]);
Z[attr]={r:(CI.r-n[attr].r)/s,g:(CI.g-n[attr].g)/s,b:(CI.b-n[attr].b)/s};
break;
case"path":var CG=B6(n[attr],c[attr]),CF=CG[1];
n[attr]=CG[0],Z[attr]=[];
for(V=0,T=n[attr].length;
V<T;
V++){Z[attr][V]=[0];
for(var r=1,g=n[attr][V].length;
r<g;
r++){Z[attr][V][r]=(CF[V][r]-n[attr][V][r])/s
}}break;
case"transform":var a=CT._,Y=Bw(a[attr],c[attr]);
if(Y){n[attr]=Y.from,c[attr]=Y.to,Z[attr]=[],Z[attr].real=!0;
for(V=0,T=n[attr].length;
V<T;
V++){Z[attr][V]=[n[attr][V][0]];
for(r=1,g=n[attr][V].length;
r<g;
r++){Z[attr][V][r]=(c[attr][V][r]-n[attr][V][r])/s
}}}else{var W=CT.matrix||new Bv,U={_:{transform:a.transform},getBBox:function(){return CT.getBBox(1)
}};
n[attr]=[W.a,W.b,W.c,W.d,W.e,W.f],By(U,c[attr]),c[attr]=U._.transform,Z[attr]=[(U.matrix.a-W.a)/s,(U.matrix.b-W.b)/s,(U.matrix.c-W.c)/s,(U.matrix.d-W.d)/s,(U.matrix.e-W.e)/s,(U.matrix.e-W.f)/s]
}break;
case"csv":var S=AZ(CO[attr])[AX](Ay),R=AZ(n[attr])[AX](Ay);
if(attr=="clip-rect"){n[attr]=R,Z[attr]=[],V=R.length;
while(V--){Z[attr][V]=(S[V]-n[attr][V])/s
}}c[attr]=S;
break;
default:S=[][Ah](CO[attr]),R=[][Ah](n[attr]),Z[attr]=[],V=CT.paper.customAttributes[attr].length;
while(V--){Z[attr][V]=((S[V]||0)-(R[V]||0))/s
}}}}}var Q=CO.easing,N=A1.easing_formulas[Q];
if(!N){N=AZ(Q).match(BY);
if(N&&N.length==5){var C=N;
N=function(A){return AF(A,+C[1],+C[2],+C[3],+C[4],s)
}
}else{N=BO
}}CH=CO.start||CU.start||+(new Date),X={anim:CU,percent:CS,timestamp:CH,start:CH+(CU.del||0),status:0,initstatus:CR||0,stop:!1,ms:s,easing:N,from:n,diff:Z,to:c,el:CT,callback:CO.callback,prev:CJ,next:CK,repeat:CP||CU.times,origin:CT.attr(),totalOrigin:CQ},AK.push(X);
if(CR&&!CN&&!CM){X.stop=!0,X.start=new Date-s*CR;
if(AK.length==1){return AH()
}}CM&&(X.start=new Date-X.ms*CR),AK.length==1&&AI(AH)
}else{CN.initstatus=CR,CN.start=new Date-CN.ms*CR
}eve("anim.start."+CT.id,CT,CU)
}}function AE(B,A){var E=[],D={};
this.ms=A,this.times=1;
if(B){for(var C in B){B[Aq](C)&&(D[BT(C)]=B[C],E.push(BT(C)))
}E.sort(BS)
}this.anim=D,this.top=E[E.length-1],this.percents=E
}function AF(O,N,M,L,K,J){function A(Q,P){var W,V,U,T,S,R;
for(U=Q,R=0;
R<8;
R++){T=C(U)-Q;
if(AJ(T)<P){return U
}S=(3*G*U+2*H)*U+I;
if(AJ(S)<0.000001){break
}U=U-T/S
}W=0,V=1,U=Q;
if(U<W){return W
}if(U>V){return V
}while(W<V){T=C(U);
if(AJ(T-Q)<P){return U
}Q>T?W=U:V=U,U=(V-W)/2+W
}return U
}function B(Q,P){var R=A(Q,P);
return((D*R+E)*R+F)*R
}function C(P){return((G*P+H)*P+I)*P
}var I=3*N,H=3*(L-N)-I,G=1-I-H,F=3*M,E=3*(K-M)-F,D=1-F-E;
return B(O,1/(200*J))
}function Ab(){return this.x+Aa+this.y+Aa+this.width+" × "+this.height
}function Ad(){return this.x+Aa+this.y
}function Bv(B,A,F,E,D,C){B!=null?(this.a=+B,this.b=+A,this.c=+F,this.d=+E,this.e=+D,this.f=+C):(this.a=1,this.b=0,this.c=0,this.d=1,this.e=0,this.f=0)
}function Au(B){var A=[];
for(var E=0,D=B.length;
D-2>E;
E+=2){var C=[{x:+B[E],y:+B[E+1]},{x:+B[E],y:+B[E+1]},{x:+B[E+2],y:+B[E+3]},{x:+B[E+4],y:+B[E+5]}];
D-4==E?(C[0]={x:+B[E-2],y:+B[E-1]},C[3]=C[2]):E&&(C[0]={x:+B[E-2],y:+B[E-1]}),A.push(["C",(-C[0].x+6*C[1].x+C[2].x)/6,(-C[0].y+6*C[1].y+C[2].y)/6,(C[1].x+6*C[2].x-C[3].x)/6,(C[1].y+6*C[2].y-C[3].y)/6,C[2].x,C[2].y])
}return A
}function Aw(){return this.hex
}function A0(B,A,D){function C(){var H=Array.prototype.slice.call(arguments,0),G=H.join("␀"),F=C.cache=C.cache||{},E=C.count=C.count||[];
if(F[Aq](G)){A2(E,G);
return D?D(F[G]):F[G]
}E.length>=1000&&delete F[E.shift()],E.push(G),F[G]=B[Ai](A,H);
return D?D(F[G]):F[G]
}return C
}function A2(B,A){for(var D=0,C=B.length;
D<C;
D++){if(B[D]===A){return B.push(B.splice(D,1)[0])
}}}function A1(I){if(A1.is(I,"function")){return Az?I():eve.on("DOMload",I)
}if(A1.is(I,Bm)){var H=I,G=A1._engine.create[Ai](A1,H.splice(0,3+A1.is(H[0],Bq))),F=G.set(),E=0,D=H.length,C;
for(;
E<D;
E++){C=H[E]||{},Av[Aq](C.type)&&F.push(G[C.type]().attr(C))
}return F
}var B=Array.prototype.slice.call(arguments,0);
if(A1.is(B[B.length-1],"function")){var A=B.pop();
return Az?A.call(A1._engine.create[Ai](A1,B)):eve.on("DOMload",function(){A.call(A1._engine.create[Ai](A1,B))
})
}return A1._engine.create[Ai](A1,arguments)
}A1.version="2.0.0",A1.eve=eve;
var Az,Ay=/[, ]+/,Av={circle:1,rect:1,path:1,ellipse:1,text:1,image:1},At=/\{(\d+)\}/g,As="prototype",Aq="hasOwnProperty",Ao={doc:document,win:window},Am={was:Object.prototype[Aq].call(Ao.win,"Raphael"),is:Ao.win.Raphael},Al=function(){this.ca=this.customAttributes={}
},Ak,Aj="appendChild",Ai="apply",Ah="concat",Af="createTouch" in Ao.doc,Ac="",Aa=" ",AZ=String,AX="split",AV="click dblclick mousedown mousemove mouseout mouseover mouseup touchstart touchmove touchend touchcancel"[AX](Aa),AT={mousedown:"touchstart",mousemove:"touchmove",mouseup:"touchend"},AR=AZ.prototype.toLowerCase,AP=Math,AN=AP.max,AL=AP.min,AJ=AP.abs,Bu=AP.pow,Bs=AP.PI,Bq="number",Bo="string",Bm="array",Bk="toString",Bi="fill",Bg=Object.prototype.toString,Be={},Bd="push",Bc=A1._ISURL=/^url\(['"]?([^\)]+?)['"]?\)$/i,Bb=/^\s*((#[a-f\d]{6})|(#[a-f\d]{3})|rgba?\(\s*([\d\.]+%?\s*,\s*[\d\.]+%?\s*,\s*[\d\.]+%?(?:\s*,\s*[\d\.]+%?)?)\s*\)|hsba?\(\s*([\d\.]+(?:deg|\xb0|%)?\s*,\s*[\d\.]+%?\s*,\s*[\d\.]+(?:%?\s*,\s*[\d\.]+)?)%?\s*\)|hsla?\(\s*([\d\.]+(?:deg|\xb0|%)?\s*,\s*[\d\.]+%?\s*,\s*[\d\.]+(?:%?\s*,\s*[\d\.]+)?)%?\s*\))\s*$/i,Ba={NaN:1,Infinity:1,"-Infinity":1},BY=/^(?:cubic-)?bezier\(([^,]+),([^,]+),([^,]+),([^\)]+)\)/,BX=AP.round,BV="setAttribute",BT=parseFloat,BR=parseInt,BP=AZ.prototype.toUpperCase,BN=A1._availableAttrs={"arrow-end":"none","arrow-start":"none",blur:0,"clip-rect":"0 0 1e9 1e9",cursor:"default",cx:0,cy:0,fill:"#fff","fill-opacity":1,font:'10px "Arial"',"font-family":'"Arial"',"font-size":"10","font-style":"normal","font-weight":400,gradient:0,height:0,href:"http://raphaeljs.com/",opacity:1,path:"M0,0",r:0,rx:0,ry:0,src:"",stroke:"#000","stroke-dasharray":"","stroke-linecap":"butt","stroke-linejoin":"butt","stroke-miterlimit":0,"stroke-opacity":1,"stroke-width":1,target:"_blank","text-anchor":"middle",title:"Raphael",transform:"",width:0,x:0,y:0},BL=A1._availableAnimAttrs={blur:Bq,"clip-rect":"csv",cx:Bq,cy:Bq,fill:"colour","fill-opacity":Bq,"font-size":Bq,height:Bq,opacity:Bq,path:"path",r:Bq,rx:Bq,ry:Bq,stroke:"colour","stroke-opacity":Bq,"stroke-width":Bq,transform:"transform",width:Bq,x:Bq,y:Bq},BJ=/\s*,\s*/,BH={hs:1,rg:1},BF=/,?([achlmqrstvxz]),?/gi,BD=/([achlmrqstvz])[\s,]*((-?\d*\.?\d*(?:e[\-+]?\d+)?\s*,?\s*)+)/ig,BB=/([rstm])[\s,]*((-?\d*\.?\d*(?:e[\-+]?\d+)?\s*,?\s*)+)/ig,CD=/(-?\d*\.?\d*(?:e[\-+]?\d+)?)\s*,?\s*/ig,A5=A1._radial_gradient=/^r(?:\(([^,]+?)\s*,\s*([^\)]+?)\))?/,BW={},BU=function(B,A){return B.key-A.key
},BS=function(B,A){return BT(B)-BT(A)
},BQ=function(){},BO=function(A){return A
},BM=A1._rectPath=function(B,A,E,D,C){if(C){return[["M",B+C,A],["l",E-C*2,0],["a",C,C,0,0,1,C,C],["l",0,D-C*2],["a",C,C,0,0,1,-C,C],["l",C*2-E,0],["a",C,C,0,0,1,-C,-C],["l",0,C*2-D],["a",C,C,0,0,1,C,-C],["z"]]
}return[["M",B,A],["l",E,0],["l",0,D],["l",-E,0],["z"]]
},BK=function(B,A,D,C){C==null&&(C=D);
return[["M",B,A],["m",0,-C],["a",D,C,0,1,1,0,2*C],["a",D,C,0,1,1,0,-2*C],["z"]]
},BI=A1._getPath={path:function(A){return A.attr("path")
},circle:function(B){var A=B.attrs;
return BK(A.cx,A.cy,A.r)
},ellipse:function(B){var A=B.attrs;
return BK(A.cx,A.cy,A.rx,A.ry)
},rect:function(B){var A=B.attrs;
return BM(A.x,A.y,A.width,A.height,A.r)
},image:function(B){var A=B.attrs;
return BM(A.x,A.y,A.width,A.height)
},text:function(B){var A=B._getBBox();
return BM(A.x,A.y,A.width,A.height)
}},BG=A1.mapPath=function(B,A){if(!A){return B
}var G,F,E,D,C;
B=B6(B);
for(E=0,ii=B.length;
E<ii;
E++){C=B[E];
for(D=1,jj=C.length;
D<jj;
D+=2){G=A.x(C[D],C[D+1]),F=A.y(C[D],C[D+1]),C[D]=G,C[D+1]=F
}}return B
};
A1._g=Ao,A1.type=Ao.win.SVGAngle||Ao.doc.implementation.hasFeature("http://www.w3.org/TR/SVG11/feature#BasicStructure","1.1")?"SVG":"VML";
if(A1.type=="VML"){var BE=Ao.doc.createElement("div"),BC;
BE.innerHTML='<v:shape adj="1"/>',BC=BE.firstChild,BC.style.behavior="url(#default#VML)";
if(!BC||typeof BC.adj!="object"){return A1.type=Ac
}BE=null
}A1.svg=!(A1.vml=A1.type=="VML"),A1._Paper=Al,A1.fn=Ak=Al.prototype=A1.prototype,A1._id=0,A1._oid=0,A1.is=function(B,A){A=AR.call(A);
if(A=="finite"){return !Ba[Aq](+B)
}if(A=="array"){return B instanceof Array
}return A=="null"&&B===null||A==typeof B&&B!==null||A=="object"&&B===Object(B)||A=="array"&&Array.isArray&&Array.isArray(B)||Bg.call(B).slice(8,-1).toLowerCase()==A
},A1.angle=function(A,H,G,F,E,D){if(E==null){var C=A-G,B=H-F;
if(!C&&!B){return 0
}return(180+AP.atan2(-B,-C)*180/Bs+360)%360
}return A1.angle(A,H,E,D)-A1.angle(G,F,E,D)
},A1.rad=function(A){return A%360*Bs/180
},A1.deg=function(A){return A*180/Bs%360
},A1.snapTo=function(A,E,D){D=A1.is(D,"finite")?D:10;
if(A1.is(A,Bm)){var C=A.length;
while(C--){if(AJ(A[C]-E)<=D){return A[C]
}}}else{A=+A;
var B=E%A;
if(B<D){return E-B
}if(B>A-D){return E-B+A
}}return E
};
var BA=A1.createUUID=function(B,A){return function(){return"xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(B,A).toUpperCase()
}
}(/[xy]/g,function(B){var A=AP.random()*16|0,C=B=="x"?A:A&3|8;
return C.toString(16)
});
A1.setWindow=function(A){eve("setWindow",A1,Ao.win,A),Ao.win=A,Ao.doc=Ao.win.document,initWin&&initWin(Ao.win)
};
var A9=function(A){if(A1.vml){var G=/^\s+|\s+$/g,F;
try{var E=new ActiveXObject("htmlfile");
E.write("<body>"),E.close(),F=E.body
}catch(D){F=createPopup().document.body
}var C=F.createTextRange();
A9=A0(function(I){try{F.style.color=AZ(I).replace(G,Ac);
var H=C.queryCommandValue("ForeColor");
H=(H&255)<<16|H&65280|(H&16711680)>>>16;
return"#"+("000000"+H.toString(16)).slice(-6)
}catch(J){return"none"
}})
}else{var B=Ao.doc.createElement("i");
B.title="Raphaël Colour Picker",B.style.display="none",Ao.doc.body.appendChild(B),A9=A0(function(H){B.style.color=H;
return Ao.doc.defaultView.getComputedStyle(B,Ac).getPropertyValue("color")
})
}return A9(A)
},A8=function(){return"hsb("+[this.h,this.s,this.b]+")"
},A7=function(){return"hsl("+[this.h,this.s,this.l]+")"
},A6=function(){return this.hex
},A4=function(A,D,C){D==null&&A1.is(A,"object")&&"r" in A&&"g" in A&&"b" in A&&(C=A.b,D=A.g,A=A.r);
if(D==null&&A1.is(A,Bo)){var B=A1.getRGB(A);
A=B.r,D=B.g,C=B.b
}if(A>1||D>1||C>1){A/=255,D/=255,C/=255
}return[A,D,C]
},A3=function(A,E,D,C){A*=255,E*=255,D*=255;
var B={r:A,g:E,b:D,hex:A1.rgb(A,E,D),toString:A6};
A1.is(C,"finite")&&(B.opacity=C);
return B
};
A1.color=function(A){var B;
A1.is(A,"object")&&"h" in A&&"s" in A&&"b" in A?(B=A1.hsb2rgb(A),A.r=B.r,A.g=B.g,A.b=B.b,A.hex=B.hex):A1.is(A,"object")&&"h" in A&&"s" in A&&"l" in A?(B=A1.hsl2rgb(A),A.r=B.r,A.g=B.g,A.b=B.b,A.hex=B.hex):(A1.is(A,"string")&&(A=A1.getRGB(A)),A1.is(A,"object")&&"r" in A&&"g" in A&&"b" in A?(B=A1.rgb2hsl(A),A.h=B.h,A.s=B.s,A.l=B.l,B=A1.rgb2hsb(A),A.v=B.b):(A={hex:"none"},crl.r=A.g=A.b=A.h=A.s=A.v=A.l=-1)),A.toString=A6;
return A
},A1.hsb2rgb=function(I,H,G,F){this.is(I,"object")&&"h" in I&&"s" in I&&"b" in I&&(G=I.b,H=I.s,I=I.h,F=I.o),I*=360;
var E,D,C,B,A;
I=I%360/60,A=G*H,B=A*(1-AJ(I%2-1)),E=D=C=G-A,I=~~I,E+=[A,B,0,0,B,A][I],D+=[B,A,A,B,0,0][I],C+=[0,0,B,A,A,B][I];
return A3(E,D,C,F)
},A1.hsl2rgb=function(I,H,G,F){this.is(I,"object")&&"h" in I&&"s" in I&&"l" in I&&(G=I.l,H=I.s,I=I.h);
if(I>1||H>1||G>1){I/=360,H/=100,G/=100
}I*=360;
var E,D,C,B,A;
I=I%360/60,A=2*H*(G<0.5?G:1-G),B=A*(1-AJ(I%2-1)),E=D=C=G-A/2,I=~~I,E+=[A,B,0,0,B,A][I],D+=[B,A,A,B,0,0][I],C+=[0,0,B,A,A,B][I];
return A3(E,D,C,F)
},A1.rgb2hsb=function(B,A,G){G=A4(B,A,G),B=G[0],A=G[1],G=G[2];
var F,E,D,C;
D=AN(B,A,G),C=D-AL(B,A,G),F=C==0?null:D==B?(A-G)/C:D==A?(G-B)/C+2:(B-A)/C+4,F=(F+360)%6*60/360,E=C==0?0:C/D;
return{h:F,s:E,b:D,toString:A8}
},A1.rgb2hsl=function(I,H,G){G=A4(I,H,G),I=G[0],H=G[1],G=G[2];
var F,E,D,C,B,A;
C=AN(I,H,G),B=AL(I,H,G),A=C-B,F=A==0?null:C==I?(H-G)/A:C==H?(G-I)/A+2:(I-H)/A+4,F=(F+360)%6*60/360,D=(C+B)/2,E=A==0?0:D<0.5?A/(2*D):A/(2-2*D);
return{h:F,s:E,l:D,toString:A7}
},A1._path2string=function(){return this.join(",").replace(BF,"$1")
};
var Ax=A1._preload=function(B,A){var C=Ao.doc.createElement("img");
C.style.cssText="position:absolute;left:-9999em;top-9999em",C.onload=function(){A.call(this),this.onload=null,Ao.doc.body.removeChild(this)
},C.onerror=function(){Ao.doc.body.removeChild(this)
},Ao.doc.body.appendChild(C),C.src=B
};
A1.getRGB=A0(function(I){if(!I||!!((I=AZ(I)).indexOf("-")+1)){return{r:-1,g:-1,b:-1,hex:"none",error:1,toString:Aw}
}if(I=="none"){return{r:-1,g:-1,b:-1,hex:"none",toString:Aw}
}!BH[Aq](I.toLowerCase().substring(0,2))&&I.charAt()!="#"&&(I=A9(I));
var H,G,F,E,D,C,B,A=I.match(Bb);
if(A){A[2]&&(E=BR(A[2].substring(5),16),F=BR(A[2].substring(3,5),16),G=BR(A[2].substring(1,3),16)),A[3]&&(E=BR((C=A[3].charAt(3))+C,16),F=BR((C=A[3].charAt(2))+C,16),G=BR((C=A[3].charAt(1))+C,16)),A[4]&&(B=A[4][AX](BJ),G=BT(B[0]),B[0].slice(-1)=="%"&&(G*=2.55),F=BT(B[1]),B[1].slice(-1)=="%"&&(F*=2.55),E=BT(B[2]),B[2].slice(-1)=="%"&&(E*=2.55),A[1].toLowerCase().slice(0,4)=="rgba"&&(D=BT(B[3])),B[3]&&B[3].slice(-1)=="%"&&(D/=100));
if(A[5]){B=A[5][AX](BJ),G=BT(B[0]),B[0].slice(-1)=="%"&&(G*=2.55),F=BT(B[1]),B[1].slice(-1)=="%"&&(F*=2.55),E=BT(B[2]),B[2].slice(-1)=="%"&&(E*=2.55),(B[0].slice(-3)=="deg"||B[0].slice(-1)=="°")&&(G/=360),A[1].toLowerCase().slice(0,4)=="hsba"&&(D=BT(B[3])),B[3]&&B[3].slice(-1)=="%"&&(D/=100);
return A1.hsb2rgb(G,F,E,D)
}if(A[6]){B=A[6][AX](BJ),G=BT(B[0]),B[0].slice(-1)=="%"&&(G*=2.55),F=BT(B[1]),B[1].slice(-1)=="%"&&(F*=2.55),E=BT(B[2]),B[2].slice(-1)=="%"&&(E*=2.55),(B[0].slice(-3)=="deg"||B[0].slice(-1)=="°")&&(G/=360),A[1].toLowerCase().slice(0,4)=="hsla"&&(D=BT(B[3])),B[3]&&B[3].slice(-1)=="%"&&(D/=100);
return A1.hsl2rgb(G,F,E,D)
}A={r:G,g:F,b:E,toString:Aw},A.hex="#"+(16777216|E|F<<8|G<<16).toString(16).slice(1),A1.is(D,"finite")&&(A.opacity=D);
return A
}return{r:-1,g:-1,b:-1,hex:"none",error:1,toString:Aw}
},A1),A1.hsb=A0(function(A,C,B){return A1.hsb2rgb(A,C,B).hex
}),A1.hsl=A0(function(A,C,B){return A1.hsl2rgb(A,C,B).hex
}),A1.rgb=A0(function(B,A,C){return"#"+(16777216|C|A<<8|B<<16).toString(16).slice(1)
}),A1.getColor=function(B){var A=this.getColor.start=this.getColor.start||{h:0,s:1,b:B||0.75},C=this.hsb2rgb(A.h,A.s,A.b);
A.h+=0.075,A.h>1&&(A.h=0,A.s-=0.2,A.s<=0&&(this.getColor.start={h:0,s:1,b:A.b}));
return C.hex
},A1.getColor.reset=function(){delete this.start
},A1.parsePathString=A0(function(A){if(!A){return null
}var C={a:7,c:6,h:1,l:2,m:2,r:4,q:4,s:4,t:2,v:1,z:0},B=[];
A1.is(A,Bm)&&A1.is(A[0],Bm)&&(B=Ap(A)),B.length||AZ(A).replace(BD,function(E,D,H){var G=[],F=D.toLowerCase();
H.replace(CD,function(J,I){I&&G.push(+I)
}),F=="m"&&G.length>2&&(B.push([D][Ah](G.splice(0,2))),F="l",D=D=="m"?"l":"L");
if(F=="r"){B.push([D][Ah](G))
}else{while(G.length>=C[F]){B.push([D][Ah](G.splice(0,C[F])));
if(!C[F]){break
}}}}),B.toString=A1._path2string;
return B
}),A1.parseTransformString=A0(function(A){if(!A){return null
}var C={r:3,s:4,t:2,m:6},B=[];
A1.is(A,Bm)&&A1.is(A[0],Bm)&&(B=Ap(A)),B.length||AZ(A).replace(BB,function(E,D,H){var G=[],F=AR.call(D);
H.replace(CD,function(J,I){I&&G.push(+I)
}),B.push([D][Ah](G))
}),B.toString=A1._path2string;
return B
}),A1.findDotsAtSegment=function(Y,X,W,V,U,T,S,R,Q){var P=1-Q,O=Bu(P,3),N=Bu(P,2),M=Q*Q,L=M*Q,K=O*Y+N*3*Q*W+P*3*Q*Q*U+L*S,J=O*X+N*3*Q*V+P*3*Q*Q*T+L*R,I=Y+2*Q*(W-Y)+M*(U-2*W+Y),H=X+2*Q*(V-X)+M*(T-2*V+X),G=W+2*Q*(U-W)+M*(S-2*U+W),F=V+2*Q*(T-V)+M*(R-2*T+V),E=P*Y+Q*W,D=P*X+Q*V,C=P*U+Q*S,B=P*T+Q*R,A=90-AP.atan2(I-G,H-F)*180/Bs;
(I>G||H<F)&&(A+=180);
return{x:K,y:J,m:{x:I,y:H},n:{x:G,y:F},start:{x:E,y:D},end:{x:C,y:B},alpha:A}
};
var Ar=A0(function(K){if(!K){return{x:0,y:0,width:0,height:0}
}K=B6(K);
var J=0,I=0,H=[],G=[],F;
for(var E=0,D=K.length;
E<D;
E++){F=K[E];
if(F[0]=="M"){J=F[1],I=F[2],H.push(J),G.push(I)
}else{var C=B7(J,I,F[1],F[2],F[3],F[4],F[5],F[6]);
H=H[Ah](C.min.x,C.max.x),G=G[Ah](C.min.y,C.max.y),J=F[5],I=F[6]
}}var B=AL[Ai](0,H),A=AL[Ai](0,G);
return{x:B,y:A,width:AN[Ai](0,H)-B,height:AN[Ai](0,G)-A}
},null,function(A){return{x:A.x,y:A.y,width:A.width,height:A.height}
}),Ap=function(A){var F=[];
if(!A1.is(A,Bm)||!A1.is(A&&A[0],Bm)){A=A1.parsePathString(A)
}for(var E=0,D=A.length;
E<D;
E++){F[E]=[];
for(var C=0,B=A[E].length;
C<B;
C++){F[E][C]=A[E][C]
}}F.toString=A1._path2string;
return F
},An=A1._pathToRelative=A0(function(P){if(!A1.is(P,Bm)||!A1.is(P&&P[0],Bm)){P=A1.parsePathString(P)
}var O=[],N=0,M=0,L=0,K=0,J=0;
P[0][0]=="M"&&(N=P[0][1],M=P[0][2],L=N,K=M,J++,O.push(["M",N,M]));
for(var I=J,H=P.length;
I<H;
I++){var G=O[I]=[],F=P[I];
if(F[0]!=AR.call(F[0])){G[0]=AR.call(F[0]);
switch(G[0]){case"a":G[1]=F[1],G[2]=F[2],G[3]=F[3],G[4]=F[4],G[5]=F[5],G[6]=+(F[6]-N).toFixed(3),G[7]=+(F[7]-M).toFixed(3);
break;
case"v":G[1]=+(F[1]-M).toFixed(3);
break;
case"m":L=F[1],K=F[2];
default:for(var E=1,D=F.length;
E<D;
E++){G[E]=+(F[E]-(E%2?N:M)).toFixed(3)
}}}else{G=O[I]=[],F[0]=="m"&&(L=F[1]+N,K=F[2]+M);
for(var C=0,B=F.length;
C<B;
C++){O[I][C]=F[C]
}}var A=O[I].length;
switch(O[I][0]){case"z":N=L,M=K;
break;
case"h":N+=+O[I][A-1];
break;
case"v":M+=+O[I][A-1];
break;
default:N+=+O[I][A-2],M+=+O[I][A-1]
}}O.toString=A1._path2string;
return O
},0,Ap),CC=A1._pathToAbsolute=A0(function(P){if(!A1.is(P,Bm)||!A1.is(P&&P[0],Bm)){P=A1.parsePathString(P)
}if(!P||!P.length){return[["M",0,0]]
}var O=[],N=0,M=0,L=0,K=0,J=0;
P[0][0]=="M"&&(N=+P[0][1],M=+P[0][2],L=N,K=M,J++,O[0]=["M",N,M]);
for(var I,H,G=J,F=P.length;
G<F;
G++){O.push(I=[]),H=P[G];
if(H[0]!=BP.call(H[0])){I[0]=BP.call(H[0]);
switch(I[0]){case"A":I[1]=H[1],I[2]=H[2],I[3]=H[3],I[4]=H[4],I[5]=H[5],I[6]=+(H[6]+N),I[7]=+(H[7]+M);
break;
case"V":I[1]=+H[1]+M;
break;
case"H":I[1]=+H[1]+N;
break;
case"R":var E=[N,M][Ah](H.slice(1));
for(var D=2,C=E.length;
D<C;
D++){E[D]=+E[D]+N,E[++D]=+E[D]+M
}O.pop(),O=O[Ah](Au(E));
break;
case"M":L=+H[1]+N,K=+H[2]+M;
default:for(D=1,C=H.length;
D<C;
D++){I[D]=+H[D]+(D%2?N:M)
}}}else{if(H[0]=="R"){E=[N,M][Ah](H.slice(1)),O.pop(),O=O[Ah](Au(E)),I=["R"][Ah](H.slice(-2))
}else{for(var B=0,A=H.length;
B<A;
B++){I[B]=H[B]
}}}switch(I[0]){case"Z":N=L,M=K;
break;
case"H":N=I[1];
break;
case"V":M=I[1];
break;
case"M":L=I[I.length-2],K=I[I.length-1];
default:N=I[I.length-2],M=I[I.length-1]
}}O.toString=A1._path2string;
return O
},null,Ap),CB=function(B,A,D,C){return[B,A,D,C,D,C]
},CA=function(B,A,H,G,F,E){var D=1/3,C=2/3;
return[D*B+C*H,D*A+C*G,D*F+C*H,D*E+C*G,F,E]
},B9=function(Cq,Cp,Co,Cn,Cm,Cl,Ck,Cj,Ci,Ch){var Cg=Bs*120/180,Cf=Bs/180*(+Cm||0),Ce=[],Cd,Cc=A0(function(C,A,F){var E=C*AP.cos(F)-A*AP.sin(F),D=C*AP.sin(F)+A*AP.cos(F);
return{x:E,y:D}
});
if(!Ch){Cd=Cc(Cq,Cp,-Cf),Cq=Cd.x,Cp=Cd.y,Cd=Cc(Cj,Ci,-Cf),Cj=Cd.x,Ci=Cd.y;
var Cb=AP.cos(Bs/180*Cm),Ca=AP.sin(Bs/180*Cm),CZ=(Cq-Cj)/2,CY=(Cp-Ci)/2,CX=CZ*CZ/(Co*Co)+CY*CY/(Cn*Cn);
CX>1&&(CX=AP.sqrt(CX),Co=CX*Co,Cn=CX*Cn);
var CW=Co*Co,CV=Cn*Cn,CU=(Cl==Ck?-1:1)*AP.sqrt(AJ((CW*CV-CW*CY*CY-CV*CZ*CZ)/(CW*CY*CY+CV*CZ*CZ))),CT=CU*Co*CY/Cn+(Cq+Cj)/2,CS=CU*-Cn*CZ/Co+(Cp+Ci)/2,CR=AP.asin(((Cp-CS)/Cn).toFixed(9)),CQ=AP.asin(((Ci-CS)/Cn).toFixed(9));
CR=Cq<CT?Bs-CR:CR,CQ=Cj<CT?Bs-CQ:CQ,CR<0&&(CR=Bs*2+CR),CQ<0&&(CQ=Bs*2+CQ),Ck&&CR>CQ&&(CR=CR-Bs*2),!Ck&&CQ>CR&&(CQ=CQ-Bs*2)
}else{CR=Ch[0],CQ=Ch[1],CT=Ch[2],CS=Ch[3]
}var CP=CQ-CR;
if(AJ(CP)>Cg){var CO=CQ,CN=Cj,CM=Ci;
CQ=CR+Cg*(Ck&&CQ>CR?1:-1),Cj=CT+Co*AP.cos(CQ),Ci=CS+Cn*AP.sin(CQ),Ce=B9(Cj,Ci,Co,Cn,Cm,0,Ck,CN,CM,[CQ,CO,CT,CS])
}CP=CQ-CR;
var CL=AP.cos(CR),CK=AP.sin(CR),CJ=AP.cos(CQ),CI=AP.sin(CQ),CH=AP.tan(CP/4),CG=4/3*Co*CH,CF=4/3*Cn*CH,z=[Cq,Cp],w=[Cq+CG*CK,Cp-CF*CL],s=[Cj+CG*CI,Ci-CF*CJ],n=[Cj,Ci];
w[0]=2*z[0]-w[0],w[1]=2*z[1]-w[1];
if(Ch){return[w,s,n][Ah](Ce)
}Ce=[w,s,n][Ah](Ce).join()[AX](",");
var Z=[];
for(var Y=0,B=Ce.length;
Y<B;
Y++){Z[Y]=Y%2?Cc(Ce[Y-1],Ce[Y],Cf).y:Cc(Ce[Y],Ce[Y+1],Cf).x
}return Z
},B8=function(J,I,H,G,F,E,D,C,B){var A=1-B;
return{x:Bu(A,3)*J+Bu(A,2)*3*B*H+A*3*B*B*F+Bu(B,3)*D,y:Bu(A,3)*I+Bu(A,2)*3*B*G+A*3*B*B*E+Bu(B,3)*C}
},B7=A0(function(P,O,N,M,L,K,J,I){var H=L-2*N+P-(J-2*L+N),G=2*(N-P)-2*(L-N),F=P-N,E=(-G+AP.sqrt(G*G-4*H*F))/2/H,D=(-G-AP.sqrt(G*G-4*H*F))/2/H,C=[O,I],B=[P,J],A;
AJ(E)>"1e12"&&(E=0.5),AJ(D)>"1e12"&&(D=0.5),E>0&&E<1&&(A=B8(P,O,N,M,L,K,J,I,E),B.push(A.x),C.push(A.y)),D>0&&D<1&&(A=B8(P,O,N,M,L,K,J,I,D),B.push(A.x),C.push(A.y)),H=K-2*M+O-(I-2*K+M),G=2*(M-O)-2*(K-M),F=O-M,E=(-G+AP.sqrt(G*G-4*H*F))/2/H,D=(-G-AP.sqrt(G*G-4*H*F))/2/H,AJ(E)>"1e12"&&(E=0.5),AJ(D)>"1e12"&&(D=0.5),E>0&&E<1&&(A=B8(P,O,N,M,L,K,J,I,E),B.push(A.x),C.push(A.y)),D>0&&D<1&&(A=B8(P,O,N,M,L,K,J,I,D),B.push(A.x),C.push(A.y));
return{min:{x:AL[Ai](0,B),y:AL[Ai](0,C)},max:{x:AN[Ai](0,B),y:AN[Ai](0,C)}}
}),B6=A1._path2curve=A0(function(O,N){var M=CC(O),L=N&&CC(N),K={x:0,y:0,bx:0,by:0,X:0,Y:0,qx:null,qy:null},J={x:0,y:0,bx:0,by:0,X:0,Y:0,qx:null,qy:null},I=function(Q,P){var S,R;
if(!Q){return["C",P.x,P.y,P.x,P.y,P.x,P.y]
}!(Q[0] in {T:1,Q:1})&&(P.qx=P.qy=null);
switch(Q[0]){case"M":P.X=Q[1],P.Y=Q[2];
break;
case"A":Q=["C"][Ah](B9[Ai](0,[P.x,P.y][Ah](Q.slice(1))));
break;
case"S":S=P.x+(P.x-(P.bx||P.x)),R=P.y+(P.y-(P.by||P.y)),Q=["C",S,R][Ah](Q.slice(1));
break;
case"T":P.qx=P.x+(P.x-(P.qx||P.x)),P.qy=P.y+(P.y-(P.qy||P.y)),Q=["C"][Ah](CA(P.x,P.y,P.qx,P.qy,Q[1],Q[2]));
break;
case"Q":P.qx=Q[1],P.qy=Q[2],Q=["C"][Ah](CA(P.x,P.y,Q[1],Q[2],Q[3],Q[4]));
break;
case"L":Q=["C"][Ah](CB(P.x,P.y,Q[1],Q[2]));
break;
case"H":Q=["C"][Ah](CB(P.x,P.y,Q[1],P.y));
break;
case"V":Q=["C"][Ah](CB(P.x,P.y,P.x,Q[1]));
break;
case"Z":Q=["C"][Ah](CB(P.x,P.y,P.X,P.Y))
}return Q
},H=function(Q,P){if(Q[P].length>7){Q[P].shift();
var R=Q[P];
while(R.length){Q.splice(P++,0,["C"][Ah](R.splice(0,6)))
}Q.splice(P,1),E=AN(M.length,L&&L.length||0)
}},G=function(Q,P,T,S,R){Q&&P&&Q[R][0]=="M"&&P[R][0]!="M"&&(P.splice(R,0,["M",S.x,S.y]),T.bx=0,T.by=0,T.x=Q[R][1],T.y=Q[R][2],E=AN(M.length,L&&L.length||0))
};
for(var F=0,E=AN(M.length,L&&L.length||0);
F<E;
F++){M[F]=I(M[F],K),H(M,F),L&&(L[F]=I(L[F],J)),L&&H(L,F),G(M,L,K,J,F),G(L,M,J,K,F);
var D=M[F],C=L&&L[F],B=D.length,A=L&&C.length;
K.x=D[B-2],K.y=D[B-1],K.bx=BT(D[B-4])||K.x,K.by=BT(D[B-3])||K.y,J.bx=L&&(BT(C[A-4])||J.x),J.by=L&&(BT(C[A-3])||J.y),J.x=L&&C[A-2],J.y=L&&C[A-1]
}return L?[M,L]:M
},null,Ap),B5=A1._parseDots=A0(function(J){var I=[];
for(var H=0,G=J.length;
H<G;
H++){var F={},E=J[H].match(/^([^:]*):?([\d\.]*)/);
F.color=A1.getRGB(E[1]);
if(F.color.error){return null
}F.color=F.color.hex,E[2]&&(F.offset=E[2]+"%"),I.push(F)
}for(H=1,G=I.length-1;
H<G;
H++){if(!I[H].offset){var D=BT(I[H-1].offset||0),C=0;
for(var B=H+1;
B<G;
B++){if(I[B].offset){C=I[B].offset;
break
}}C||(C=100,B=G),C=BT(C);
var A=(C-D)/(B-H+1);
for(;
H<B;
H++){D+=A,I[H].offset=D+"%"
}}}return I
}),B4=A1._tear=function(B,A){B==A.top&&(A.top=B.prev),B==A.bottom&&(A.bottom=B.next),B.next&&(B.next.prev=B.prev),B.prev&&(B.prev.next=B.next)
},B3=A1._tofront=function(B,A){A.top!==B&&(B4(B,A),B.next=null,B.prev=A.top,A.top.next=B,A.top=B)
},B2=A1._toback=function(B,A){A.bottom!==B&&(B4(B,A),B.next=A.bottom,B.prev=null,A.bottom.prev=B,A.bottom=B)
},B1=A1._insertafter=function(B,A,C){B4(B,C),A==C.top&&(C.top=B),A.next&&(A.next.prev=B),B.next=A.next,B.prev=A,A.next=B
},B0=A1._insertbefore=function(B,A,C){B4(B,C),A==C.bottom&&(C.bottom=B),A.prev&&(A.prev.next=B),B.prev=A.prev,A.prev=B,B.next=A
},Bz=function(A){return function(){throw new Error("Raphaël: you are calling to method “"+A+"” of removed object")
}
},By=A1._extractTransform=function(V,U){if(U==null){return V._.transform
}U=AZ(U).replace(/\.{3}|\u2026/g,V._.transform||Ac);
var T=A1.parseTransformString(U),S=0,R=0,Q=0,P=1,O=1,N=V._,M=new Bv;
N.transform=T||[];
if(T){for(var L=0,K=T.length;
L<K;
L++){var J=T[L],I=J.length,H=AZ(J[0]).toLowerCase(),G=J[0]!=H,F=G?M.invert():0,E,D,C,B,A;
H=="t"&&I==3?G?(E=F.x(0,0),D=F.y(0,0),C=F.x(J[1],J[2]),B=F.y(J[1],J[2]),M.translate(C-E,B-D)):M.translate(J[1],J[2]):H=="r"?I==2?(A=A||V.getBBox(1),M.rotate(J[1],A.x+A.width/2,A.y+A.height/2),S+=J[1]):I==4&&(G?(C=F.x(J[2],J[3]),B=F.y(J[2],J[3]),M.rotate(J[1],C,B)):M.rotate(J[1],J[2],J[3]),S+=J[1]):H=="s"?I==2||I==3?(A=A||V.getBBox(1),M.scale(J[1],J[I-1],A.x+A.width/2,A.y+A.height/2),P*=J[1],O*=J[I-1]):I==5&&(G?(C=F.x(J[3],J[4]),B=F.y(J[3],J[4]),M.scale(J[1],J[2],C,B)):M.scale(J[1],J[2],J[3],J[4]),P*=J[1],O*=J[2]):H=="m"&&I==7&&M.add(J[1],J[2],J[3],J[4],J[5],J[6]),N.dirtyT=1,V.matrix=M
}}V.matrix=M,N.sx=P,N.sy=O,N.deg=S,N.dx=R=M.e,N.dy=Q=M.f,P==1&&O==1&&!S&&N.bbox?(N.bbox.x+=+R,N.bbox.y+=+Q):N.dirtyT=1
},Bx=function(B){var A=B[0];
switch(A.toLowerCase()){case"t":return[A,0,0];
case"m":return[A,1,0,0,1,0,0];
case"r":return B.length==4?[A,0,B[2],B[3]]:[A,0];
case"s":return B.length==5?[A,1,1,B[3],B[4]]:B.length==3?[A,1,1]:[A,1]
}},Bw=A1._equaliseTransform=function(J,I){I=AZ(I).replace(/\.{3}|\u2026/g,J),J=A1.parseTransformString(J)||[],I=A1.parseTransformString(I)||[];
var H=AN(J.length,I.length),G=[],F=[],E=0,D,C,B,A;
for(;
E<H;
E++){B=J[E]||Bx(I[E]),A=I[E]||Bx(B);
if(B[0]!=A[0]||B[0].toLowerCase()=="r"&&(B[2]!=A[2]||B[3]!=A[3])||B[0].toLowerCase()=="s"&&(B[3]!=A[3]||B[4]!=A[4])){return 
}G[E]=[],F[E]=[];
for(D=0,C=AN(B.length,A.length);
D<C;
D++){D in B&&(G[E][D]=B[D]),D in A&&(F[E][D]=A[D])
}}return{from:G,to:F}
};
A1._getContainer=function(A,E,D,C){var B;
B=C==null&&!A1.is(A,"object")?Ao.doc.getElementById(A):A;
if(B!=null){if(B.tagName){return E==null?{container:B,width:B.style.pixelWidth||B.offsetWidth,height:B.style.pixelHeight||B.offsetHeight}:{container:B,width:E,height:D}
}return{container:1,x:A,y:E,width:D,height:C}
}},A1.pathToRelative=An,A1._engine={},A1.path2curve=B6,A1.matrix=function(B,A,F,E,D,C){return new Bv(B,A,F,E,D,C)
},function(A){function B(E){var D=AP.sqrt(C(E));
E[0]&&(E[0]/=D),E[1]&&(E[1]/=D)
}function C(D){return D[0]*D[0]+D[1]*D[1]
}A.add=function(P,O,N,M,L,K){var J=[[],[],[]],I=[[this.a,this.c,this.e],[this.b,this.d,this.f],[0,0,1]],H=[[P,N,L],[O,M,K],[0,0,1]],G,F,E,D;
P&&P instanceof Bv&&(H=[[P.a,P.c,P.e],[P.b,P.d,P.f],[0,0,1]]);
for(G=0;
G<3;
G++){for(F=0;
F<3;
F++){D=0;
for(E=0;
E<3;
E++){D+=I[G][E]*H[E][F]
}J[G][F]=D
}}this.a=J[0][0],this.b=J[1][0],this.c=J[0][1],this.d=J[1][1],this.e=J[0][2],this.f=J[1][2]
},A.invert=function(){var E=this,D=E.a*E.d-E.b*E.c;
return new Bv(E.d/D,-E.b/D,-E.c/D,E.a/D,(E.c*E.f-E.d*E.e)/D,(E.b*E.e-E.a*E.f)/D)
},A.clone=function(){return new Bv(this.a,this.b,this.c,this.d,this.e,this.f)
},A.translate=function(E,D){this.add(1,0,0,1,E,D)
},A.scale=function(E,D,G,F){D==null&&(D=E),(G||F)&&this.add(1,0,0,1,G,F),this.add(E,0,0,D,0,0),(G||F)&&this.add(1,0,0,1,-G,-F)
},A.rotate=function(D,H,G){D=A1.rad(D),H=H||0,G=G||0;
var F=+AP.cos(D).toFixed(9),E=+AP.sin(D).toFixed(9);
this.add(F,E,-E,F,H,G),this.add(1,0,0,1,-H,-G)
},A.x=function(E,D){return E*this.a+D*this.c+this.e
},A.y=function(E,D){return E*this.b+D*this.d+this.f
},A.get=function(D){return +this[AZ.fromCharCode(97+D)].toFixed(4)
},A.toString=function(){return A1.svg?"matrix("+[this.get(0),this.get(1),this.get(2),this.get(3),this.get(4),this.get(5)].join()+")":[this.get(0),this.get(2),this.get(1),this.get(3),0,0].join()
},A.toFilter=function(){return"progid:DXImageTransform.Microsoft.Matrix(M11="+this.get(0)+", M12="+this.get(2)+", M21="+this.get(1)+", M22="+this.get(3)+", Dx="+this.get(4)+", Dy="+this.get(5)+", sizingmethod='auto expand')"
},A.offset=function(){return[this.e.toFixed(4),this.f.toFixed(4)]
},A.split=function(){var D={};
D.dx=this.e,D.dy=this.f;
var G=[[this.a,this.c],[this.b,this.d]];
D.scalex=AP.sqrt(C(G[0])),B(G[0]),D.shear=G[0][0]*G[1][0]+G[0][1]*G[1][1],G[1]=[G[1][0]-G[0][0]*D.shear,G[1][1]-G[0][1]*D.shear],D.scaley=AP.sqrt(C(G[1])),B(G[1]),D.shear/=D.scaley;
var F=-G[0][1],E=G[1][1];
E<0?(D.rotate=A1.deg(AP.acos(E)),F<0&&(D.rotate=360-D.rotate)):D.rotate=A1.deg(AP.asin(F)),D.isSimple=!+D.shear.toFixed(9)&&(D.scalex.toFixed(9)==D.scaley.toFixed(9)||!D.rotate),D.isSuperSimple=!+D.shear.toFixed(9)&&D.scalex.toFixed(9)==D.scaley.toFixed(9)&&!D.rotate,D.noRotation=!+D.shear.toFixed(9)&&!D.rotate;
return D
},A.toTransformString=function(E){var D=E||this[AX]();
return D.isSimple?"t"+[D.dx,D.dy]+"s"+[D.scalex,D.scaley,0,0]+"r"+[D.rotate,0,0]:"m"+[this.get(0),this.get(1),this.get(2),this.get(3),this.get(4),this.get(5)]
}
}(Bv.prototype);
var Bt=navigator.userAgent.match(/Version\/(.*?)\s/)||navigator.userAgent.match(/Chrome\/(\d+)/);
navigator.vendor=="Apple Computer, Inc."&&(Bt&&Bt[1]<4||navigator.platform.slice(0,2)=="iP")||navigator.vendor=="Google Inc."&&Bt&&Bt[1]<8?Ak.safari=function(){var A=this.rect(-99,-99,this.width+99,this.height+99).attr({stroke:"none"});
setTimeout(function(){A.remove()
})
}:Ak.safari=BQ;
var Br=function(){this.returnValue=!1
},Bp=function(){return this.originalEvent.preventDefault()
},Bn=function(){this.cancelBubble=!0
},Bl=function(){return this.originalEvent.stopPropagation()
},Bj=function(){if(Ao.doc.addEventListener){return function(B,A,F,E){var D=Af&&AT[A]?AT[A]:A,C=function(M){var L=Ao.doc.documentElement.scrollTop||Ao.doc.body.scrollTop,K=Ao.doc.documentElement.scrollLeft||Ao.doc.body.scrollLeft,J=M.clientX+K,I=M.clientY+L;
if(Af&&AT[Aq](A)){for(var H=0,G=M.targetTouches&&M.targetTouches.length;
H<G;
H++){if(M.targetTouches[H].target==B){var N=M;
M=M.targetTouches[H],M.originalEvent=N,M.preventDefault=Bp,M.stopPropagation=Bl;
break
}}}return F.call(E,M,J,I)
};
B.addEventListener(D,C,!1);
return function(){B.removeEventListener(D,C,!1);
return !0
}
}
}if(Ao.doc.attachEvent){return function(B,A,F,E){var D=function(H){H=H||Ao.win.event;
var G=Ao.doc.documentElement.scrollTop||Ao.doc.body.scrollTop,K=Ao.doc.documentElement.scrollLeft||Ao.doc.body.scrollLeft,J=H.clientX+K,I=H.clientY+G;
H.preventDefault=H.preventDefault||Br,H.stopPropagation=H.stopPropagation||Bn;
return F.call(E,H,J,I)
};
B.attachEvent("on"+A,D);
var C=function(){B.detachEvent("on"+A,D);
return !0
};
return C
}
}}(),Bh=[],Bf=function(N){var M=N.clientX,L=N.clientY,K=Ao.doc.documentElement.scrollTop||Ao.doc.body.scrollTop,J=Ao.doc.documentElement.scrollLeft||Ao.doc.body.scrollLeft,I,H=Bh.length;
while(H--){I=Bh[H];
if(Af){var G=N.touches.length,F;
while(G--){F=N.touches[G];
if(F.identifier==I.el._drag.id){M=F.clientX,L=F.clientY,(N.originalEvent?N.originalEvent:N).preventDefault();
break
}}}else{N.preventDefault()
}var E=I.el.node,D,C=E.nextSibling,B=E.parentNode,A=E.style.display;
Ao.win.opera&&B.removeChild(E),E.style.display="none",D=I.el.paper.getElementByPoint(M,L),E.style.display=A,Ao.win.opera&&(C?B.insertBefore(E,C):B.appendChild(E)),D&&eve("drag.over."+I.el.id,I.el,D),M+=J,L+=K,eve("drag.move."+I.el.id,I.move_scope||I.el,M-I.el._drag.x,L-I.el._drag.y,M,L,N)
}},CE=function(A){A1.unmousemove(Bf).unmouseup(CE);
var C=Bh.length,B;
while(C--){B=Bh[C],B.el._drag={},eve("drag.end."+B.el.id,B.end_scope||B.start_scope||B.move_scope||B.el,A)
}Bh=[]
},BZ=A1.el={};
for(var Ag=AV.length;
Ag--;
){(function(A){A1[A]=BZ[A]=function(C,B){A1.is(C,"function")&&(this.events=this.events||[],this.events.push({name:A,f:C,unbind:Bj(this.shape||this.node||Ao.doc,A,C,B||this)}));
return this
},A1["un"+A]=BZ["un"+A]=function(B){var D=this.events,C=D.length;
while(C--){if(D[C].name==A&&D[C].f==B){D[C].unbind(),D.splice(C,1),!D.length&&delete this.events;
return this
}}return this
}
})(AV[Ag])
}BZ.data=function(A,D){var C=BW[this.id]=BW[this.id]||{};
if(arguments.length==1){if(A1.is(A,"object")){for(var B in A){A[Aq](B)&&this.data(B,A[B])
}return this
}eve("data.get."+this.id,this,C[A],A);
return C[A]
}C[A]=D,eve("data.set."+this.id,this,D,A);
return this
},BZ.removeData=function(A){A==null?BW[this.id]={}:BW[this.id]&&delete BW[this.id][A];
return this
},BZ.hover=function(B,A,D,C){return this.mouseover(B,D).mouseout(A,C||D)
},BZ.unhover=function(B,A){return this.unmouseover(B).unmouseout(A)
},BZ.drag=function(A,G,F,E,D,C){function B(J){(J.originalEvent||J).preventDefault();
var I=Ao.doc.documentElement.scrollTop||Ao.doc.body.scrollTop,H=Ao.doc.documentElement.scrollLeft||Ao.doc.body.scrollLeft;
this._drag.x=J.clientX+H,this._drag.y=J.clientY+I,this._drag.id=J.identifier,!Bh.length&&A1.mousemove(Bf).mouseup(CE),Bh.push({el:this,move_scope:E,start_scope:D,end_scope:C}),G&&eve.on("drag.start."+this.id,G),A&&eve.on("drag.move."+this.id,A),F&&eve.on("drag.end."+this.id,F),eve("drag.start."+this.id,D||E||this,J.clientX+H,J.clientY+I,J)
}this._drag={},this.mousedown(B);
return this
},BZ.onDragOver=function(A){A?eve.on("drag.over."+this.id,A):eve.unbind("drag.over."+this.id)
},BZ.undrag=function(){var A=Bh.length;
while(A--){Bh[A].el==this&&(A1.unmousedown(Bh[A].start),Bh.splice(A++,1),eve.unbind("drag.*."+this.id))
}!Bh.length&&A1.unmousemove(Bf).unmouseup(CE)
},Ak.circle=function(A,D,C){var B=A1._engine.circle(this,A||0,D||0,C||0);
this.__set__&&this.__set__.push(B);
return B
},Ak.rect=function(A,F,E,D,C){var B=A1._engine.rect(this,A||0,F||0,E||0,D||0,C||0);
this.__set__&&this.__set__.push(B);
return B
},Ak.ellipse=function(A,E,D,C){var B=A1._engine.ellipse(this,A||0,E||0,D||0,C||0);
this.__set__&&this.__set__.push(B);
return B
},Ak.path=function(A){A&&!A1.is(A,Bo)&&!A1.is(A[0],Bm)&&(A+=Ac);
var B=A1._engine.path(A1.format[Ai](A1,arguments),this);
this.__set__&&this.__set__.push(B);
return B
},Ak.image=function(A,F,E,D,C){var B=A1._engine.image(this,A||"about:blank",F||0,E||0,D||0,C||0);
this.__set__&&this.__set__.push(B);
return B
},Ak.text=function(A,D,C){var B=A1._engine.text(this,A||0,D||0,AZ(C));
this.__set__&&this.__set__.push(B);
return B
},Ak.set=function(A){!A1.is(A,"array")&&(A=Array.prototype.splice.call(arguments,0,arguments.length));
var B=new AC(A);
this.__set__&&this.__set__.push(B);
return B
},Ak.setStart=function(A){this.__set__=A||this.set()
},Ak.setFinish=function(B){var A=this.__set__;
delete this.__set__;
return A
},Ak.setSize=function(A,B){return A1._engine.setSize.call(this,A,B)
},Ak.setViewBox=function(A,E,D,C,B){return A1._engine.setViewBox.call(this,A,E,D,C,B)
},Ak.top=Ak.bottom=null,Ak.raphael=A1;
var Ae=function(I){var H=I.getBoundingClientRect(),G=I.ownerDocument,F=G.body,E=G.documentElement,D=E.clientTop||F.clientTop||0,C=E.clientLeft||F.clientLeft||0,B=H.top+(Ao.win.pageYOffset||E.scrollTop||F.scrollTop)-D,A=H.left+(Ao.win.pageXOffset||E.scrollLeft||F.scrollLeft)-C;
return{y:B,x:A}
};
Ak.getElementByPoint=function(B,A){var H=this,G=H.canvas,F=Ao.doc.elementFromPoint(B,A);
if(Ao.win.opera&&F.tagName=="svg"){var E=Ae(G),D=G.createSVGRect();
D.x=B-E.x,D.y=A-E.y,D.width=D.height=1;
var C=G.getIntersectionList(D,null);
C.length&&(F=C[C.length-1])
}if(!F){return null
}while(F.parentNode&&F!=G.parentNode&&!F.raphael){F=F.parentNode
}F==H.canvas.parentNode&&(F=G),F=F&&F.raphael?H.getById(F.raphaelid):null;
return F
},Ak.getById=function(B){var A=this.bottom;
while(A){if(A.id==B){return A
}A=A.next
}return null
},Ak.forEach=function(B,A){var C=this.bottom;
while(C){if(B.call(A,C)===!1){return this
}C=C.next
}return this
},BZ.getBBox=function(B){if(this.removed){return{}
}var A=this._;
if(B){if(A.dirty||!A.bboxwt){this.realPath=BI[this.type](this),A.bboxwt=Ar(this.realPath),A.bboxwt.toString=Ab,A.dirty=0
}return A.bboxwt
}if(A.dirty||A.dirtyT||!A.bbox){if(A.dirty||!this.realPath){A.bboxwt=0,this.realPath=BI[this.type](this)
}A.bbox=Ar(BG(this.realPath,this.matrix)),A.bbox.toString=Ab,A.dirty=A.dirtyT=0
}return A.bbox
},BZ.clone=function(){if(this.removed){return null
}var A=this.paper[this.type]().attr(this.attr());
this.__set__&&this.__set__.push(A);
return A
},BZ.glow=function(B){if(this.type=="text"){return null
}B=B||{};
var A={width:(B.width||10)+(+this.attr("stroke-width")||1),fill:B.fill||!1,opacity:B.opacity||0.5,offsetx:B.offsetx||0,offsety:B.offsety||0,color:B.color||"#000"},G=A.width/2,F=this.paper,E=F.set(),D=this.realPath||BI[this.type](this);
D=this.matrix?BG(D,this.matrix):D;
for(var C=1;
C<G+1;
C++){E.push(F.path(D).attr({stroke:A.color,fill:A.fill?A.color:"none","stroke-linejoin":"round","stroke-linecap":"round","stroke-width":+(A.width/G*C).toFixed(3),opacity:+(A.opacity/G).toFixed(3)}))
}return E.insertBefore(this).translate(A.offsetx,A.offsety)
};
var AY={},AW=function(Q,P,O,N,M,L,K,J,I){var H=0,G=100,F=[Q,P,O,N,M,L,K,J].join(),E=AY[F],D,C;
!E&&(AY[F]=E={data:[]}),E.timer&&clearTimeout(E.timer),E.timer=setTimeout(function(){delete AY[F]
},2000);
if(I!=null&&!E.precision){var B=AW(Q,P,O,N,M,L,K,J);
E.precision=~~B*10,E.data=[]
}G=E.precision||G;
for(var A=0;
A<G+1;
A++){E.data[A*G]?C=E.data[A*G]:(C=A1.findDotsAtSegment(Q,P,O,N,M,L,K,J,A/G),E.data[A*G]=C),A&&(H+=Bu(Bu(D.x-C.x,2)+Bu(D.y-C.y,2),0.5));
if(I!=null&&H>=I){return C
}D=C
}if(I==null){return H
}},AU=function(A,B){return function(O,N,M){O=B6(O);
var L,K,J,I,H="",G={},F,E=0;
for(var D=0,C=O.length;
D<C;
D++){J=O[D];
if(J[0]=="M"){L=+J[1],K=+J[2]
}else{I=AW(L,K,J[1],J[2],J[3],J[4],J[5],J[6]);
if(E+I>N){if(B&&!G.start){F=AW(L,K,J[1],J[2],J[3],J[4],J[5],J[6],N-E),H+=["C"+F.start.x,F.start.y,F.m.x,F.m.y,F.x,F.y];
if(M){return H
}G.start=H,H=["M"+F.x,F.y+"C"+F.n.x,F.n.y,F.end.x,F.end.y,J[5],J[6]].join(),E+=I,L=+J[5],K=+J[6];
continue
}if(!A&&!B){F=AW(L,K,J[1],J[2],J[3],J[4],J[5],J[6],N-E);
return{x:F.x,y:F.y,alpha:F.alpha}
}}E+=I,L=+J[5],K=+J[6]
}H+=J.shift()+J
}G.end=H,F=A?E:B?G:A1.findDotsAtSegment(L,K,J[0],J[1],J[2],J[3],J[4],J[5],1),F.alpha&&(F={x:F.x,y:F.y,alpha:F.alpha});
return F
}
},AS=AU(1),AQ=AU(),AO=AU(0,1);
A1.getTotalLength=AS,A1.getPointAtLength=AQ,A1.getSubpath=function(B,A,D){if(this.getTotalLength(B)-D<0.000001){return AO(B,A).end
}var C=AO(B,D,1);
return A?AO(C,A).end:C
},BZ.getTotalLength=function(){if(this.type=="path"){if(this.node.getTotalLength){return this.node.getTotalLength()
}return AS(this.attrs.path)
}},BZ.getPointAtLength=function(A){if(this.type=="path"){return AQ(this.attrs.path,A)
}},BZ.getSubpath=function(A,B){if(this.type=="path"){return A1.getSubpath(this.attrs.path,A,B)
}};
var AM=A1.easing_formulas={linear:function(A){return A
},"<":function(A){return Bu(A,1.7)
},">":function(A){return Bu(A,0.48)
},"<>":function(B){var A=0.48-B/1.04,H=AP.sqrt(0.1734+A*A),G=H-A,F=Bu(AJ(G),1/3)*(G<0?-1:1),E=-H-A,D=Bu(AJ(E),1/3)*(E<0?-1:1),C=F+D+0.5;
return(1-C)*3*C*C+C*C*C
},backIn:function(B){var A=1.70158;
return B*B*((A+1)*B-A)
},backOut:function(B){B=B-1;
var A=1.70158;
return B*B*((A+1)*B+A)+1
},elastic:function(A){if(A==!!A){return A
}return Bu(2,-10*A)*AP.sin((A-0.075)*2*Bs/0.3)+1
},bounce:function(B){var A=7.5625,D=2.75,C;
B<1/D?C=A*B*B:B<2/D?(B-=1.5/D,C=A*B*B+0.75):B<2.5/D?(B-=2.25/D,C=A*B*B+0.9375):(B-=2.625/D,C=A*B*B+0.984375);
return C
}};
AM.easeIn=AM["ease-in"]=AM["<"],AM.easeOut=AM["ease-out"]=AM[">"],AM.easeInOut=AM["ease-in-out"]=AM["<>"],AM["back-in"]=AM.backIn,AM["back-out"]=AM.backOut;
var AK=[],AI=window.requestAnimationFrame||window.webkitRequestAnimationFrame||window.mozRequestAnimationFrame||window.oRequestAnimationFrame||window.msRequestAnimationFrame||function(A){setTimeout(A,16)
},AH=function(){var X=+(new Date),W=0;
for(;
W<AK.length;
W++){var V=AK[W];
if(V.el.removed||V.paused){continue
}var U=X-V.start,T=V.ms,S=V.easing,R=V.from,Q=V.diff,P=V.to,O=V.t,N=V.el,M={},L,J={},I;
V.initstatus?(U=(V.initstatus*V.anim.top-V.prev)/(V.percent-V.prev)*T,V.status=V.initstatus,delete V.initstatus,V.stop&&AK.splice(W--,1)):V.status=(V.prev+(V.percent-V.prev)*(U/T))/V.anim.top;
if(U<0){continue
}if(U<T){var H=S(U/T);
for(var G in R){if(R[Aq](G)){switch(BL[G]){case Bq:L=+R[G]+H*T*Q[G];
break;
case"colour":L="rgb("+[AG(BX(R[G].r+H*T*Q[G].r)),AG(BX(R[G].g+H*T*Q[G].g)),AG(BX(R[G].b+H*T*Q[G].b))].join(",")+")";
break;
case"path":L=[];
for(var F=0,E=R[G].length;
F<E;
F++){L[F]=[R[G][F][0]];
for(var D=1,C=R[G][F].length;
D<C;
D++){L[F][D]=+R[G][F][D]+H*T*Q[G][F][D]
}L[F]=L[F].join(Aa)
}L=L.join(Aa);
break;
case"transform":if(Q[G].real){L=[];
for(F=0,E=R[G].length;
F<E;
F++){L[F]=[R[G][F][0]];
for(D=1,C=R[G][F].length;
D<C;
D++){L[F][D]=R[G][F][D]+H*T*Q[G][F][D]
}}}else{var B=function(A){return +R[G][A]+H*T*Q[G][A]
};
L=[["m",B(0),B(1),B(2),B(3),B(4),B(5)]]
}break;
case"csv":if(G=="clip-rect"){L=[],F=4;
while(F--){L[F]=+R[G][F]+H*T*Q[G][F]
}}break;
default:var K=[][Ah](R[G]);
L=[],F=N.paper.customAttributes[G].length;
while(F--){L[F]=+K[F]+H*T*Q[G][F]
}}M[G]=L
}}N.attr(M),function(Y,A,Z){setTimeout(function(){eve("anim.frame."+Y,A,Z)
})
}(N.id,N,V.anim)
}else{(function(A,Z,Y){setTimeout(function(){eve("anim.frame."+Z.id,Z,Y),eve("anim.finish."+Z.id,Z,Y),A1.is(A,"function")&&A.call(Z)
})
})(V.callback,N,V.anim),N.attr(P),AK.splice(W--,1);
if(V.repeat>1&&!V.next){for(I in P){P[Aq](I)&&(J[I]=V.totalOrigin[I])
}V.el.attr(J),AD(V.anim,V.el,V.anim.percents[0],null,V.totalOrigin,V.repeat-1)
}V.next&&!V.stop&&AD(V.anim,V.el,V.next,null,V.totalOrigin,V.repeat)
}}A1.svg&&N&&N.paper&&N.paper.safari(),AK.length&&AI(AH)
},AG=function(A){return A>255?255:A<0?0:A
};
BZ.animateWith=function(A,G,F,E,D,C){var B=F?A1.animation(F,E,D,C):G;
status=A.status(G);
return this.animate(B).status(B,status*G.ms/B.ms)
},BZ.onAnimation=function(A){A?eve.on("anim.frame."+this.id,A):eve.unbind("anim.frame."+this.id);
return this
},AE.prototype.delay=function(B){var A=new AE(this.anim,this.ms);
A.times=this.times,A.del=+B||0;
return A
},AE.prototype.repeat=function(B){var A=new AE(this.anim,this.ms);
A.del=this.del,A.times=AP.floor(AN(B,0))||1;
return A
},A1.animation=function(A,G,F,E){if(A instanceof AE){return A
}if(A1.is(F,"function")||!F){E=E||F||null,F=null
}A=Object(A),G=+G||0;
var D={},C,B;
for(B in A){A[Aq](B)&&BT(B)!=B&&BT(B)+"%"!=B&&(C=!0,D[B]=A[B])
}if(!C){return new AE(A,G)
}F&&(D.easing=F),E&&(D.callback=E);
return new AE({100:D},G)
},BZ.animate=function(A,F,E,D){var C=this;
if(C.removed){D&&D.call(C);
return C
}var B=A instanceof AE?A:A1.animation(A,F,E,D);
AD(B,C,B.percents[0],null,C.attr());
return C
},BZ.setTime=function(B,A){B&&A!=null&&this.status(B,AL(A,B.ms)/B.ms);
return this
},BZ.status=function(B,A){var F=[],E=0,D,C;
if(A!=null){AD(B,this,-1,AL(A,1));
return this
}D=AK.length;
for(;
E<D;
E++){C=AK[E];
if(C.el.id==this.id&&(!B||C.anim==B)){if(B){return C.status
}F.push({anim:C.anim,status:C.status})
}}if(B){return 0
}return F
},BZ.pause=function(B){for(var A=0;
A<AK.length;
A++){AK[A].el.id==this.id&&(!B||AK[A].anim==B)&&eve("anim.pause."+this.id,this,AK[A].anim)!==!1&&(AK[A].paused=!0)
}return this
},BZ.resume=function(B){for(var A=0;
A<AK.length;
A++){if(AK[A].el.id==this.id&&(!B||AK[A].anim==B)){var C=AK[A];
eve("anim.resume."+this.id,this,C.anim)!==!1&&(delete C.paused,this.status(C.anim,C.status))
}}return this
},BZ.stop=function(B){for(var A=0;
A<AK.length;
A++){AK[A].el.id==this.id&&(!B||AK[A].anim==B)&&eve("anim.stop."+this.id,this,AK[A].anim)!==!1&&AK.splice(A--,1)
}return this
},BZ.toString=function(){return"Raphaël’s object"
};
var AC=function(B){this.items=[],this.length=0,this.type="set";
if(B){for(var A=0,C=B.length;
A<C;
A++){B[A]&&(B[A].constructor==BZ.constructor||B[A].constructor==AC)&&(this[this.items.length]=this.items[this.items.length]=B[A],this.length++)
}}},AB=AC.prototype;
AB.push=function(){var B,A;
for(var D=0,C=arguments.length;
D<C;
D++){B=arguments[D],B&&(B.constructor==BZ.constructor||B.constructor==AC)&&(A=this.items.length,this[A]=this.items[A]=B,this.length++)
}return this
},AB.pop=function(){this.length&&delete this[this.length--];
return this.items.pop()
},AB.forEach=function(B,A){for(var D=0,C=this.items.length;
D<C;
D++){if(B.call(A,this.items[D],D)===!1){return this
}}return this
};
for(var AA in BZ){BZ[Aq](AA)&&(AB[AA]=function(A){return function(){var B=arguments;
return this.forEach(function(C){C[A][Ai](C,B)
})
}
}(AA))
}AB.attr=function(A,F){if(A&&A1.is(A,Bm)&&A1.is(A[0],"object")){for(var E=0,D=A.length;
E<D;
E++){this.items[E].attr(A[E])
}}else{for(var C=0,B=this.items.length;
C<B;
C++){this.items[C].attr(A,F)
}}return this
},AB.clear=function(){while(this.length){this.pop()
}},AB.splice=function(B,A,H){B=B<0?AN(this.length+B,0):B,A=AN(0,AL(this.length-B,A));
var G=[],F=[],E=[],D;
for(D=2;
D<arguments.length;
D++){E.push(arguments[D])
}for(D=0;
D<A;
D++){F.push(this[B+D])
}for(;
D<this.length-B;
D++){G.push(this[B+D])
}var C=E.length;
for(D=0;
D<C+G.length;
D++){this.items[B+D]=this[B+D]=D<C?E[D]:G[D-C]
}D=this.items.length=this.length-=A-C;
while(this[D]){delete this[D++]
}return new AC(F)
},AB.exclude=function(B){for(var A=0,C=this.length;
A<C;
A++){if(this[A]==B){this.splice(A,1);
return !0
}}},AB.animate=function(J,I,H,G){(A1.is(H,"function")||!H)&&(G=H||null);
var F=this.items.length,E=F,D,C=this,B;
if(!F){return this
}G&&(B=function(){!--F&&G.call(C)
}),H=A1.is(H,Bo)?H:B;
var A=A1.animation(J,I,H,B);
D=this.items[--E].animate(A);
while(E--){this.items[E]&&!this.items[E].removed&&this.items[E].animateWith(D,A)
}return this
},AB.insertAfter=function(B){var A=this.items.length;
while(A--){this.items[A].insertAfter(B)
}return this
},AB.getBBox=function(){var B=[],A=[],F=[],E=[];
for(var D=this.items.length;
D--;
){if(!this.items[D].removed){var C=this.items[D].getBBox();
B.push(C.x),A.push(C.y),F.push(C.x+C.width),E.push(C.y+C.height)
}}B=AL[Ai](0,B),A=AL[Ai](0,A);
return{x:B,y:A,width:AN[Ai](0,F)-B,height:AN[Ai](0,E)-A}
},AB.clone=function(B){B=new AC;
for(var A=0,C=this.items.length;
A<C;
A++){B.push(this.items[A].clone())
}return B
},AB.toString=function(){return"Raphaël‘s set"
},A1.registerFont=function(B){if(!B.face){return B
}this.fonts=this.fonts||{};
var A={w:B.w,face:{},glyphs:{}},G=B.face["font-family"];
for(var F in B.face){B.face[Aq](F)&&(A.face[F]=B.face[F])
}this.fonts[G]?this.fonts[G].push(A):this.fonts[G]=[A];
if(!B.svg){A.face["units-per-em"]=BR(B.face["units-per-em"],10);
for(var E in B.glyphs){if(B.glyphs[Aq](E)){var D=B.glyphs[E];
A.glyphs[E]={w:D.w,k:{},d:D.d&&"M"+D.d.replace(/[mlcxtrv]/g,function(H){return{l:"L",c:"C",x:"z",t:"m",r:"l",v:"c"}[H]||"M"
})+"z"};
if(D.k){for(var C in D.k){D[Aq](C)&&(A.glyphs[E].k[C]=D.k[C])
}}}}}return B
},Ak.getFont=function(J,I,H,G){G=G||"normal",H=H||"normal",I=+I||{normal:400,bold:700,lighter:300,bolder:800}[I]||400;
if(!!A1.fonts){var F=A1.fonts[J];
if(!F){var E=new RegExp("(^|\\s)"+J.replace(/[^\w\d\s+!~.:_-]/g,Ac)+"(\\s|$)","i");
for(var D in A1.fonts){if(A1.fonts[Aq](D)&&E.test(D)){F=A1.fonts[D];
break
}}}var C;
if(F){for(var B=0,A=F.length;
B<A;
B++){C=F[B];
if(C.face["font-weight"]==I&&(C.face["font-style"]==H||!C.face["font-style"])&&C.face["font-stretch"]==G){break
}}}return C
}},Ak.print=function(S,R,Q,P,O,N,M){N=N||"middle",M=AN(AL(M||0,1),-1);
var L=this.set(),K=AZ(Q)[AX](Ac),J=0,I=Ac,H;
A1.is(P,Q)&&(P=this.getFont(P));
if(P){H=(O||16)/P.face["units-per-em"];
var G=P.face.bbox[AX](Ay),F=+G[0],E=+G[1]+(N=="baseline"?G[3]-G[1]+ +P.face.descent:(G[3]-G[1])/2);
for(var D=0,C=K.length;
D<C;
D++){var B=D&&P.glyphs[K[D-1]]||{},A=P.glyphs[K[D]];
J+=D?(B.w||P.w)+(B.k&&B.k[K[D]]||0)+P.w*M:0,A&&A.d&&L.push(this.path(A.d).attr({fill:"#000",stroke:"none",transform:[["t",J*H,0]]}))
}L.transform(["...s",H,H,F,E,"t",(S-F)/H,(R-E)/H])
}return L
},A1.format=function(A,C){var B=A1.is(C,Bm)?[0][Ah](C):arguments;
A&&A1.is(A,Bo)&&B.length-1&&(A=A.replace(At,function(E,D){return B[++D]==null?Ac:B[D]
}));
return A||Ac
},A1.fullfill=function(){var B=/\{([^\}]+)\}/g,A=/(?:(?:^|\.)(.+?)(?=\[|\.|$|\()|\[('|")(.+?)\2\])(\(\))?/g,C=function(D,G,F){var E=F;
G.replace(A,function(I,H,L,K,J){H=H||K,E&&(H in E&&(E=E[H]),typeof E=="function"&&J&&(E=E()))
}),E=(E==null||E==F?D:E)+"";
return E
};
return function(D,E){return String(D).replace(B,function(G,F){return C(G,F,E)
})
}
}(),A1.ninja=function(){Am.was?Ao.win.Raphael=Am.is:delete Raphael;
return A1
},A1.st=AB,function(A,D,C){function B(){/in/.test(A.readyState)?setTimeout(B,9):A1.eve("DOMload")
}A.readyState==null&&A.addEventListener&&(A.addEventListener(D,C=function(){A.removeEventListener(D,C,!1),A.readyState="complete"
},!1),A.readyState="loading"),B()
}(document,"DOMContentLoaded"),Am.was?Ao.win.Raphael=A1:Raphael=A1,eve.on("DOMload",function(){Az=!0
})
}(),window.Raphael.svg&&function(AL){var AK="hasOwnProperty",AJ=String,AI=parseFloat,AH=parseInt,AG=Math,AF=AG.max,AE=AG.abs,AD=AG.pow,AC=/[, ]+/,AB=AL.eve,AA="",Z=" ",Y="http://www.w3.org/1999/xlink",X={block:"M5,0 0,2.5 5,5z",classic:"M5,0 0,2.5 5,5 3.5,3 3.5,2z",diamond:"M2.5,0 5,2.5 2.5,5 0,2.5z",open:"M6,1 1,3.5 6,6",oval:"M2.5,0A2.5,2.5,0,0,1,2.5,5 2.5,2.5,0,0,1,2.5,0z"},W={};
AL.toString=function(){return"Your browser supports SVG.\nYou are running Raphaël "+this.version
};
var U=function(C,B){if(B){typeof C=="string"&&(C=U(C));
for(var A in B){B[AK](A)&&(A.substring(0,6)=="xlink:"?C.setAttributeNS(Y,A.substring(6),AJ(B[A])):C.setAttribute(A,AJ(B[A])))
}}else{C=AL._g.doc.createElementNS("http://www.w3.org/2000/svg",C),C.style&&(C.style.webkitTapHighlightColor="rgba(0,0,0,0)")
}return C
},S={},Q=/^url\(#(.*)\)$/,O=function(A,C){var B=A.getAttribute("fill");
B=B&&B.match(Q),B&&!--S[B[1]]&&(delete S[B[1]],C.defs.removeChild(AL._g.doc.getElementById(B[1])))
},M=function(f,c){var a="linear",F=f.id+c,E=0.5,D=0.5,C=f.node,B=f.paper,A=C.style,z=AL._g.doc.getElementById(F);
if(!z){c=AJ(c).replace(AL._radial_gradient,function(k,j,n){a="radial";
if(j&&n){E=AI(j),D=AI(n);
var m=(D>0.5)*2-1;
AD(E-0.5,2)+AD(D-0.5,2)>0.25&&(D=AG.sqrt(0.25-AD(E-0.5,2))*m+0.5)&&D!=0.5&&(D=D.toFixed(5)-0.00001*m)
}return AA
}),c=c.split(/\s*\-\s*/);
if(a=="linear"){var q=c.shift();
q=-AI(q);
if(isNaN(q)){return null
}var l=[0,0,AG.cos(AL.rad(q)),AG.sin(AL.rad(q))],i=1/(AF(AE(l[2]),AE(l[3]))||1);
l[2]*=i,l[3]*=i,l[2]<0&&(l[0]=-l[2],l[2]=0),l[3]<0&&(l[1]=-l[3],l[3]=0)
}var h=AL._parseDots(c);
if(!h){return null
}f.gradient&&(B.defs.removeChild(f.gradient),delete f.gradient),F=F.replace(/[\(\)\s,\xb0#]/g,"-"),z=U(a+"Gradient",{id:F}),f.gradient=z,U(z,a=="radial"?{fx:E,fy:D}:{x1:l[0],y1:l[1],x2:l[2],y2:l[3],gradientTransform:f.matrix.invert()}),B.defs.appendChild(z);
for(var g=0,d=h.length;
g<d;
g++){z.appendChild(U("stop",{offset:h[g].offset?h[g].offset:g?"100%":"0%","stop-color":h[g].color||"#fff"}))
}}U(C,{fill:"url(#"+F+")",opacity:1,"fill-opacity":1}),A.fill=AA,A.opacity=1,A.fillOpacity=1;
return 1
},K=function(B){var A=B.getBBox(1);
U(B.pattern,{patternTransform:B.matrix.invert()+" translate("+A.x+","+A.y+")"})
},J=function(Ad,Ac,Ab){if(Ad.type=="path"){var Aa=AJ(Ac).toLowerCase().split("-"),AZ=Ad.paper,AY=Ab?"end":"start",AX=Ad.node,AW=Ad.attrs,AV=AW["stroke-width"],AU=Aa.length,AR="classic",AP,AN,q,o,m,c=3,b=3,a=5;
while(AU--){switch(Aa[AU]){case"block":case"classic":case"oval":case"diamond":case"open":case"none":AR=Aa[AU];
break;
case"wide":b=5;
break;
case"narrow":b=2;
break;
case"long":c=5;
break;
case"short":c=2
}}AR=="open"?(c+=2,b+=2,a+=2,q=1,o=Ab?4:1,m={fill:"none",stroke:AW.stroke}):(o=q=c/2,m={fill:AW.stroke,stroke:"none"}),Ad._.arrows?Ab?(Ad._.arrows.endPath&&W[Ad._.arrows.endPath]--,Ad._.arrows.endMarker&&W[Ad._.arrows.endMarker]--):(Ad._.arrows.startPath&&W[Ad._.arrows.startPath]--,Ad._.arrows.startMarker&&W[Ad._.arrows.startMarker]--):Ad._.arrows={};
if(AR!="none"){var AT="raphael-marker-"+AR,AS="raphael-marker-"+AY+AR+c+b;
AL._g.doc.getElementById(AT)?W[AT]++:(AZ.defs.appendChild(U(U("path"),{"stroke-linecap":"round",d:X[AR],id:AT})),W[AT]=1);
var AQ=AL._g.doc.getElementById(AS),AO;
AQ?(W[AS]++,AO=AQ.getElementsByTagName("use")[0]):(AQ=U(U("marker"),{id:AS,markerHeight:b,markerWidth:c,orient:"auto",refX:o,refY:b/2}),AO=U(U("use"),{"xlink:href":"#"+AT,transform:(Ab?" rotate(180 "+c/2+" "+b/2+") ":Z)+"scale("+c/a+","+b/a+")","stroke-width":1/((c/a+b/a)/2)}),AQ.appendChild(AO),AZ.defs.appendChild(AQ),W[AS]=1),U(AO,m);
var AM=q*(AR!="diamond"&&AR!="oval");
Ab?(AP=Ad._.arrows.startdx*AV||0,AN=AL.getTotalLength(AW.path)-AM*AV):(AP=AM*AV,AN=AL.getTotalLength(AW.path)-(Ad._.arrows.enddx*AV||0)),m={},m["marker-"+AY]="url(#"+AS+")";
if(AN||AP){m.d=Raphael.getSubpath(AW.path,AP,AN)
}U(AX,m),Ad._.arrows[AY+"Path"]=AT,Ad._.arrows[AY+"Marker"]=AS,Ad._.arrows[AY+"dx"]=AM,Ad._.arrows[AY+"Type"]=AR,Ad._.arrows[AY+"String"]=Ac
}else{Ab?(AP=Ad._.arrows.startdx*AV||0,AN=AL.getTotalLength(AW.path)-AP):(AP=0,AN=AL.getTotalLength(AW.path)-(Ad._.arrows.enddx*AV||0)),Ad._.arrows[AY+"Path"]&&U(AX,{d:Raphael.getSubpath(AW.path,AP,AN)}),delete Ad._.arrows[AY+"Path"],delete Ad._.arrows[AY+"Marker"],delete Ad._.arrows[AY+"dx"],delete Ad._.arrows[AY+"Type"],delete Ad._.arrows[AY+"String"]
}for(m in W){if(W[AK](m)&&!W[m]){var p=AL._g.doc.getElementById(m);
p&&p.parentNode.removeChild(p)
}}}},I={"":[0],none:[0],"-":[3,1],".":[1,1],"-.":[3,1,1,1],"-..":[3,1,1,1,1,1],". ":[1,3],"- ":[4,3],"--":[8,3],"- .":[4,3,1,3],"--.":[8,3,1,3],"--..":[8,3,1,3,1,3]},H=function(B,A,c){A=I[AJ(A).toLowerCase()];
if(A){var F=B.attrs["stroke-width"]||"1",E={round:F,square:F,butt:0}[B.attrs["stroke-linecap"]||c["stroke-linecap"]]||0,D=[],C=A.length;
while(C--){D[C]=A[C]*F+(C%2?1:-1)*E
}U(B.node,{"stroke-dasharray":D.join(",")})
}},G=function(AP,AO){var AN=AP.node,AM=AP.attrs,y=AN.style.visibility;
AN.style.visibility="hidden";
for(var w in AO){if(AO[AK](w)){if(!AL._availableAttrs[AK](w)){continue
}var v=AO[w];
AM[w]=v;
switch(w){case"blur":AP.blur(v);
break;
case"href":case"title":case"target":var q=AN.parentNode;
if(q.tagName.toLowerCase()!="a"){var j=U("a");
q.insertBefore(j,AN),j.appendChild(AN),q=j
}w=="target"&&v=="blank"?q.setAttributeNS(Y,"show","new"):q.setAttributeNS(Y,w,v);
break;
case"cursor":AN.style.cursor=v;
break;
case"transform":AP.transform(v);
break;
case"arrow-start":J(AP,v);
break;
case"arrow-end":J(AP,v,1);
break;
case"clip-rect":var h=AJ(v).split(AC);
if(h.length==4){AP.clip&&AP.clip.parentNode.parentNode.removeChild(AP.clip.parentNode);
var a=U("clipPath"),B=U("rect");
a.id=AL.createUUID(),U(B,{x:h[0],y:h[1],width:h[2],height:h[3]}),a.appendChild(B),AP.paper.defs.appendChild(a),U(AN,{"clip-path":"url(#"+a.id+")"}),AP.clip=B
}if(!v){var u=AL._g.doc.getElementById(AN.getAttribute("clip-path").replace(/(^url\(#|\)$)/g,AA));
u&&u.parentNode.removeChild(u),U(AN,{"clip-path":AA}),delete AP.clip
}break;
case"path":AP.type=="path"&&(U(AN,{d:v?AM.path=AL._pathToAbsolute(v):"M0,0"}),AP._.dirty=1,AP._.arrows&&("startString" in AP._.arrows&&J(AP,AP._.arrows.startString),"endString" in AP._.arrows&&J(AP,AP._.arrows.endString,1)));
break;
case"width":AN.setAttribute(w,v),AP._.dirty=1;
if(AM.fx){w="x",v=AM.x
}else{break
}case"x":AM.fx&&(v=-AM.x-(AM.width||0));
case"rx":if(w=="rx"&&AP.type=="rect"){break
}case"cx":AN.setAttribute(w,v),AP.pattern&&K(AP),AP._.dirty=1;
break;
case"height":AN.setAttribute(w,v),AP._.dirty=1;
if(AM.fy){w="y",v=AM.y
}else{break
}case"y":AM.fy&&(v=-AM.y-(AM.height||0));
case"ry":if(w=="ry"&&AP.type=="rect"){break
}case"cy":AN.setAttribute(w,v),AP.pattern&&K(AP),AP._.dirty=1;
break;
case"r":AP.type=="rect"?U(AN,{rx:v,ry:v}):AN.setAttribute(w,v),AP._.dirty=1;
break;
case"src":AP.type=="image"&&AN.setAttributeNS(Y,"href",v);
break;
case"stroke-width":if(AP._.sx!=1||AP._.sy!=1){v/=AF(AE(AP._.sx),AE(AP._.sy))||1
}AP.paper._vbSize&&(v*=AP.paper._vbSize),AN.setAttribute(w,v),AM["stroke-dasharray"]&&H(AP,AM["stroke-dasharray"],AO),AP._.arrows&&("startString" in AP._.arrows&&J(AP,AP._.arrows.startString),"endString" in AP._.arrows&&J(AP,AP._.arrows.endString,1));
break;
case"stroke-dasharray":H(AP,v,AO);
break;
case"fill":var n=AJ(v).match(AL._ISURL);
if(n){a=U("pattern");
var l=U("image");
a.id=AL.createUUID(),U(a,{x:0,y:0,patternUnits:"userSpaceOnUse",height:1,width:1}),U(l,{x:0,y:0,"xlink:href":n[1]}),a.appendChild(l),function(A){AL._preload(n[1],function(){var C=this.offsetWidth,D=this.offsetHeight;
U(A,{width:C,height:D}),U(l,{width:C,height:D}),AP.paper.safari()
})
}(a),AP.paper.defs.appendChild(a),AN.style.fill="url(#"+a.id+")",U(AN,{fill:"url(#"+a.id+")"}),AP.pattern=a,AP.pattern&&K(AP);
break
}var g=AL.getRGB(v);
if(!g.error){delete AO.gradient,delete AM.gradient,!AL.is(AM.opacity,"undefined")&&AL.is(AO.opacity,"undefined")&&U(AN,{opacity:AM.opacity}),!AL.is(AM["fill-opacity"],"undefined")&&AL.is(AO["fill-opacity"],"undefined")&&U(AN,{"fill-opacity":AM["fill-opacity"]})
}else{if((AP.type=="circle"||AP.type=="ellipse"||AJ(v).charAt()!="r")&&M(AP,v)){if("opacity" in AM||"fill-opacity" in AM){var e=AL._g.doc.getElementById(AN.getAttribute("fill").replace(/^url\(#|\)$/g,AA));
if(e){var c=e.getElementsByTagName("stop");
U(c[c.length-1],{"stop-opacity":("opacity" in AM?AM.opacity:1)*("fill-opacity" in AM?AM["fill-opacity"]:1)})
}}AM.gradient=v,AM.fill="none";
break
}}g[AK]("opacity")&&U(AN,{"fill-opacity":g.opacity>1?g.opacity/100:g.opacity});
case"stroke":g=AL.getRGB(v),AN.setAttribute(w,g.hex),w=="stroke"&&g[AK]("opacity")&&U(AN,{"stroke-opacity":g.opacity>1?g.opacity/100:g.opacity}),w=="stroke"&&AP._.arrows&&("startString" in AP._.arrows&&J(AP,AP._.arrows.startString),"endString" in AP._.arrows&&J(AP,AP._.arrows.endString,1));
break;
case"gradient":(AP.type=="circle"||AP.type=="ellipse"||AJ(v).charAt()!="r")&&M(AP,v);
break;
case"opacity":AM.gradient&&!AM[AK]("stroke-opacity")&&U(AN,{"stroke-opacity":v>1?v/100:v});
case"fill-opacity":if(AM.gradient){e=AL._g.doc.getElementById(AN.getAttribute("fill").replace(/^url\(#|\)$/g,AA)),e&&(c=e.getElementsByTagName("stop"),U(c[c.length-1],{"stop-opacity":v}));
break
}default:w=="font-size"&&(v=AH(v,10)+"px");
var b=w.replace(/(\-.)/g,function(A){return A.substring(1).toUpperCase()
});
AN.style[b]=v,AP._.dirty=1,AN.setAttribute(w,v)
}}}T(AP,AO),AN.style.visibility=y
},V=1.2,T=function(q,l){if(q.type=="text"&&!!(l[AK]("text")||l[AK]("font")||l[AK]("font-size")||l[AK]("x")||l[AK]("y"))){var e=q.attrs,c=q.node,b=c.firstChild?AH(AL._g.doc.defaultView.getComputedStyle(c.firstChild,AA).getPropertyValue("font-size"),10):10;
if(l[AK]("text")){e.text=l.text;
while(c.firstChild){c.removeChild(c.firstChild)
}var a=AJ(l.text).split("\n"),F=[],E;
for(var D=0,C=a.length;
D<C;
D++){E=U("tspan"),D&&U(E,{dy:b*V,x:e.x}),E.appendChild(AL._g.doc.createTextNode(a[D])),c.appendChild(E),F[D]=E
}}else{F=c.getElementsByTagName("tspan");
for(D=0,C=F.length;
D<C;
D++){D?U(F[D],{dy:b*V,x:e.x}):U(F[0],{dy:0})
}}U(c,{x:e.x,y:e.y}),q._.dirty=1;
var B=q._getBBox(),A=e.y-(B.y+B.height/2);
A&&AL.is(A,"finite")&&U(F[0],{dy:A})
}},R=function(A,D){var C=0,B=0;
this[0]=this.node=A,A.raphael=!0,this.id=AL._oid++,A.raphaelid=this.id,this.matrix=AL.matrix(),this.realPath=null,this.paper=D,this.attrs=this.attrs||{},this._={transform:[],sx:1,sy:1,deg:0,dx:0,dy:0,dirty:1},!D.bottom&&(D.bottom=this),this.prev=D.top,D.top&&(D.top.next=this),D.top=this,this.next=null
},P=AL.el;
R.prototype=P,P.constructor=R,AL._engine.path=function(B,A){var D=U("path");
A.canvas&&A.canvas.appendChild(D);
var C=new R(D,A);
C.type="path",G(C,{fill:"none",stroke:"#000",path:B});
return C
},P.rotate=function(B,A,D){if(this.removed){return this
}B=AJ(B).split(AC),B.length-1&&(A=AI(B[1]),D=AI(B[2])),B=AI(B[0]),D==null&&(A=D);
if(A==null||D==null){var C=this.getBBox(1);
A=C.x+C.width/2,D=C.y+C.height/2
}this.transform(this._.transform.concat([["r",B,A,D]]));
return this
},P.scale=function(B,A,E,D){if(this.removed){return this
}B=AJ(B).split(AC),B.length-1&&(A=AI(B[1]),E=AI(B[2]),D=AI(B[3])),B=AI(B[0]),A==null&&(A=B),D==null&&(E=D);
if(E==null||D==null){var C=this.getBBox(1)
}E=E==null?C.x+C.width/2:E,D=D==null?C.y+C.height/2:D,this.transform(this._.transform.concat([["s",B,A,E,D]]));
return this
},P.translate=function(B,A){if(this.removed){return this
}B=AJ(B).split(AC),B.length-1&&(A=AI(B[1])),B=AI(B[0])||0,A=+A||0,this.transform(this._.transform.concat([["t",B,A]]));
return this
},P.transform=function(C){var B=this._;
if(C==null){return B.transform
}AL._extractTransform(this,C),this.clip&&U(this.clip,{transform:this.matrix.invert()}),this.pattern&&K(this),this.node&&U(this.node,{transform:this.matrix});
if(B.sx!=1||B.sy!=1){var A=this.attrs[AK]("stroke-width")?this.attrs["stroke-width"]:1;
this.attr({"stroke-width":A})
}return this
},P.hide=function(){!this.removed&&this.paper.safari(this.node.style.display="none");
return this
},P.show=function(){!this.removed&&this.paper.safari(this.node.style.display="");
return this
},P.remove=function(){if(!this.removed){this.paper.__set__&&this.paper.__set__.exclude(this),AB.unbind("*.*."+this.id),AL._tear(this,this.paper),this.node.parentNode.removeChild(this.node);
for(var A in this){delete this[A]
}this.removed=!0
}},P._getBBox=function(){if(this.node.style.display=="none"){this.show();
var B=!0
}var A={};
try{A=this.node.getBBox()
}catch(C){}finally{A=A||{}
}B&&this.hide();
return A
},P.attr=function(r,q){if(this.removed){return this
}if(r==null){var k={};
for(var j in this.attrs){this.attrs[AK](j)&&(k[j]=this.attrs[j])
}k.gradient&&k.fill=="none"&&(k.fill=k.gradient)&&delete k.gradient,k.transform=this._.transform;
return k
}if(q==null&&AL.is(r,"string")){if(r=="fill"&&this.attrs.fill=="none"&&this.attrs.gradient){return this.attrs.gradient
}if(r=="transform"){return this._.transform
}var b=r.split(AC),a={};
for(var F=0,E=b.length;
F<E;
F++){r=b[F],r in this.attrs?a[r]=this.attrs[r]:AL.is(this.paper.customAttributes[r],"function")?a[r]=this.paper.customAttributes[r].def:a[r]=AL._availableAttrs[r]
}return E-1?a:a[b[0]]
}if(q==null&&AL.is(r,"array")){a={};
for(F=0,E=r.length;
F<E;
F++){a[r[F]]=this.attr(r[F])
}return a
}if(q!=null){var D={};
D[r]=q
}else{r!=null&&AL.is(r,"object")&&(D=r)
}for(var C in D){AB("attr."+C+"."+this.id,this,D[C])
}for(C in this.paper.customAttributes){if(this.paper.customAttributes[AK](C)&&D[AK](C)&&AL.is(this.paper.customAttributes[C],"function")){var B=this.paper.customAttributes[C].apply(this,[].concat(D[C]));
this.attrs[C]=D[C];
for(var A in B){B[AK](A)&&(D[A]=B[A])
}}}G(this,D);
return this
},P.toFront=function(){if(this.removed){return this
}this.node.parentNode.appendChild(this.node);
var A=this.paper;
A.top!=this&&AL._tofront(this,A);
return this
},P.toBack=function(){if(this.removed){return this
}if(this.node.parentNode.firstChild!=this.node){this.node.parentNode.insertBefore(this.node,this.node.parentNode.firstChild),AL._toback(this,this.paper);
var A=this.paper
}return this
},P.insertAfter=function(A){if(this.removed){return this
}var B=A.node||A[A.length-1].node;
B.nextSibling?B.parentNode.insertBefore(this.node,B.nextSibling):B.parentNode.appendChild(this.node),AL._insertafter(this,A,this.paper);
return this
},P.insertBefore=function(A){if(this.removed){return this
}var B=A.node||A[0].node;
B.parentNode.insertBefore(this.node,B),AL._insertbefore(this,A,this.paper);
return this
},P.blur=function(A){var D=this;
if(+A!==0){var C=U("filter"),B=U("feGaussianBlur");
D.attrs.blur=A,C.id=AL.createUUID(),U(B,{stdDeviation:+A||1.5}),C.appendChild(B),D.paper.defs.appendChild(C),D._blur=C,U(D.node,{filter:"url(#"+C.id+")"})
}else{D._blur&&(D._blur.parentNode.removeChild(D._blur),delete D._blur,delete D.attrs.blur),D.node.removeAttribute("filter")
}},AL._engine.circle=function(B,A,F,E){var D=U("circle");
B.canvas&&B.canvas.appendChild(D);
var C=new R(D,B);
C.attrs={cx:A,cy:F,r:E,fill:"none",stroke:"#000"},C.type="circle",U(D,C.attrs);
return C
},AL._engine.rect=function(B,A,j,i,F,E){var D=U("rect");
B.canvas&&B.canvas.appendChild(D);
var C=new R(D,B);
C.attrs={x:A,y:j,width:i,height:F,r:E||0,rx:E||0,ry:E||0,fill:"none",stroke:"#000"},C.type="rect",U(D,C.attrs);
return C
},AL._engine.ellipse=function(B,A,h,F,E){var D=U("ellipse");
B.canvas&&B.canvas.appendChild(D);
var C=new R(D,B);
C.attrs={cx:A,cy:h,rx:F,ry:E,fill:"none",stroke:"#000"},C.type="ellipse",U(D,C.attrs);
return C
},AL._engine.image=function(B,A,j,i,F,E){var D=U("image");
U(D,{x:j,y:i,width:F,height:E,preserveAspectRatio:"none"}),D.setAttributeNS(Y,"href",A),B.canvas&&B.canvas.appendChild(D);
var C=new R(D,B);
C.attrs={x:j,y:i,width:F,height:E,src:A},C.type="image";
return C
},AL._engine.text=function(A,F,E,D){var C=U("text");
A.canvas&&A.canvas.appendChild(C);
var B=new R(C,A);
B.attrs={x:F,y:E,"text-anchor":"middle",text:D,font:AL._availableAttrs.font,stroke:"none",fill:"#000"},B.type="text",G(B,B.attrs);
return B
},AL._engine.setSize=function(B,A){this.width=B||this.width,this.height=A||this.height,this.canvas.setAttribute("width",this.width),this.canvas.setAttribute("height",this.height),this._viewBox&&this.setViewBox.apply(this,this._viewBox);
return this
},AL._engine.create=function(){var l=AL._getContainer.apply(0,arguments),k=l&&l.container,a=l.x,F=l.y,E=l.width,D=l.height;
if(!k){throw new Error("SVG container not found.")
}var C=U("svg"),B="overflow:hidden;",A;
a=a||0,F=F||0,E=E||512,D=D||342,U(C,{height:D,version:1.1,width:E,xmlns:"http://www.w3.org/2000/svg"}),k==1?(C.style.cssText=B+"position:absolute;left:"+a+"px;top:"+F+"px",AL._g.doc.body.appendChild(C),A=1):(C.style.cssText=B+"position:relative",k.firstChild?k.insertBefore(C,k.firstChild):k.appendChild(C)),k=new AL._Paper,k.width=E,k.height=D,k.canvas=C,k.clear(),k._left=k._top=0,A&&(k.renderfix=function(){}),k.renderfix();
return k
},AL._engine.setViewBox=function(n,m,k,g,F){AB("setViewBox",this,this._viewBox,[n,m,k,g,F]);
var E=AF(k/this.width,g/this.height),D=this.top,C=F?"meet":"xMinYMin",B,A;
n==null?(this._vbSize&&(E=1),delete this._vbSize,B="0 0 "+this.width+Z+this.height):(this._vbSize=E,B=n+Z+m+Z+k+Z+g),U(this.canvas,{viewBox:B,preserveAspectRatio:C});
while(E&&D){A="stroke-width" in D.attrs?D.attrs["stroke-width"]:1,D.attr({"stroke-width":A}),D._.dirty=1,D._.dirtyT=1,D=D.prev
}this._viewBox=[n,m,k,g,!!F];
return this
},AL.prototype.renderfix=function(){var B=this.canvas,A=B.style,E=B.getScreenCTM()||B.createSVGMatrix(),D=-E.e%1,C=-E.f%1;
if(D||C){D&&(this._left=(this._left+D)%1,A.left=this._left+"px"),C&&(this._top=(this._top+C)%1,A.top=this._top+"px")
}},AL.prototype.clear=function(){AL.eve("clear",this);
var A=this.canvas;
while(A.firstChild){A.removeChild(A.firstChild)
}this.bottom=this.top=null,(this.desc=U("desc")).appendChild(AL._g.doc.createTextNode("Created with Raphaël "+AL.version)),A.appendChild(this.desc),A.appendChild(this.defs=U("defs"))
},AL.prototype.remove=function(){AB("remove",this),this.canvas.parentNode&&this.canvas.parentNode.removeChild(this.canvas);
for(var A in this){this[A]=removed(A)
}};
var N=AL.st;
for(var L in P){P[AK](L)&&!N[AK](L)&&(N[L]=function(A){return function(){var B=arguments;
return this.forEach(function(C){C[A].apply(C,B)
})
}
}(L))
}}(window.Raphael),window.Raphael.vml&&function(AJ){var AI="hasOwnProperty",AH=String,AG=parseFloat,AF=Math,AE=AF.round,AD=AF.max,AC=AF.min,AB=AF.abs,AA="fill",Z=/[, ]+/,Y=AJ.eve,X=" progid:DXImageTransform.Microsoft",W=" ",V="",U={M:"m",L:"l",C:"c",Z:"x",m:"t",l:"r",c:"v",z:"x"},S=/([clmz]),?([^clmz]*)/gi,Q=/ progid:\S+Blur\([^\)]+\)/g,O=/-?[^,\s-]+/g,M="position:absolute;left:0;top:0;width:1px;height:1px",K=21600,J={path:1,rect:1,image:1},I={circle:1,ellipse:1},H=function(p){var o=/[ahqstv]/ig,n=AJ._pathToAbsolute;
AH(p).match(o)&&(n=AJ._path2curve),o=/[clmz]/g;
if(n==AJ._pathToAbsolute&&!AH(p).match(o)){var f=AH(p).replace(S,function(i,h,m){var l=[],k=h.toLowerCase()=="m",j=U[h];
m.replace(O,function(b){k&&l.length==2&&(j+=l+U[h=="m"?"l":"L"],l=[]),l.push(AE(b*K))
});
return j+l
});
return f
}var c=n(p),a,E;
f=[];
for(var D=0,C=c.length;
D<C;
D++){a=c[D],E=c[D][0].toLowerCase(),E=="z"&&(E="x");
for(var B=1,A=a.length;
B<A;
B++){E+=AE(a[B]*K)+(B!=A-1?",":V)
}f.push(E)
}return f.join(W)
},G=function(A,D,C){var B=AJ.matrix();
B.rotate(-A,0.5,0.5);
return{dx:B.x(D,C),dy:B.y(D,C)}
},F=function(AO,AN,AM,AL,AK,z){var y=AO._,x=AO.matrix,w=y.fillpos,u=AO.node,n=u.style,j=1,i="",E,D=K/AN,C=K/AM;
n.visibility="hidden";
if(!!AN&&!!AM){u.coordsize=AB(D)+W+AB(C),n.rotation=z*(AN*AM<0?-1:1);
if(z){var B=G(z,AL,AK);
AL=B.dx,AK=B.dy
}AN<0&&(i+="x"),AM<0&&(i+=" y")&&(j=-1),n.flip=i,u.coordorigin=AL*-D+W+AK*-C;
if(w||y.fillsize){var A=u.getElementsByTagName(AA);
A=A&&A[0],u.removeChild(A),w&&(B=G(z,x.x(w[0],w[1]),x.y(w[0],w[1])),A.position=B.dx*j+W+B.dy*j),y.fillsize&&(A.size=y.fillsize[0]*AB(AN)+W+y.fillsize[1]*AB(AM)),u.appendChild(A)
}n.visibility="visible"
}};
AJ.toString=function(){return"Your browser doesn’t support SVG. Falling down to VML.\nYou are running Raphaël "+this.version
},addArrow=function(o,n,m){var l=AH(n).toLowerCase().split("-"),c=m?"end":"start",E=l.length,D="classic",C="medium",B="medium";
while(E--){switch(l[E]){case"block":case"classic":case"oval":case"diamond":case"open":case"none":D=l[E];
break;
case"wide":case"narrow":B=l[E];
break;
case"long":case"short":C=l[E]
}}var A=o.node.getElementsByTagName("stroke")[0];
A[c+"arrow"]=D,A[c+"arrowlength"]=C,A[c+"arrowwidth"]=B
},setFillAndStroke=function(Aa,AZ){Aa.attrs=Aa.attrs||{};
var AY=Aa.node,AX=Aa.attrs,AW=AY.style,AU,AT=J[Aa.type]&&(AZ.x!=AX.x||AZ.y!=AX.y||AZ.width!=AX.width||AZ.height!=AX.height||AZ.cx!=AX.cx||AZ.cy!=AX.cy||AZ.rx!=AX.rx||AZ.ry!=AX.ry||AZ.r!=AX.r),AQ=I[Aa.type]&&(AX.cx!=AZ.cx||AX.cy!=AZ.cy||AX.r!=AZ.r||AX.rx!=AZ.rx||AX.ry!=AZ.ry),AP=Aa;
for(var z in AZ){AZ[AI](z)&&(AX[z]=AZ[z])
}AT&&(AX.path=AJ._getPath[Aa.type](Aa),Aa._.dirty=1),AZ.href&&(AY.href=AZ.href),AZ.title&&(AY.title=AZ.title),AZ.target&&(AY.target=AZ.target),AZ.cursor&&(AW.cursor=AZ.cursor),"blur" in AZ&&Aa.blur(AZ.blur);
if(AZ.path&&Aa.type=="path"||AT){AY.path=H(~AH(AX.path).toLowerCase().indexOf("r")?AJ._pathToAbsolute(AX.path):AX.path),Aa.type=="image"&&(Aa._.fillpos=[AX.x,AX.y],Aa._.fillsize=[AX.width,AX.height],F(Aa,1,1,0,0,0))
}"transform" in AZ&&Aa.transform(AZ.transform);
if(AQ){var AV=+AX.cx,AS=+AX.cy,AR=+AX.rx||+AX.r||0,AO=+AX.ry||+AX.r||0;
AY.path=AJ.format("ar{0},{1},{2},{3},{4},{1},{4},{1}x",AE((AV-AR)*K),AE((AS-AO)*K),AE((AV+AR)*K),AE((AS+AO)*K),AE(AV*K))
}if("clip-rect" in AZ){var AN=AH(AZ["clip-rect"]).split(Z);
if(AN.length==4){AN[2]=+AN[2]+ +AN[0],AN[3]=+AN[3]+ +AN[1];
var AM=AY.clipRect||AJ._g.doc.createElement("div"),AL=AM.style;
AL.clip=AJ.format("rect({1}px {2}px {3}px {0}px)",AN),AY.clipRect||(AL.position="absolute",AL.top=0,AL.left=0,AL.width=Aa.paper.width+"px",AL.height=Aa.paper.height+"px",AY.parentNode.insertBefore(AM,AY),AM.appendChild(AY),AY.clipRect=AM)
}AZ["clip-rect"]||AY.clipRect&&(AY.clipRect.style.clip=V)
}if(Aa.textpath){var AK=Aa.textpath.style;
AZ.font&&(AK.font=AZ.font),AZ["font-family"]&&(AK.fontFamily='"'+AZ["font-family"].split(",")[0].replace(/^['"]+|['"]+$/g,V)+'"'),AZ["font-size"]&&(AK.fontSize=AZ["font-size"]),AZ["font-weight"]&&(AK.fontWeight=AZ["font-weight"]),AZ["font-style"]&&(AK.fontStyle=AZ["font-style"])
}"arrow-start" in AZ&&addArrow(AP,AZ["arrow-start"]),"arrow-end" in AZ&&addArrow(AP,AZ["arrow-end"],1);
if(AZ.opacity!=null||AZ["stroke-width"]!=null||AZ.fill!=null||AZ.src!=null||AZ.stroke!=null||AZ["stroke-width"]!=null||AZ["stroke-opacity"]!=null||AZ["fill-opacity"]!=null||AZ["stroke-dasharray"]!=null||AZ["stroke-miterlimit"]!=null||AZ["stroke-linejoin"]!=null||AZ["stroke-linecap"]!=null){var x=AY.getElementsByTagName(AA),w=!1;
x=x&&x[0],!x&&(w=x=R(AA)),Aa.type=="image"&&AZ.src&&(x.src=AZ.src),AZ.fill&&(x.on=!0);
if(x.on==null||AZ.fill=="none"||AZ.fill===null){x.on=!1
}if(x.on&&AZ.fill){var v=AH(AZ.fill).match(AJ._ISURL);
if(v){x.parentNode==AY&&AY.removeChild(x),x.rotate=!0,x.src=v[1],x.type="tile";
var u=Aa.getBBox(1);
x.position=u.x+W+u.y,Aa._.fillpos=[u.x,u.y],AJ._preload(v[1],function(){Aa._.fillsize=[this.offsetWidth,this.offsetHeight]
})
}else{x.color=AJ.getRGB(AZ.fill).hex,x.src=V,x.type="solid",AJ.getRGB(AZ.fill).error&&(AP.type in {circle:1,ellipse:1}||AH(AZ.fill).charAt()!="r")&&addGradientFill(AP,AZ.fill,x)&&(AX.fill="none",AX.gradient=AZ.fill,x.rotate=!1)
}}if("fill-opacity" in AZ||"opacity" in AZ){var o=((+AX["fill-opacity"]+1||2)-1)*((+AX.opacity+1||2)-1)*((+AJ.getRGB(AZ.fill).o+1||2)-1);
o=AC(AD(o,0),1),x.opacity=o,x.src&&(x.color="none")
}AY.appendChild(x);
var n=AY.getElementsByTagName("stroke")&&AY.getElementsByTagName("stroke")[0],k=!1;
!n&&(k=n=R("stroke"));
if(AZ.stroke&&AZ.stroke!="none"||AZ["stroke-width"]||AZ["stroke-opacity"]!=null||AZ["stroke-dasharray"]||AZ["stroke-miterlimit"]||AZ["stroke-linejoin"]||AZ["stroke-linecap"]){n.on=!0
}(AZ.stroke=="none"||AZ.stroke===null||n.on==null||AZ.stroke==0||AZ["stroke-width"]==0)&&(n.on=!1);
var j=AJ.getRGB(AZ.stroke);
n.on&&AZ.stroke&&(n.color=j.hex),o=((+AX["stroke-opacity"]+1||2)-1)*((+AX.opacity+1||2)-1)*((+j.o+1||2)-1);
var g=(AG(AZ["stroke-width"])||1)*0.75;
o=AC(AD(o,0),1),AZ["stroke-width"]==null&&(g=AX["stroke-width"]),AZ["stroke-width"]&&(n.weight=g),g&&g<1&&(o*=g)&&(n.weight=1),n.opacity=o,AZ["stroke-linejoin"]&&(n.joinstyle=AZ["stroke-linejoin"]||"miter"),n.miterlimit=AZ["stroke-miterlimit"]||8,AZ["stroke-linecap"]&&(n.endcap=AZ["stroke-linecap"]=="butt"?"flat":AZ["stroke-linecap"]=="square"?"square":"round");
if(AZ["stroke-dasharray"]){var f={"-":"shortdash",".":"shortdot","-.":"shortdashdot","-..":"shortdashdotdot",". ":"dot","- ":"dash","--":"longdash","- .":"dashdot","--.":"longdashdot","--..":"longdashdotdot"};
n.dashstyle=f[AI](AZ["stroke-dasharray"])?f[AZ["stroke-dasharray"]]:V
}k&&AY.appendChild(n)
}if(AP.type=="text"){AP.paper.canvas.style.display=V;
var d=AP.paper.span,c=100,b=AX.font&&AX.font.match(/\d+(?:\.\d*)?(?=px)/);
AW=d.style,AX.font&&(AW.font=AX.font),AX["font-family"]&&(AW.fontFamily=AX["font-family"]),AX["font-weight"]&&(AW.fontWeight=AX["font-weight"]),AX["font-style"]&&(AW.fontStyle=AX["font-style"]),b=AG(b?b[0]:AX["font-size"]),AW.fontSize=b*c+"px",AP.textpath.string&&(d.innerHTML=AH(AP.textpath.string).replace(/</g,"&#60;").replace(/&/g,"&#38;").replace(/\n/g,"<br>"));
var a=d.getBoundingClientRect();
AP.W=AX.w=(a.right-a.left)/c,AP.H=AX.h=(a.bottom-a.top)/c,AP.X=AX.x,AP.Y=AX.y+AP.H/2,("x" in AZ||"y" in AZ)&&(AP.path.v=AJ.format("m{0},{1}l{2},{1}",AE(AX.x*K),AE(AX.y*K),AE(AX.x*K)+1));
var B=["x","y","text","font","font-family","font-weight","font-style","font-size"];
for(var h=0,Ab=B.length;
h<Ab;
h++){if(B[h] in AZ){AP._.dirty=1;
break
}}switch(AX["text-anchor"]){case"start":AP.textpath.style["v-text-align"]="left",AP.bbx=AP.W/2;
break;
case"end":AP.textpath.style["v-text-align"]="right",AP.bbx=-AP.W/2;
break;
default:AP.textpath.style["v-text-align"]="center",AP.bbx=0
}AP.textpath.style["v-text-kern"]=!0
}},addGradientFill=function(u,o,n){u.attrs=u.attrs||{};
var e=u.attrs,d=Math.pow,c,a,E="linear",D=".5 .5";
u.attrs.gradient=o,o=AH(o).replace(AJ._radial_gradient,function(g,f,h){E="radial",f&&h&&(f=AG(f),h=AG(h),d(f-0.5,2)+d(h-0.5,2)>0.25&&(h=AF.sqrt(0.25-d(f-0.5,2))*((h>0.5)*2-1)+0.5),D=f+W+h);
return V
}),o=o.split(/\s*\-\s*/);
if(E=="linear"){var C=o.shift();
C=-AG(C);
if(isNaN(C)){return null
}}var B=AJ._parseDots(o);
if(!B){return null
}u=u.shape||u.node;
if(B.length){u.removeChild(n),n.on=!0,n.method="none",n.color=B[0].color,n.color2=B[B.length-1].color;
var A=[];
for(var w=0,v=B.length;
w<v;
w++){B[w].offset&&A.push(B[w].offset+W+B[w].color)
}n.colors=A.length?A.join():"0% "+n.color,E=="radial"?(n.type="gradientTitle",n.focus="100%",n.focussize="0 0",n.focusposition=D,n.angle=0):(n.type="gradient",n.angle=(270-C)%360),u.appendChild(n)
}return 1
},Element=function(A,B){this[0]=this.node=A,A.raphael=!0,this.id=AJ._oid++,A.raphaelid=this.id,this.X=0,this.Y=0,this.attrs={},this.paper=B,this.matrix=AJ.matrix(),this._={transform:[],sx:1,sy:1,dx:0,dy:0,deg:0,dirty:1,dirtyT:1},!B.bottom&&(B.bottom=this),this.prev=B.top,B.top&&(B.top.next=this),B.top=this,this.next=null
};
var T=AJ.el;
Element.prototype=T,T.constructor=Element,T.transform=function(w){if(w==null){return this._.transform
}var v=this.paper._viewBoxShift,u=v?"s"+[v.scale,v.scale]+"-1-1t"+[v.dx,v.dy]:V,t;
v&&(t=w=AH(w).replace(/\.{3}|\u2026/g,this._.transform||V)),AJ._extractTransform(this,u+w);
var s=this.matrix.clone(),o=this.skew,n=this.node,c,a=~AH(this.attrs.fill).indexOf("-"),E=!AH(this.attrs.fill).indexOf("url(");
s.translate(-0.5,-0.5);
if(E||a||this.type=="image"){o.matrix="1 0 0 1",o.offset="0 0",c=s.split();
if(a&&c.noRotation||!c.isSimple){n.style.filter=s.toFilter();
var D=this.getBBox(),C=this.getBBox(1),B=D.x-C.x,A=D.y-C.y;
n.coordorigin=B*-K+W+A*-K,F(this,1,1,B,A,0)
}else{n.style.filter=V,F(this,c.scalex,c.scaley,c.dx,c.dy,c.rotate)
}}else{n.style.filter=V,o.matrix=AH(s),o.offset=s.offset()
}t&&(this._.transform=t);
return this
},T.rotate=function(B,A,D){if(this.removed){return this
}if(B!=null){B=AH(B).split(Z),B.length-1&&(A=AG(B[1]),D=AG(B[2])),B=AG(B[0]),D==null&&(A=D);
if(A==null||D==null){var C=this.getBBox(1);
A=C.x+C.width/2,D=C.y+C.height/2
}this._.dirtyT=1,this.transform(this._.transform.concat([["r",B,A,D]]));
return this
}},T.translate=function(B,A){if(this.removed){return this
}B=AH(B).split(Z),B.length-1&&(A=AG(B[1])),B=AG(B[0])||0,A=+A||0,this._.bbox&&(this._.bbox.x+=B,this._.bbox.y+=A),this.transform(this._.transform.concat([["t",B,A]]));
return this
},T.scale=function(B,A,E,D){if(this.removed){return this
}B=AH(B).split(Z),B.length-1&&(A=AG(B[1]),E=AG(B[2]),D=AG(B[3]),isNaN(E)&&(E=null),isNaN(D)&&(D=null)),B=AG(B[0]),A==null&&(A=B),D==null&&(E=D);
if(E==null||D==null){var C=this.getBBox(1)
}E=E==null?C.x+C.width/2:E,D=D==null?C.y+C.height/2:D,this.transform(this._.transform.concat([["s",B,A,E,D]])),this._.dirtyT=1;
return this
},T.hide=function(){!this.removed&&(this.node.style.display="none");
return this
},T.show=function(){!this.removed&&(this.node.style.display=V);
return this
},T._getBBox=function(){if(this.removed){return{}
}return this.type=="text"?{x:this.X+(this.bbx||0)-this.W/2,y:this.Y-this.H,width:this.W,height:this.H}:pathDimensions(this.attrs.path)
},T.remove=function(){if(!this.removed){this.paper.__set__&&this.paper.__set__.exclude(this),AJ.eve.unbind("*.*."+this.id),AJ._tear(this,this.paper),this.node.parentNode.removeChild(this.node),this.shape&&this.shape.parentNode.removeChild(this.shape);
for(var A in this){delete this[A]
}this.removed=!0
}},T.attr=function(s,r){if(this.removed){return this
}if(s==null){var l={};
for(var k in this.attrs){this.attrs[AI](k)&&(l[k]=this.attrs[k])
}l.gradient&&l.fill=="none"&&(l.fill=l.gradient)&&delete l.gradient,l.transform=this._.transform;
return l
}if(r==null&&AJ.is(s,"string")){if(s==AA&&this.attrs.fill=="none"&&this.attrs.gradient){return this.attrs.gradient
}var j=s.split(Z),b={};
for(var a=0,E=j.length;
a<E;
a++){s=j[a],s in this.attrs?b[s]=this.attrs[s]:AJ.is(this.paper.customAttributes[s],"function")?b[s]=this.paper.customAttributes[s].def:b[s]=AJ._availableAttrs[s]
}return E-1?b:b[j[0]]
}if(this.attrs&&r==null&&AJ.is(s,"array")){b={};
for(a=0,E=s.length;
a<E;
a++){b[s[a]]=this.attr(s[a])
}return b
}var D;
r!=null&&(D={},D[s]=r),r==null&&AJ.is(s,"object")&&(D=s);
for(var C in D){Y("attr."+C+"."+this.id,this,D[C])
}if(D){for(C in this.paper.customAttributes){if(this.paper.customAttributes[AI](C)&&D[AI](C)&&AJ.is(this.paper.customAttributes[C],"function")){var B=this.paper.customAttributes[C].apply(this,[].concat(D[C]));
this.attrs[C]=D[C];
for(var A in B){B[AI](A)&&(D[A]=B[A])
}}}D.text&&this.type=="text"&&(this.textpath.string=D.text),setFillAndStroke(this,D)
}return this
},T.toFront=function(){!this.removed&&this.node.parentNode.appendChild(this.node),this.paper&&this.paper.top!=this&&AJ._tofront(this,this.paper);
return this
},T.toBack=function(){if(this.removed){return this
}this.node.parentNode.firstChild!=this.node&&(this.node.parentNode.insertBefore(this.node,this.node.parentNode.firstChild),AJ._toback(this,this.paper));
return this
},T.insertAfter=function(A){if(this.removed){return this
}A.constructor==AJ.st.constructor&&(A=A[A.length-1]),A.node.nextSibling?A.node.parentNode.insertBefore(this.node,A.node.nextSibling):A.node.parentNode.appendChild(this.node),AJ._insertafter(this,A,this.paper);
return this
},T.insertBefore=function(A){if(this.removed){return this
}A.constructor==AJ.st.constructor&&(A=A[0]),A.node.parentNode.insertBefore(this.node,A.node),AJ._insertbefore(this,A,this.paper);
return this
},T.blur=function(A){var C=this.node.runtimeStyle,B=C.filter;
B=B.replace(Q,V),+A!==0?(this.attrs.blur=A,C.filter=B+W+X+".Blur(pixelradius="+(+A||1.5)+")",C.margin=AJ.format("-{0}px 0 0 -{0}px",AE(+A||1.5))):(C.filter=B,C.margin=0,delete this.attrs.blur)
},AJ._engine.path=function(B,A){var g=R("shape");
g.style.cssText=M,g.coordsize=K+W+K,g.coordorigin=A.coordorigin;
var E=new Element(g,A),D={fill:"none",stroke:"#000"};
B&&(D.path=B),E.type="path",E.path=[],E.Path=V,setFillAndStroke(E,D),A.canvas.appendChild(g);
var C=R("skew");
C.on=!0,g.appendChild(C),E.skew=C,E.transform(V);
return E
},AJ._engine.rect=function(m,l,k,a,E,D){var C=AJ._rectPath(l,k,a,E,D),B=m.path(C),A=B.attrs;
B.X=A.x=l,B.Y=A.y=k,B.W=A.width=a,B.H=A.height=E,A.r=D,A.path=C,B.type="rect";
return B
},AJ._engine.ellipse=function(B,A,i,h,E){var D=B.path(),C=D.attrs;
D.X=A-h,D.Y=i-E,D.W=h*2,D.H=E*2,D.type="ellipse",setFillAndStroke(D,{cx:A,cy:i,rx:h,ry:E});
return D
},AJ._engine.circle=function(B,A,g,E){var D=B.path(),C=D.attrs;
D.X=A-E,D.Y=g-E,D.W=D.H=E*2,D.type="circle",setFillAndStroke(D,{cx:A,cy:g,r:E});
return D
},AJ._engine.image=function(q,p,o,n,j,a){var E=AJ._rectPath(o,n,j,a),D=q.path(E).attr({stroke:"none"}),C=D.attrs,B=D.node,A=B.getElementsByTagName(AA)[0];
C.src=p,D.X=C.x=o,D.Y=C.y=n,D.W=C.width=j,D.H=C.height=a,C.path=E,D.type="image",A.parentNode==B&&B.removeChild(A),A.rotate=!0,A.src=p,A.type="tile",D._.fillpos=[o,n],D._.fillsize=[j,a],B.appendChild(A),F(D,1,1,0,0,0);
return D
},AJ._engine.text=function(o,n,f,c){var a=R("shape"),E=R("path"),D=R("textpath");
n=n||0,f=f||0,c=c||"",E.v=AJ.format("m{0},{1}l{2},{1}",AE(n*K),AE(f*K),AE(n*K)+1),E.textpathok=!0,D.string=AH(c),D.on=!0,a.style.cssText=M,a.coordsize=K+W+K,a.coordorigin="0 0";
var C=new Element(a,o),B={fill:"#000",stroke:"none",font:AJ._availableAttrs.font,text:c};
C.shape=a,C.path=E,C.textpath=D,C.type="text",C.attrs.text=AH(c),C.attrs.x=n,C.attrs.y=f,C.attrs.w=1,C.attrs.h=1,setFillAndStroke(C,B),a.appendChild(D),a.appendChild(E),o.canvas.appendChild(a);
var A=R("skew");
A.on=!0,a.appendChild(A),C.skew=A,C.transform(V);
return C
},AJ._engine.setSize=function(B,A){var C=this.canvas.style;
this.width=B,this.height=A,B==+B&&(B+="px"),A==+A&&(A+="px"),C.width=B,C.height=A,C.clip="rect(0 "+B+" "+A+" 0)",this._viewBox&&setViewBox.apply(this,this._viewBox);
return this
},AJ._engine.setViewBox=function(o,n,m,g,a){AJ.eve("setViewBox",this,this._viewBox,[o,n,m,g,a]);
var E=this.width,D=this.height,C=1/AD(m/E,g/D),B,A;
a&&(B=D/g,A=E/m,m*B<E&&(o-=(E-m*B)/2/B),g*A<D&&(n-=(D-g*A)/2/A)),this._viewBox=[o,n,m,g,!!a],this._viewBoxShift={dx:-o,dy:-n,scale:C},this.forEach(function(b){b.transform("...")
});
return this
};
var R,P=function(B){var A=B.document;
A.createStyleSheet().addRule(".rvml","behavior:url(#default#VML)");
try{!A.namespaces.rvml&&A.namespaces.add("rvml","urn:schemas-microsoft-com:vml"),R=function(D){return A.createElement("<rvml:"+D+' class="rvml">')
}
}catch(C){R=function(D){return A.createElement("<"+D+' xmlns="urn:schemas-microsoft.com:vml" class="rvml">')
}
}};
P(AJ._g.win),AJ._engine.create=function(){var o=AJ._getContainer.apply(0,arguments),n=o.container,m=o.height,l,a=o.width,E=o.x,D=o.y;
if(!n){throw new Error("VML container not found.")
}var C=new AJ._Paper,B=C.canvas=AJ._g.doc.createElement("div"),A=B.style;
E=E||0,D=D||0,a=a||512,m=m||342,C.width=a,C.height=m,a==+a&&(a+="px"),m==+m&&(m+="px"),C.coordsize=K*1000+W+K*1000,C.coordorigin="0 0",C.span=AJ._g.doc.createElement("span"),C.span.style.cssText="position:absolute;left:-9999em;top:-9999em;padding:0;margin:0;line-height:1;",B.appendChild(C.span),A.cssText=AJ.format("top:0;left:0;width:{0};height:{1};display:inline-block;position:relative;clip:rect(0 {0} {1} 0);overflow:hidden",a,m),n==1?(AJ._g.doc.body.appendChild(B),A.left=E+"px",A.top=D+"px",A.position="absolute"):n.firstChild?n.insertBefore(B,n.firstChild):n.appendChild(B),C.renderfix=function(){};
return C
},AJ.prototype.clear=function(){AJ.eve("clear",this),this.canvas.innerHTML=V,this.span=AJ._g.doc.createElement("span"),this.span.style.cssText="position:absolute;left:-9999em;top:-9999em;padding:0;margin:0;line-height:1;display:inline;",this.canvas.appendChild(this.span),this.bottom=this.top=null
},AJ.prototype.remove=function(){AJ.eve("remove",this),this.canvas.parentNode.removeChild(this.canvas);
for(var A in this){this[A]=removed(A)
}return !0
};
var N=AJ.st;
for(var L in T){T[AI](L)&&!N[AI](L)&&(N[L]=function(A){return function(){var B=arguments;
return this.forEach(function(C){C[A].apply(C,B)
})
}
}(L))
}}(window.Raphael);