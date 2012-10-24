(function(C){var B=function(){var D=window.location.hash.replace("#","");
var F=C("ul.navigationTabs a").bind("click",A).filter("a[rel="+D+"]");
if(F.size()==0){F=C("ul.navigationTabs a:first")
}A.apply(F.get(0));
C("#colorpickerHolder").ColorPicker({flat:true});
C("#colorpickerHolder2").ColorPicker({flat:true,color:"#00ff00",onSubmit:function(G,I,H){C("#colorSelector2 div").css("backgroundColor","#"+I)
}});
C("#colorpickerHolder2>div").css("position","absolute");
var E=false;
C("#colorSelector2").bind("click",function(){C("#colorpickerHolder2").stop().animate({height:E?0:173},500);
E=!E
});
C("#colorpickerField1, #colorpickerField2, #colorpickerField3").ColorPicker({onSubmit:function(G,J,H,I){C(I).val(J);
C(I).ColorPickerHide()
},onBeforeShow:function(){C(this).ColorPickerSetColor(this.value)
}}).bind("keyup",function(){C(this).ColorPickerSetColor(this.value)
});
C("#colorSelector").ColorPicker({color:"#0000ff",onShow:function(G){C(G).fadeIn(500);
return false
},onHide:function(G){C(G).fadeOut(500);
return false
},onChange:function(G,I,H){C("#colorSelector div").css("backgroundColor","#"+I)
}})
};
var A=function(E){var D=C("ul.navigationTabs a").removeClass("active").index(this);
C(this).addClass("active").blur();
C("div.tab").hide().eq(D).show()
};
EYE.register(B,"init")
})(jQuery);