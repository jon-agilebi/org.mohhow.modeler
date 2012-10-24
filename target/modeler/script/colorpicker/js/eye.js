(function(A){var B=window.EYE=function(){var C={init:[]};
return{init:function(){A.each(C.init,function(E,D){D.call()
})
},extend:function(E){for(var D in E){if(E[D]!=undefined){this[D]=E[D]
}}},register:function(E,D){if(!C[D]){C[D]=[]
}C[D].push(E)
}}
}();
A(B.init)
})(jQuery);