if ((typeof(dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1)) == "undefined") {dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1 = {};
};
(function(){basis$0General$1.print$163("<html><body><h1>Check for additive inverses</h1><table border='1'><tr><th align='left'>List of numbers:(e.g. 1,~1,2,~2)</th><td><input type='text' id='jL'></td></tr><tr><th align='left'>Each have its additive inverse?:</th><td><div id='jR'>?</div></td></tr></table></body></html>");
dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.get$54 = function(id$57){var v$62 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$88,id$57);
switch (v$62[0]) { case 0: {return v$62[1];
 break; }default: {throw [basis$0Initial$1.en$Fail$54,"Missing id in document: " + id$57];
} };
};
dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.input$65;
var v$170 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$88,"jL");
switch (v$170[0]) { case 0: {dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.input$65 = v$170[1];
 break; }default: {throw [basis$0Initial$1.en$Fail$54,"Missing id in document: jL"];
} };
(function(fp,s,v){fp[s] = v;})((function(fp,s){return fp[s];})(dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.input$65,"style"),"backgroundColor","yellow");
dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.allHaveAddInv$67 = function(original$70){var fix$228 = {};
fix$228.$hasAddInv = function(v$226,v$83){lab$hasAddInv: while (true) {if (v$83 == null) {return false;
} else {var v$90 = v$83;
var v$91 = v$90[0];
var v$92 = v$90[1];
if ((SmlPrims.chk_ovf_i32(v$226 + v$91)) == 0) {return true;
} else {var t$229 = v$226;
var t$230 = v$92;
var v$226 = t$229;
var v$83 = t$230;
continue lab$hasAddInv;
};
};
};
};
var hasAddInv$71 = fix$228.$hasAddInv;
var fix$231 = {};
fix$231.$allHaveAddInvHelper = function(v$96){lab$allHaveAddInvHelper: while (true) {if (v$96 == null) {return true;
} else {var v$107 = v$96;
var v$108 = v$107[0];
var v$109 = v$107[1];
if (hasAddInv$71(v$108,original$70)) {var t$232 = v$109;
var v$96 = t$232;
continue lab$allHaveAddInvHelper;
} else {return false;
};
};
};
};
var allHaveAddInvHelper$93 = fix$231.$allHaveAddInvHelper;
return allHaveAddInvHelper$93(original$70);
};
dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.numbers$110 = function(s$113){return basis$0List$1.map$697(function(x$182){return basis$0General$1.valOf$130(basis$0Int32$1.fromString$462(x$182));
},basis$0String$1.tokens$224(function(v$183){switch (v$183) { case 44: {return true;
 break; }default: {return (v$183 == 32)?true:((9 <= v$183)?(v$183 <= 13):false);
} };
},s$113));
};
dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.comp$123 = function(v$125){var v$126 = (function(e){return e.value;})(dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.input$65);
var res$127 = (dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.allHaveAddInv$67(basis$0List$1.map$697(function(x$191){return basis$0General$1.valOf$130(basis$0Int32$1.fromString$462(x$191));
},basis$0String$1.tokens$224(function(v$192){switch (v$192) { case 44: {return true;
 break; }default: {return (v$192 == 32)?true:((9 <= v$192)?(v$192 <= 13):false);
} };
},v$126))))?"true":"false";
var r$128;
var v$194 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$88,"jR");
switch (v$194[0]) { case 0: {r$128 = v$194[1];
 break; }default: {throw [basis$0Initial$1.en$Fail$54,"Missing id in document: jR"];
} };
(function(fp,s,v){fp[s] = v;})((function(fp,s){return fp[s];})(r$128,"style"),"backgroundColor","yellow");
(function(e,s){e.innerHTML = s;})(r$128,res$127);
return false;
};
js$0Js$1.installEventHandler$197(dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.input$65,5,function(v$227){return dmfp$33$313$33$3js$0dmfp$33$313$33$3js$1.comp$123(v$227);
});
return 0;
})();
