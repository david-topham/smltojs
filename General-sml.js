if ((typeof(basis$0General$1)) == "undefined") {basis$0General$1 = {};
};
(function(){basis$0General$1.eq_order$254 = function(v$256,v$257){switch (v$256) { case 0: {switch (v$257) { case 0: {return true;
 break; }default: {return false;
} };
 break; }case 1: {switch (v$257) { case 1: {return true;
 break; }default: {return false;
} };
 break; }case 2: {switch (v$257) { case 2: {return true;
 break; }default: {return false;
} };
 break; } };
};
basis$0General$1.eq_option$258 = function(v$262,v$259){var v$260 = v$259[0];
var v$261 = v$259[1];
switch (v$260[0]) { case 0: {switch (v$261[0]) { case 0: {var v$263 = v$260[1];
var v$264 = v$261[1];
return v$262([v$263,v$264]);
 break; }default: {return false;
} };
 break; }case 1: {switch (v$261[0]) { case 1: {return true;
 break; }default: {return false;
} };
 break; } };
};
basis$0General$1.en$Span$60 = new String("Span");
basis$0General$1.exn$Span$60 = [basis$0General$1.en$Span$60];
basis$0General$1.en$Domain$61 = new String("Domain");
basis$0General$1.exn$Domain$61 = [basis$0General$1.en$Domain$61];
basis$0General$1.en$Chr$62 = new String("Chr");
basis$0General$1.exn$Chr$62 = [basis$0General$1.en$Chr$62];
basis$0General$1.exnName$63 = function(e$66){return e$66[0];
};
basis$0General$1.exnMessage$67 = function(e$70){return e$70[0];
};
basis$0General$1.s$a$74 = function(x$77){return x$77[0];
};
basis$0General$1.s$ik$78 = function(v$280,v$281){return (v$280[0] = v$281,0);
};
basis$0General$1.o$85 = function(v$93,v$94){return function(x$92){return v$93(v$94(x$92));
};
};
basis$0General$1.before$95 = function(v$282,v$283){return v$282;
};
basis$0General$1.ignore$100 = function(a$103){return 0;
};
basis$0General$1.en$Option$106 = new String("Option");
basis$0General$1.exn$Option$106 = [basis$0General$1.en$Option$106];
basis$0General$1.getOpt$107 = function(v$118,v$284){switch (v$118[0]) { case 1: {return v$284;
 break; }default: {return v$118[1];
} };
};
basis$0General$1.isSome$122 = function(v$125){switch (v$125[0]) { case 1: {return false;
 break; }default: {return true;
} };
};
basis$0General$1.valOf$130 = function(v$133){switch (v$133[0]) { case 0: {return v$133[1];
 break; }default: {throw basis$0General$1.exn$Option$106;
} };
};
basis$0General$1.s$k$141 = function(v$265,v$145){var v$146 = v$145[0];
var v$147 = v$145[1];
return v$265([v$146,v$147]);
};
basis$0General$1.not$148 = function(v$151){return v$151?false:true;
};
basis$0General$1.s$jl$156 = function(v$266,v$160){var v$161 = v$160[0];
var v$162 = v$160[1];
return (v$266([v$161,v$162]))?false:true;
};
basis$0General$1.print$163 = function(s$166){return (basis$0Initial$1.printer_get$102(0))(s$166);
};
basis$0General$1.implode$167 = function(chars$170){return SmlPrims.implode(chars$170);
};
basis$0General$1.concat$171 = function(ss$174){return SmlPrims.concat(ss$174);
};
basis$0General$1.s$r$175 = function(v$285,v$286){return v$285 + v$286;
};
basis$0General$1.str$182 = function(c$185){return SmlPrims.implode([c$185,null]);
};
basis$0General$1.size$186 = function(s$189){return s$189.length;
};
basis$0General$1.ord$190 = function(c$193){return c$193;
};
basis$0General$1.chr$194 = function(i$197){if ((i$197 >= 0)?(i$197 < 256):false) {return i$197;
} else {throw basis$0General$1.exn$Chr$62;
};
};
basis$0General$1.explode$213 = function(s$216){var fix$287 = {};
fix$287.$h = function(v$226,v$227){lab$h: while (true) {if (v$226 < 0) {return v$227;
} else {var t$288 = SmlPrims.chk_ovf_i32(v$226 - 1);
var t$289 = [s$216.charCodeAt(v$226),v$227];
var v$226 = t$288;
var v$227 = t$289;
continue lab$h;
};
};
};
var h$217 = fix$287.$h;
return h$217(SmlPrims.chk_ovf_i32(s$216.length - 1),null);
};
return 0;
})();
