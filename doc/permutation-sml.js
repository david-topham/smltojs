(function(){basis$0General$1.print$163("<html></head></head><body><h1>Permutations and Combinations</h1><h1>input range: [0,12]</h1>n: <input id='a' value='0'/> r: <input id='b' value='0'/><br>P(n,r)=  <span id='c'>?</span><br>C(n,r)=  <span id='d'>?</span></body></html>");
var form$55 = (permutation$0frp2$1.pair$286(function(v$109){return v$109[0] == v$109[1];
},function(v$110){return v$110[0] == v$110[1];
}))([permutation$0frp2$1.textField$407("a"),permutation$0frp2$1.textField$407("b")]);
var fix$275 = {};
fix$275.$factorial = function(v$59){switch (v$59) { case 0: {return 1;
 break; }default: {return SmlPrims.chk_ovf_i32(v$59 * (fix$275.$factorial(SmlPrims.chk_ovf_i32(v$59 - 1))));
} };
};
var factorial$56 = fix$275.$factorial;
var pr$79;
var v$180;
var v$182 = permutation$0frp2$1.s$ttt$610(function(v$111){return v$111[0] == v$111[1];
},function(v$112){return v$112[0] == v$112[1];
},function(v$113){return v$113[0] == v$113[1];
},function(v$114){return v$114[0] == v$114[1];
},[function(v$249){return permutation$0frp2$1.arr$559(function(v$250){return v$250[0] == v$250[1];
},function(v$251){return v$251[0] == v$251[1];
},function(x$252){return (basis$0Option$1.valOf$57(0))(basis$0Int32$1.fromString$462(x$252));
},v$249);
},function(v$253){return permutation$0frp2$1.arr$559(function(v$254){return v$254[0] == v$254[1];
},function(v$255){return v$255[0] == v$255[1];
},function(x$256){return (basis$0Option$1.valOf$57(0))(basis$0Int32$1.fromString$462(x$256));
},v$253);
}]);
v$180 = (function(x$150){var v$226 = v$182(x$150);
return permutation$0frp2$1.arr$559(function(v$227){return (v$227[0][0] == v$227[1][0])?(v$227[0][1] == v$227[1][1]):false;
},function(v$228){return v$228[0] == v$228[1];
},function(v$229){var v$230 = v$229[0];
var v$231 = v$229[1];
return SmlPrims.div_i32(factorial$56(v$230),factorial$56(SmlPrims.chk_ovf_i32(v$230 - v$231)),CompilerInitial.exn$Div$47);
},v$226);
});
pr$79 = (function(x$146){var v$258 = v$180(x$146);
return permutation$0frp2$1.arr$559(function(v$259){return v$259[0] == v$259[1];
},function(v$260){return v$260[0] == v$260[1];
},function(v$273){return basis$0Int32$1.toString$458(v$273);
},v$258);
});
var cr$80;
var v$188;
var v$190 = permutation$0frp2$1.s$ttt$610(function(v$121){return v$121[0] == v$121[1];
},function(v$122){return v$122[0] == v$122[1];
},function(v$123){return v$123[0] == v$123[1];
},function(v$124){return v$124[0] == v$124[1];
},[function(v$261){return permutation$0frp2$1.arr$559(function(v$262){return v$262[0] == v$262[1];
},function(v$263){return v$263[0] == v$263[1];
},function(x$264){return (basis$0Option$1.valOf$57(0))(basis$0Int32$1.fromString$462(x$264));
},v$261);
},function(v$265){return permutation$0frp2$1.arr$559(function(v$266){return v$266[0] == v$266[1];
},function(v$267){return v$267[0] == v$267[1];
},function(x$268){return (basis$0Option$1.valOf$57(0))(basis$0Int32$1.fromString$462(x$268));
},v$265);
}]);
v$188 = (function(x$161){var v$240 = v$190(x$161);
return permutation$0frp2$1.arr$559(function(v$241){return (v$241[0][0] == v$241[1][0])?(v$241[0][1] == v$241[1][1]):false;
},function(v$242){return v$242[0] == v$242[1];
},function(v$243){var v$244 = v$243[0];
var v$245 = v$243[1];
return SmlPrims.div_i32(SmlPrims.div_i32(factorial$56(v$244),factorial$56(SmlPrims.chk_ovf_i32(v$244 - v$245)),CompilerInitial.exn$Div$47),factorial$56(v$245),CompilerInitial.exn$Div$47);
},v$240);
});
cr$80 = (function(x$157){var v$270 = v$188(x$157);
return permutation$0frp2$1.arr$559(function(v$271){return v$271[0] == v$271[1];
},function(v$272){return v$272[0] == v$272[1];
},function(v$274){return basis$0Int32$1.toString$458(v$274);
},v$270);
});
permutation$0frp2$1.insertDOM$303(pr$79(form$55),"c");
permutation$0frp2$1.insertDOM$303(cr$80(form$55),"d");
return 0;
})();
