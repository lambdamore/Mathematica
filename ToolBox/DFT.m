DFT[data_, threshold_:0, sort_:1] := Module[{fs, s1, s = {}, i, mean, af, pf, 
      pos, fr, frpos, fdata, fdatac, n, per, presult}, 
     n = Length[data]; fs = Fourier[data]; 
      s1 = Drop[fs, -Floor[Length[fs]/2]]; For[i = 2, i <= Length[s1], i++, 
       If[(2/Sqrt[n])*Abs[fs][[i]] > threshold, AppendTo[s, i]]]; 
      mean = {Infinity, Abs[fs[[1]]]/Sqrt[n], 0}; 
      af = (2/Sqrt[n])*Abs[fs][[s]]; pf = Arg[fs][[s]]; 
      presult = Append[Transpose[{n/(s - 1.), af, pf}], mean]; 
      If[sort == 1, Sort[presult, #1[[2]] > #2[[2]] & ], presult]]
 
DFT /: DFT::usage = "DFT[data]={{T,\!\(\*SubscriptBox[\(Amp\), \
\(max\)]\),Phase}...{T,\!\(\*SubscriptBox[\(Amp\), \(min\)]\),Phase}}\ne.g. \
DFT[Cos[x]+y]={{\[Infinity],y,0},{2\[Pi],1,0}}"
