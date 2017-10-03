(* ::Package:: *)

Cut[data_, polygon_] := Block[{firstcut, secondcut, InOrOut}, 
     firstcut = Block[{scope = Table[MinMax[Flatten[((#1[[i]] & ) /@ 
                #1 & ) /@ polygon]], {i, 2}]}, Select[data, 
         #1[[1]] > scope[[1,1]] && #1[[1]] < scope[[1,2]] && 
           #1[[2]] > scope[[2,1]] && #1[[2]] < scope[[2,2]] & ]]; 
      InOrOut[point_] := Block[{singletest}, 
        singletest[poly_] := Round[Total[Mod[(#1 - RotateRight[#1] & )[
                 (ArcTan @@ (point - #1) & ) /@ poly], 2*Pi, -Pi]]/2/Pi] != 
           0; singletest /@ polygon /. List -> Or]; 
      Select[firstcut, InOrOut[#1[[1 ;; 2]]] & ]]

