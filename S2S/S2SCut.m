(* Created with the Wolfram Language : www.wolfram.com *)
S2SCut
S2SCut[polygon_, s2sdirectory_, cpcdirectory_:
      "/Users/lambda/Documents/Data/CPC_Precipitation/global/"] := 
    Block[{cpcprecip, cpclat, cpclon, cpctime, s2sprecip, s2slat, s2slon, 
      s2stime, year = StringTake[StringCases[s2sdirectory, "_"~~__~~"-"], 
         {2, 5}][[1]], interval, points, indexes, cpcfile}, 
     cpcfile = {StringJoin[cpcdirectory, "precip.", year, ".nc"], 
        StringJoin[cpcdirectory, "precip.", ToString[ToExpression[year] + 1], 
         ".nc"]}; {cpcprecip, cpclat, cpclon, cpctime} = 
       Block[{test1, test2}, 
        test1 = (Import[cpcfile[[1]], {"Datasets", #1}] & ) /@ 
           {"precip", "lat", "lon", "time"}; 
         test2 = (Import[cpcfile[[2]], {"Datasets", #1}] & ) /@ 
           {"precip", "lat", "lon", "time"}; Table[Join[test1[[i]], 
           test2[[i]]], {i, 4}]]; {s2sprecip, s2slat, s2slon, s2stime} = 
       (Import[s2sdirectory, {"Datasets", #1}] & ) /@ {"tp", "latitude", 
         "longitude", "time"}; interval = 
       Block[{start = Position[Abs[cpctime - Min[s2stime]], 
           Min[Abs[cpctime - Min[s2stime]]]]}, 
        {start, start + Length[s2stime] - 1}]; 
      cpcprecip = Block[{DownScale, size}, DownScale[data_, ratio_] := 
          Block[{convoluter}, convoluter = Block[{net}, net = NetInitialize[
                 ConvolutionLayer[1, ratio, "Stride" -> ratio, "Input" -> 
                   Flatten[{1, Dimensions[data]}]]]; NetReplacePart[net, 
                {"Weights" -> ConstantArray[1./(ratio /. List -> Times), 
                   Flatten[{1, 1, ratio}]]}]]; convoluter[{data}][[1]]]; 
         size = {Round[Abs[s2slat[[2]] - s2slat[[1]]]/
             Abs[cpclat[[2]] - cpclat[[1]]]], 
           Round[Abs[s2slon[[2]] - s2slon[[1]]]/Abs[cpclon[[2]] - cpclon[[
                1]]]]}; (DownScale[#1, size] & ) /@ cpcprecip[[
           interval[[1]] ;; interval[[2]]]]]; 
      points = Block[{InOrOut, grids}, InOrOut[point_] := 
          Block[{singletest}, singletest[poly_] := 
             Round[Total[Mod[(#1 - RotateRight[#1] & )[(ArcTan @@ (point - 
                        #1) & ) /@ poly], 2*Pi, -Pi]]/2/Pi] != 0; 
            singletest /@ polygon /. List -> Or]; 
         grids = Block[{scope = MinMax /@ Transpose[Flatten[polygon, 1]], 
            lat, lon}, lat = Select[s2slat, #1 <= scope[[2,2]] && 
                #1 >= scope[[2,1]] & ]; lon = Select[s2slon, 
              #1 <= scope[[1,2]] && #1 >= scope[[1,1]] & ]; 
            Table[Table[{lat[[i]], lon[[j]]}, {j, Length[lon]}], 
             {i, Length[lat]}]]; Select[Flatten[grids, 1], 
          InOrOut[Reverse[#1]] & ]]; indexes = 
       Block[{grids = Table[Table[{s2slat[[i]], s2slon[[j]]}, 
            {j, Length[s2slon]}], {i, Length[s2slat]}]}, 
        (Position[grids, #1][[1]] & ) /@ points]; 
      cpcprecip = (cpcprecip[[1 ;; All,#1[[1]],#1[[2]]]] & ) /@ indexes; 
      s2sprecip = Block[{scale, offset, origin, sindex}, 
        sindex = Position[(Flatten[#1 /. Rule -> List] & ) /@ 
             Import["/Users/lambda/Documents/BoM_1981-01-01.nc", 
              "Annotations"], _?(MemberQ[#1, "Total Precipitation"] & )][[1,
           1]]; {scale, offset} = Import[s2sdirectory, "Annotations"][[sindex,
           1 ;; 2,2]]; origin = (s2sprecip[[1 ;; All,1 ;; All,#1[[1]],#1[[
                2]]]]*scale + offset & ) /@ indexes; 
         Transpose[Table[If[i == 1, origin[[1 ;; All,1,1 ;; All]], 
            origin[[1 ;; All,i,1 ;; All]] - origin[[1 ;; All,i - 1,
              1 ;; All]]], {i, Dimensions[origin][[2]]}]]]; 
      {cpcprecip, s2sprecip}]
