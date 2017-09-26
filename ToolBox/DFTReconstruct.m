DFTReconstruct[fp_, n_] := 
    Table[Sum[fp[[j,2]]*Cos[2*(Pi/fp[[j,1]])*t - fp[[j,3]]], 
      {j, 1, Length[fp]}], {t, 0, n - 1}]
 
DFTReconstruct /: DFTReconstruct::usage = "DFTReconstruct[fp,n]=Reconstructed \
data(of length n) with frequencies listed in fp"
