ReversePCA[data_, nPC_] := Module[{eigen, pca, v, reconstruct}, 
     {eigen, pca, v} = PCA[data]; reconstruct = pca[[1 ;; All,1 ;; nPC]] . 
        Transpose[v][[1 ;; nPC,1 ;; All]]; (#1 + Mean[data] & ) /@ 
       reconstruct]
 
PCA[data_] := Module[{ndata, u, s, v, eigen, pca}, 
     ndata = (#1 - Mean[data] & ) /@ data; {u, s, v} = 
       SingularValueDecomposition[ndata]; pca = ndata . v; 
      eigen = Module[{matrix}, matrix = Transpose[pca] . pca; 
         Table[matrix[[i,i]], {i, 1, Length[matrix]}]]; 
      Return[{eigen, pca, v}]]
 
PCA /: PCA::usage = "data is a m*n matrix, m represent sample size, n \
represent dimension\n\t\t\t{eigen,pca,v}=PCA[data]:\n\t\t\teigen is the \
weight of each pca \n\t\t\tpca is a m*n matrix, m represent sample size, n \
represent pca dimension with decreasing importance\n\tv is the transpose \
matrix, each column represents the eigen vector of the eigen value in \
correspondent position is eigen*)\n\tIt should be pre-determined or using \
either\n\t\t\t* Correlation Matrix\n\t\t\t* Covariance Matrix \
\n\tndata=(#-Mean[data])/StandardDeviation[data]&/@data;"
