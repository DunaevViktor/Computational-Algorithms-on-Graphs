(* ::Package:: *)

BeginPackage["UserFunctions`"]


GetGraph::usage = "GetGraph[vertices,edges] returns prestyled graph";


PSolution::usage = "PSol[vertices,edges,dinast,dirs,pred,weight] return private solution"


Begin["`Private`"];


GetGraph[vertices_,edges_]:=Block[{}, Graph[vertices,edges,VertexSize->Large, VertexStyle->White, VertexLabels->Placed["Name",Center],VertexLabelStyle->Directive[Black,Italic,25], GraphLayout->{"CircularEmbedding"},EdgeShapeFunction->"Arrow", EdgeStyle->Black]]


End[]


PSolution[vertices_,edges_,dinast_,dirs_,pred_,weight_]:=Block[{xp,n,i,z},(
xp =Table[0,Length[vertices]];
For[n = Length[vertices],n>1,n--,
i = dinast[[n]];
xp[[i]]+=-dirs[[i]]*weight[[i]];
xp[[pred[[i]]]]+= dirs[[i]]*dirs[[pred[[i]]]]*xp[[i]];
];
(Subscript[z, #[[1]],#[[2]]]=0)&/@edges;
If[dirs[[#]] ==1,Subscript[z, pred[[#]],#]=xp[[#]]]&/@Range[Length[vertices]];
If[dirs[[#]] ==-1,Subscript[z, #,pred[[#]]]=xp[[#]]]&/@Range[Length[vertices]];
(Subscript[\!\(\*OverscriptBox[\(x\), \(~\)]\), #[[1]],#[[2]]]->Subscript[z, #[[1]],#[[2]]])&/@edges
)]


GSolution[edges_,depth_,dirs_,pred_,Ut_,Un_]:=Module[{table, deltas,edgesSet},(
table = Table[0,Length[Un],Length[edges]];
edgesSet= Join[Un,Ut];
For[i=1,i<= Length[Un],i++,
\[Tau] = Un[[i]][[1]];
\[Rho]=  Un[[i]][[2]];
\!\(\*OverscriptBox[\(\[Delta]\), \(~\)]\) = 0;
table[[i]][[i]]=1;
If[depth[[\[Tau]]]>depth[[\[Rho]]],\!\(\*OverscriptBox[\(\[Delta]\), \(~\)]\) =1,\[Tau]=Un[[i]][[2]];\[Rho]=  Un[[i]][[1]];\!\(\*OverscriptBox[\(\[Delta]\), \(~\)]\) =-1];
depthDelta = depth[[\[Tau]]]-depth[[\[Rho]]];
For[j=0,j<depthDelta,j++,
ver= pred[[\[Tau]]];
If[dirs[[\[Tau]]]>0,rib = ver\[DirectedEdge]\[Tau],rib = \[Tau]\[DirectedEdge]ver];
pos = Position[edgesSet,rib][[1]];
table[[i]][[pos]]=dirs[[\[Tau]]]*\!\(\*OverscriptBox[\(\[Delta]\), \(~\)]\) ;
\[Tau] = ver;
];
If[\[Tau]!= \[Rho],
While[True,
predT = pred[[\[Tau]]];
predRho = pred[[\[Rho]]];
If[dirs[[\[Tau]]]>0,ribT = predT\[DirectedEdge]\[Tau],ribT = \[Tau]\[DirectedEdge]predT];
If[dirs[[\[Rho]]]>0,ribRho = predRho\[DirectedEdge]\[Rho],ribRho = \[Rho]\[DirectedEdge]predRho];
posT = Position[edgesSet,ribT][[1]];
table[[i]][[posT]]=dirs[[\[Tau]]]*\!\(\*OverscriptBox[\(\[Delta]\), \(~\)]\) ;
posRho = Position[edgesSet,ribRho][[1]];
table[[i]][[posRho]]=dirs[[\[Rho]]]*\!\(\*OverscriptBox[\(\[Delta]\), \(~\)]\) *-1;
\[Tau] = predT;
\[Rho] = predRho;
If[\[Tau]== \[Rho],Break[]]
]
]
];
table=Transpose[table];
Print@TableForm[table,TableHeadings->{edgesSet,Un}];
table=Transpose[table];
deltas = Table[\!\(
\*SubsuperscriptBox[\(\[Delta]\), \(\(edgesSet[\([j]\)]\)[\([1]\)], \(edgesSet[\([j]\)]\)[\([2]\)]\), \(Un[\([i]\)]\)] -> \(\(table[\([i]\)]\)[\([j]\)]\)\),{i,1,Length[Un]},{j,1,Length[edgesSet]}]//Flatten)]


BalanceSolve[deltas_,pSol_,Ut_,Un_]:=Block[{unLen,x},(
unLen = Length[Un];
(Subscript[x, #[[1]],#[[2]]]->Sum[Subscript[x, Un[[i]][[1]],Un[[i]][[2]]]*\!\(\*SubsuperscriptBox[\(\[Delta]\), \(#\[LeftDoubleBracket]1\[RightDoubleBracket], #\[LeftDoubleBracket]2\[RightDoubleBracket]\), \(Un\[LeftDoubleBracket]i\[RightDoubleBracket]\)]\),{i,1,unLen}]+Subscript[\!\(\*OverscriptBox[\(x\), \(~\)]\), #[[1]],#[[2]]])/.pSol/.deltas&/@Ut
)]


EndPackage[]
