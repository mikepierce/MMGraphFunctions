(* ::Package:: *)

BeginPackage["PierceMMPack`"]


PierceMMPack::usage = "PierceMMPack: Mathematica package containing functions to aid in the search for minor-minimal graphs.
Functions include: MMGraphQ, NonApexGraphQ, MMNAGraphQ, NonEdgeApexGraphQ, MMNEGraphQ, NonContractionApexGraphQ, MMNCGraphQ, \
EdgeContract, DeleteGraphDuplicates, SimpleMinors, GraphSimplify, GraphColor, GraphModel.
Constants include: K5, K33.";

MMGraphQ::usage = "\!\(\*RowBox[{\"MMGraphQ \", \"[\", RowBox[{StyleBox[\"P\", \"TI\"], \",\", StyleBox[\"g\", \"TI\"]}], \"]\"}]\) \
given a graph property \!\(\*StyleBox[\"P\", \"TI\"]\) such that \!\(\*StyleBox[\"\[Not]P\", \"TI\"]\) is closed under taking minors, \
yields True if \!\(\*StyleBox[\"g\", \"TI\"]\) is minor-minimal with respect to \!\(\*StyleBox[\"P\", \"TI\"]\) and False otherwise.
For example, MMGraphQ[NonApexGraphQ, CompleteGraph[6]] returns True.
Relies on functions: SimpleMinors";

NonApexGraphQ::usage = "\!\(\*RowBox[{\"NonApexGraphQ\", \"[\",StyleBox[\"g\", \"TI\"], \"]\"}]\) \
yields True if \!\(\*StyleBox[\"g\", \"TI\"]\) is non-apex and False otherwise.";

MMNAGraphQ::usage = "\!\(\*RowBox[{\"MMNAGraphQ\", \"[\",StyleBox[\"g\", \"TI\"], \"]\"}]\) \
yields True if \!\(\*StyleBox[\"g\", \"TI\"]\) is minor-minimal non-apex and False otherwise.
Relies on functions: NonApexGraphQ, MMGraphQ";

NonEdgeApexGraphQ::usage = "\!\(\*RowBox[{\"NonEdgeApexGraphQ\", \"[\",StyleBox[\"g\", \"TI\"], \"]\"}]\) \
yields True if \!\(\*StyleBox[\"g\", \"TI\"]\) is non-edge-apex and False otherwise.";

MMNEGraphQ::usage = "\!\(\*RowBox[{\"MMNEGraphQ\", \"[\",StyleBox[\"g\", \"TI\"], \"]\"}]\) \
yields True if \!\(\*StyleBox[\"g\", \"TI\"]\) is minor-minimal non-edge-apex and False otherwise. \
Since edge-apexness is not closed under taking minors, \
this function may take some time to verify that a given graph is minor-minimal non-edge-apex.
Relies on functions: NonEdgeApexGraphQ, MMGraphQ";

NonContractionApexGraphQ::usage="\!\(\*RowBox[{\"NonContractionApexGraphQ\", \"[\",StyleBox[\"g\", \"TI\"], \"]\"}]\) \
yields True if \!\(\*StyleBox[\"g\", \"TI\"]\) is non-contraction-apex and False otherwise.";

MMNCGraphQ::usage="\!\(\*RowBox[{\"MMNCGraphQ\", \"[\",StyleBox[\"g\", \"TI\"], \"]\"}]\) \
yields True if \!\(\*StyleBox[\"g\", \"TI\"]\) is minor-minimal non-contraction-apex and False otherwise. \
Since contraction-apexness is not closed under taking minors, \
this function may take some time to verify that a given graph is minor-minimal non-contraction-apex.
Relies on functions: NonContractionApexGraphQ, MMGraphQ";

If[$VersionNumber < 10,
EdgeContract::usage = "\!\(\*RowBox[{\"EdgeContract \", \"[\", RowBox[{StyleBox[\"g\", \"TI\"], \",\", StyleBox[\"e\", \"TI\"]}], \"]\"}]\) \
makes a graph by contracting the edge \!\(\*StyleBox[\"e\", \"TI\"]\) in the graph \!\(\*StyleBox[\"g\", \"TI\"]\).";
];

DeleteGraphDuplicates::usage = "\!\(\*RowBox[{\"DeleteGraphDuplicates\", \"[\", \
RowBox[{SubscriptBox[StyleBox[\"{g\", \"TI\"], StyleBox[\"1\", \"TR\"]], \
\",\", \
SubscriptBox[StyleBox[\"g\", \"TI\"], StyleBox[\"2\", \"TR\"]], \
\", ... ,\", \
SubscriptBox[StyleBox[\"g\", \"TI\"], StyleBox[\"n\", \"TR\"]],\"}\"}], \"]\"}]\) removes graphs that are duplicates under isomorphism from \
\!\(\*RowBox[{ \
RowBox[{SubscriptBox[StyleBox[\"{g\", \"TI\"], StyleBox[\"1\", \"TR\"]], \
\",\", \
SubscriptBox[StyleBox[\"g\", \"TI\"], StyleBox[\"2\", \"TR\"]], \
\", ... ,\", \
SubscriptBox[StyleBox[\"g\", \"TI\"], StyleBox[\"n\", \"TR\"]],\"}\"}]}]\).";

SimpleMinors::usage = "\!\(\*RowBox[{\"SimpleMinors\", \"[\",StyleBox[\"g\", \"TI\"],\"]\"}]\) \
returns the list of all simple minors of \!\(\*StyleBox[\"g\", \"TI\"]\).
\!\(\*RowBox[{\"SimpleMinors \", \"[\", RowBox[{StyleBox[\"g\", \"TI\"], \",\", StyleBox[\"n\", \"TI\"]}], \"]\"}]\) \
returns the list of all simple minors of \!\(\*StyleBox[\"g\", \"TI\"]\) with a minimum vertex degree of \!\(\*StyleBox[\"n\", \"TI\"]\) .
We define a simple minor to be the result of either contracting a single edge, deleting a single edge, or deleting a degree-0 vertex.
Relies on functions: EdgeContract";

GraphSimplify::usage = "\!\(\*RowBox[{\"GraphSimplify\", \"[\",StyleBox[\"g\", \"TI\"],\"]\"}]\) \
simplifies a graph \
\!\(\*StyleBox[\"g\", \"TI\"]\) \
such that the result has no degree 0, 1, or 2 vertices. \n\
GraphSimplify[] with no arguments will print an outline of the graph simplification algorithm.";

GraphColor::usage = "\!\(\*RowBox[{\"GraphColor\", \"[\",StyleBox[\"g\", \"TI\"], \"]\"}]\) \
displays graph \!\(\*StyleBox[\"g\", \"TI\"]\) with edges and vertices colored according to their equivalence.";

GraphModel::usage = "\!\(\*RowBox[{\"GraphModel\", \"[\",StyleBox[\"g\", \"TI\"], \"]\"}]\) \
displays graph \!\(\*StyleBox[\"g\", \"TI\"]\) in various different layouts \
with edges and vertices colored according to their equivalence.
\!\(\*RowBox[{\"GraphModel\", \"[\",StyleBox[\"g\", \"TI\"], \",\",StyleBox[\"n\",\"TI\"],\"]\"}]\) \
displays \!\(\*StyleBox[\"n\", \"TI\"]\) instances of graph \!\(\*StyleBox[\"g\", \"TI\"]\) in various different layouts \
with edges and vertices colored according to their equivalence.
Relies on functions: GraphColor.";







Begin["`Private`"]

K5 = Graph[
   UndirectedEdge @@@ Subsets[Range@5, {2}], {VertexSize -> Medium, 
    VertexStyle -> Black, EdgeStyle -> Black}];

K33 = Graph[
   UndirectedEdge @@@ 
    Tuples[{{1, 2, 3}, {4, 5, 6}}], {VertexSize -> Medium, 
    VertexStyle -> Black, EdgeStyle -> Black}]; 



MMGraphQ[F_Symbol, G_Graph] := Module[{},
  Return[F[G] && ! MemberQ[F /@ SimpleMinors[G], True]];
];


NonApexGraphQ[G_Graph] := Module[{},
  Return[! MemberQ[PlanarGraphQ[VertexDelete[G, #]] & /@ VertexList[G], True]];
];


MMNAGraphQ[G_Graph] := Module[{},
  Return[MMGraphQ[NonApexGraphQ, G]];
];


NonEdgeApexGraphQ[G_Graph] := Module[{},
  Return[! MemberQ[PlanarGraphQ[EdgeDelete[G, #]] & /@ EdgeList[G], True]];
];


MMNEGraphQ[G_Graph] := Module[{sieve},
  If[!MMGraphQ[NonEdgeApexGraphQ, G], Return@False];
  sieve = {G} ~Join~ DeleteGraphDuplicates@Table[EdgeDelete[G, e], {e,EdgeList@G}];
  While[sieve != {},
    sieve = Flatten@Table[EdgeContract[g, #] & /@ EdgeList@g, {g, sieve}];
    sieve = DeleteGraphDuplicates@DeleteCases[sieve, g_ /; PlanarGraphQ@g];
    If[MemberQ[sieve, g_ /; NonEdgeApexGraphQ@g], Return@False];
  ];
  Return@True;
];


NonContractionApexGraphQ[G_Graph] := Module[{},
  Return[! MemberQ[PlanarGraphQ[EdgeContract[G, #]] & /@ EdgeList[G], True]];
];


MMNCGraphQ[G_Graph] := Module[{sieve},
  If[!MMGraphQ[NonContractionApexGraphQ, G], Return@False];
  sieve = {G} ~Join~ DeleteGraphDuplicates@Table[EdgeContract[G, e], {e,EdgeList@G}];
  While[sieve != {},
    sieve = Flatten@Table[EdgeDelete[g, #] & /@ EdgeList@g, {g, sieve}];
    sieve = DeleteGraphDuplicates@DeleteCases[sieve, g_ /; PlanarGraphQ@g];
    If[MemberQ[sieve, g_ /; NonContractionApexGraphQ@g], Return@False];
  ];
  Return@True;
];


If[$VersionNumber < 10,
  EdgeContract[G_Graph, E_UndirectedEdge] := Module[
    {v1 = Min[E[[1]], E[[2]]], v2 = Max[E[[1]], E[[2]]], graph = EdgeDelete[G, E], edges},
    (*  Get edges of v2 that need to be transferred.  *)
    edges = UndirectedEdge[Min[v1, #], Max[v1, #]] & /@ Complement[AdjacencyList[graph, v2], AdjacencyList[graph, v1]];
    Return[VertexDelete[EdgeAdd[graph, edges], v2]];
  ];
];


DeleteGraphDuplicates[G_List] := Module[{},
  Return[DeleteDuplicates[G, IsomorphicGraphQ[#1, #2] &]];
];


SimpleMinors[G_Graph, n_Integer: 0] := Module[
  {vertices, edges, vertexremovals = {}, edgeremovals = {}, edgecontractions = {}},

  vertices = Cases[VertexList[G], v_ /; VertexDegree[G, v] == 0];
  edges = DeleteDuplicates[EdgeList[G], IsomorphicGraphQ[EdgeDelete[G, #1], EdgeDelete[G, #2]] &];

  If[vertices != {}, AppendTo[vertexremovals, VertexDelete[G, First[vertices]]]];
  edgeremovals = EdgeDelete[G, #] & /@ edges;
  edgecontractions = EdgeContract[G, #] & /@ edges;

  Return[DeleteCases[Join[vertexremovals, edgeremovals, edgecontractions], g_ /; Min[VertexDegree[g]] < n]];
];


GraphSimplify[] := Module[{},
  Print[
"While[G has degree 0, 1, or 2 vertices]{
  While[G has degree 0 or 1 vertices]{
    Delete all degree 0 vertices.
    Delete all degree 1 vertices.
  }
  While[There is a degree 2 vertex in G]{
    Pick a degree 2 vertex from G and delete it.
    If the neighbors of that vertex weren't connected, then connect them.
  }
}"
  ];];

GraphSimplify[GraphPristine_Graph] := Module[
  {G = GraphPristine, deg2v, al, edge},

  While[Min[VertexDegree[G]] <= 2,
    While[Min[VertexDegree[G]] <= 1,
      If[G == Graph[{}], Return[G]]; (*Because Mathematica doesnt like running the code below on an empty graph.*)
      G = VertexDelete[G, v_ /; VertexDegree[G, v] == 0]; (*Delete all degree 0 vertices.*)
      If[G == Graph[{}], Return[G]]; (*Because Mathematica doesnt like running the code below on an empty graph.*)
      G = VertexDelete[G, v_ /; VertexDegree[G, v] == 1]; (*Delete all degree 1 vertices.*)
    ];

    While[MemberQ[VertexDegree[G], 2],
      deg2v = Cases[VertexList[G], v_ /; VertexDegree[G, v] == 2][[1]]; (*Find the first (enumerated) vertex of degree 2.*)
      al = AdjacencyList[G, deg2v];
      edge = UndirectedEdge[al[[1]], al[[2]]];
      G = VertexDelete[G, deg2v]; (*Delete our degree 2 vertex.*)
      If[\[Not] MemberQ[EdgeList[G], edge], G = EdgeAdd[G, edge]]; (*Make sure there is a single edge between the neighbors of our vertex.*)
    ];
  ];

  Return[G];
];


GraphColor[G_Graph] := Module[{vertices, edges, vertexcolors, edgecolors, colorproperties},
  vertices = Sort[Gather[VertexList[G], IsomorphicGraphQ[VertexDelete[G, #1], VertexDelete[G, #2]] &], Length[#1] > Length[#2] &];
  edges = Sort[Gather[EdgeList[G], IsomorphicGraphQ[EdgeDelete[G, #1], EdgeDelete[G, #2]] &], Position[vertices, First[List @@ Flatten[#1]]] > Position[vertices, First[List @@ Flatten[#2]]] &];
  vertexcolors = RGBColor @@@ PadRight[{{1,0,0},{1,.5,0},{.9,.9,.1},{0,1,0},{0,1,1},{.8,.5,1},{1,.5,.7},{.5,0,.5},{0,0,.7},{0,.5,0},{.5,.5,.1},{.3,.3,0},{.3,0,0}}, Length[vertices], {{0,0,0}}];
  edgecolors = RGBColor @@@ PadRight[{{0,0,0},{.4,0,.4},{.2,.2,.4},{0,.4,.4},{.2,.4,.2},{.4,.4,0},{.5,.3,0},{.4,.2,.2},{.3,0,0},{1,0,0},{1,.5,0},{.9,.9,.1},{0,1,0},{0,1,1},{.8,.5,1},{1,.5,.7}}, Length[edges], {{0,0,0}}];
  colorproperties = {
    VertexSize -> Medium,
    VertexStyle -> Flatten[Table[# -> vertexcolors[[i]] & /@ vertices[[i]], {i, Length@vertices}]],
    EdgeStyle -> Flatten[Table[# -> {Thickness[.0133], edgecolors[[i]]} & /@ edges[[i]], {i, Length@edges}]]
  };
  Return[SetProperty[G, colorproperties]];
];


GraphModel[G_Graph, N_Integer:20] := Module[{H},
  H = RemoveProperty[G, {GraphHighlight, GraphHighlightStyle, GraphLayout, GraphStyle, EdgeShapeFunction, EdgeStyle, VertexCoordinates,
      VertexShapeFunction, VertexShape, VertexSize, VertexStyle}];
  Return@Table[GraphColor@Graph@RandomSample[#,Length@#]&@EdgeList@H,{N}];
 ];


End[ ]
EndPackage[ ]
