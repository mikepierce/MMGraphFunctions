(* ::Package:: *)

BeginPackage["MMGraphFunctions`"]


MMGraphFunctions::usage = "MMGraphFunctions: A Mathematica package containing\
  functions to aid in the search for minor-minimal graphs given a certain property.

Functions include: MMGraphQ, NonApexGraphQ, MMNAGraphQ, NonEdgeApexGraphQ,\
MMNEGraphQ, NonContractionApexGraphQ, MMNCGraphQ, EdgeContract,\
DeleteGraphDuplicates, SimpleMinors, GraphSimplify, GraphColor, GraphModel.

Constants include: K5, K33, K6, J1.";


(* The complete graph on five vertices *)
K5 = Graph[UndirectedEdge @@@ Subsets[Range@5, {2}], 
        {VertexSize -> Medium, VertexStyle -> Black, EdgeStyle -> Black}
];

(* The complete bipartite graph with two set of three vertices each *)
K33 = Graph[UndirectedEdge @@@ Tuples[{{1, 2, 3}, {4, 5, 6}}], 
        {VertexSize -> Medium, VertexStyle -> Black, EdgeStyle -> Black}
];

(* The complete graph on six vertices *)
K6 = Graph[UndirectedEdge @@@ Subsets[Range@6, {2}], 
        {VertexSize -> Medium, VertexStyle -> Black, EdgeStyle -> Black}
];

(* The Jorgensen Graph *)
J1 = Graph[UndirectedEdge @@@ {{1,4},{1,5},{1,6},
    {1,7},{1,8},{2,3},{2,5},{2,6},{2,7},{2,8},{3,4},{3,6},
    {3,7},{3,8},{4,5},{4,7},{4,8},{5,7},{5,8},{6,7},{6,8}},
        {VertexSize -> Medium, VertexStyle -> Black, EdgeStyle -> Black}
];


MMGraphQ::usage = "MMGraphQ[P, g] takes a graph property P such that \[Not]P \
is closed under taking minors and a graph g and returns True if g \
is minor-minimal with respect to P and False otherwise.";

MMGraphQ[F_Symbol, G_Graph] := Module[{},
  Return[F[G] && ! MemberQ[F /@ SimpleMinors[G], True]];
];


NonApexGraphQ::usage = "NonApexGraphQ[g] takes a graph g and yields True \
if g is non-apex and False otherwise.";

NonApexGraphQ[G_Graph] := Module[{},
  Return[! MemberQ[PlanarGraphQ[VertexDelete[G, #]] & /@ VertexList[G], True]];
];


MMNAGraphQ::usage = "MMNAGraphQ[g] takes a graph g and yields True \
if g is minor-minimal on-apex and False otherwise.";

MMNAGraphQ[G_Graph] := Module[{},
  Return[MMGraphQ[NonApexGraphQ, G]];
];


NonEdgeApexGraphQ::usage = "NonEdgeApexGraphQ[g] takes a graph g and yields True \
if g is non-edge-apex and False otherwise.";

NonEdgeApexGraphQ[G_Graph] := Module[{},
  Return[! MemberQ[PlanarGraphQ[EdgeDelete[G, #]] & /@ EdgeList[G], True]];
];


MMNEGraphQ::usage = "MMNEGraphQ[g] takes a graph g and yields True \
if g is minor-minimal non-edge-apex and False otherwise. \
Since edge-apexness is not closed under taking minors, \
this function may take some time to verify that a given graph \
is minor-minimal non-edge-apex.";

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


NonContractionApexGraphQ::usage = "NonContractionApexGraphQ[g] takes a graph g \
and yields True if g is non-contraction-apex and False otherwise.";

NonContractionApexGraphQ[G_Graph] := Module[{},
  Return[! MemberQ[PlanarGraphQ[EdgeContract[G, #]] & /@ EdgeList[G], True]];
];


MMNCGraphQ::usage = "MMNCGraphQ[g] takes a graph g and yields True \
if g is minor-minimal non-contraction-apex and False otherwise. \
Since contraction-apexness is not closed under taking minors, \
this function may take some time to verify that a given graph \
is minor-minimal non-contraction-apex.";

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


(* As of Mathematica 10, this is a built-in function *)
If[$VersionNumber < 10,

  EdgeContract::usage = "EdgeContract[g, e] contracts \
  the edge e of the graph g.";

  EdgeContract[G_Graph, E_UndirectedEdge] := Module[
    {v1 = Min[E[[1]], E[[2]]], v2 = Max[E[[1]], E[[2]]], graph = EdgeDelete[G, E], edges},
    (*  Get edges of v2 that need to be transferred.  *)
    edges = UndirectedEdge[Min[v1, #], Max[v1, #]] & /@ Complement[AdjacencyList[graph, v2], AdjacencyList[graph, v1]];
    Return[VertexDelete[EdgeAdd[graph, edges], v2]];
  ];

];


DeleteGraphDuplicates::usage = "DeleteGraphDuplicates[{g1, \[Ellipsis], gn}] removes \
duplicate graphs (up to isomorphism) from the list {g1, \[Ellipsis], gn}.";

DeleteGraphDuplicates[G_List] := Module[{},
  Return[DeleteDuplicates[G, IsomorphicGraphQ[#1, #2] &]];
];


SimpleMinors::usage = "SimpleMinors[g] takes a graph g \
and returns a list of the simple minors of g. \
Specifically it returns a list of all distinct graphs \
that are the result of either deleting an edge, contracting an edge, \
or deleting a degree-0 vertex in g.

SimpleMinors[g,n] returns the distinct minors \
with a minimum vertex degree of n.";

SimpleMinors[G_Graph, n_Integer: 0] := Module[
  {vertices, edges, vertexremovals = {}, edgeremovals = {}, edgecontractions = {}},

  vertices = Cases[VertexList[G], v_ /; VertexDegree[G, v] == 0];
  edges = DeleteDuplicates[EdgeList[G], IsomorphicGraphQ[EdgeDelete[G, #1], EdgeDelete[G, #2]] &];

  If[vertices != {}, AppendTo[vertexremovals, VertexDelete[G, First[vertices]]]];
  edgeremovals = EdgeDelete[G, #] & /@ edges;
  edgecontractions = EdgeContract[G, #] & /@ edges;

  Return[DeleteCases[Join[vertexremovals, edgeremovals, edgecontractions], g_ /; Min[VertexDegree[g]] < n]];
];


GraphSimplify::usage = "GraphSimplify[g] simplifies the graph g \
so that the result will have no degree-0, -1, or -2 vertices.

GraphSimplify[] will print an outline of the graph simplification algorithm.";

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


GraphColor::usage = "GraphColor[g] displays the graph g \
with edges and vertices colored according to their equivalence. \
In particular, the edges e1 and e2 (respectively the vertices v1 and v2) \
will be colored the same if g-e1 and g-e2 (respectively g-v1 and g-v2) \
are isomorphic.";

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


GraphModel::usage = "GraphModel[g] displays the graph g \
in various different layouts with the edges and vertices \
colored with GraphColor.

GraphModel[g,n] displays n different layouts of g like above.";

GraphModel[G_Graph, N_Integer:20] := Module[{H},
  H = RemoveProperty[G, {GraphHighlight, GraphHighlightStyle, GraphLayout, GraphStyle, EdgeShapeFunction, EdgeStyle, VertexCoordinates,
      VertexShapeFunction, VertexShape, VertexSize, VertexStyle}];
  Return@Table[GraphColor@Graph@RandomSample[#,Length@#]&@EdgeList@H,{N}];
 ];


EndPackage[ ]
