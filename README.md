
# Minor-Minimal Graph Functions

A Mathematica package that contains functions 
related to finding the forbidden minors of a given graph property.
In particular, this package has functions relating to the properties
of a graph being [apex][APEX], edge-apex, or contraction-apex.
Additionally in the *brute-force-search* directory
is a Mathematica notebook containing the details how to perform 
a brute-force search for certain classes of minor-minimal graphs.

  ![K6 and the Jorgensen Graph](https://raw.githubusercontent.com/mikepierce/MMGraphFunctions/master/images/k6andjorgenson.png)

According to the [Robertson&ndash;Seymour Theorem][RST],
given any property of graphs *P* that is closed under taking [minors][MINOR]
(if graph *G* has *P*, then every minor of *G* has *P*), 
there is some finite obstruction set of graphs such that 
each obstruction doesn't have *P*, 
but every minor of an obstruction has *P*.
This finite set is referred to as the *minor-minimal non*-P graphs,
or sometimes also the forbidden minors of the property *P*.
The significance of this obstruction set is that it 
completely characterizes the graphs with property *P*.
That is, a graph *G* has *P* if and only if
it does not have one of these obstructions as a minor.

The classic example of this, preceding the 
Robertson&ndash;Seymour Theorem, is [Wagner's Theorem][WAGNER].
According to Wagner's Theorem, a graph is [planar][PLANAR] if and only if
it does not contain either *K<sub>5</sub>* or *K<sub>3,3</sub>* as a minor.
Therefore the two graphs *K<sub>5</sub>* or *K<sub>3,3</sub>* 
are the obstructions to graph planarity.

This Mathematica package was created to help search for the 
forbidden minors to the properties of a graph 
being apex, edge-apex, or contraction-apex.
Unfortunately the properties edge-apex and contraction-apex
aren't closed under taking minors. So while their obstruction sets 
don't completely characterize the property, 
they are still worthwhile to search for.

For further reading, see:

 -  *Searching for and Classifying the Finite Set
	of Minor-Minimal Non-Apex Graphs*, Pierce ([pdf][MPTHESIS])
 -  *Six Variations on a Theme: Almost Planar Graphs*, 
	Lipton, Mackall, Mattman, Pierce, Robertson, and Weinschelbaum 
	(publication under review)
 -  *Examples of Obstructions to Apex Graphs,
	Edge-Apex Graphs, and Contraction-Apex Graphs*, Pierce ([pdf][MPPRESENT])
 -  *The* K<sub>n+5</sub> *and* K<sub>3<sup>2</sup>,1<sup>n</sup></sub>
	*families are obstructions to n-apex*, Mattman and Pierce ([arXiv][KN5K321N])

  [APEX]: https://en.wikipedia.org/wiki/Apex_graph
  [RST]: https://en.wikipedia.org/wiki/Robertson%E2%80%93Seymour_theorem  
  [MINOR]: https://en.wikipedia.org/wiki/Graph_minor
  [WAGNER]: https://en.wikipedia.org/wiki/Wagner%27s_theorem
  [PLANAR]: https://en.wikipedia.org/wiki/Planar_graph
  [MPTHESIS]: http://www.csuchico.edu/~tmattman/mpthesis.pdf
  [MPPRESENT]: http://math.ucr.edu/~mpierce/files/pierce-2015-EOAGEAGCAG-presentation.pdf
  [KN5K321N]: http://arxiv.org/abs/1603.00885



# Constants

  This package contains the following graphs as constants:
  
  Name | Graph
  ----:| :---
  `K5` | *K<sub>5</sub>* , the complete graph on five vertices
  `K6` | *K<sub>6</sub>* , the complete graph on six vertices
  `K33`| *K<sub>3,3</sub>* , the complete bipartite graph on two sets of three vertices
  `J1` | The J&oslash;rgensen graph



# Important Functions

 -  `NonApexGraphQ[g]` takes a graph *g*
    and yields True if *g* is non-apex and False otherwise.
   
    There are also functions `NonEdgeApexGraphQ[g]` 
    and `NonContractionApexGraphQ[g]` defined similarly.

 -  `MMGraphQ[P,g]` takes a graph property *P* 
    such that *&not;P* is closed under taking minors
    and a graph *g* and returns True if *g* is minor-minimal
    with respect to *P* and False otherwise.
    This function is defined pretty simply as 
   
    ```Mathematica
      MMGraphQ[P,g] := Return[P[g] && !MemberQ[P /@ SimpleMinors[g], True]];
    ```

    There are three specific implementations of this function.
    Firstly there is `MMNAGraphQ[g]` which is simply defined
    as `MMGraphQ[NonApexGraphQ,g]`.
    There are also functions `MMNEGraphQ[g]` and `MMNCGraphQ[g]`, 
	but these have to defined differently because neither of the properties
    edge-apex or contraction-apex are closed under taking minors.

 -  `SimpleMinors[g]` takes a graph *g* and returns
    a list of the simple minors of *g*. 
    Specifically it returns a list of all distinct graphs 
    that are the result of either deleting an edge, 
    contracting an edge, or deleting a degree-*0* vertex in *g*.  
    
    `SimpleMinors[g,n]` returns the distinct minors with a minimum
    vertex degree of *n*.

# Supplementary Functions

 -  `EdgeContract[g,e]` contracts the edge *e* in the graph *g*.
    Note that this function is built into Mathematica 10.

 -  `DeleteGraphDuplicates[{g1, ..., gn}]` removes duplicate graphs 
    (up to isomorphism) from the list
    *{g<sub>1</sub>, &#8230;, g<sub>n</sub>}*.

 -  `GraphSimplify[g]` simplifies the graph *g* so that the result
    will have no degree-*0*, -*1*, or -*2* vertices.
    
    `GraphSimplify[]` will print an outline 
	of the graph simplification algorithm.

 -  `GraphColor[g]` displays the graph *g* with edges and vertices colored
	according to their equivalence. In particular, the edges 
	*e<sub>1</sub>* and *e<sub>2</sub>* (respectively the vertices
	*v<sub>1</sub>* and *v<sub>2</sub>*) will be colored the same if
    *g-e<sub>1</sub>* and *g-e<sub>2</sub>* (respectively 
	*g-v<sub>1</sub>* and *g-v<sub>2</sub>*) are isomorphic.

 -  `GraphModel[g]` displays the graph *g* in various different layouts
	with the edges and vertices colored with `GraphColor`.

	`GraphModel[g,n]` displays *n* different layouts of *g* like above.

   

# ToDo
 
 -  Add a citation to the README and to the Brute-Force-Search.nb
    as soon as that paper actually gets published.
 -  Add any of the other relevant function that I have in scratch notebooks
    to this package.
 -  Be open to suggestions/recommendations/requests.


