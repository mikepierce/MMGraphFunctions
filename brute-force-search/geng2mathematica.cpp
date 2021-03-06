
///////////////////////////////////////////////////////////////////////////////
// geng2mathematica.cpp
// Pierce, Mike
//
// Converts text generated by
//   geng -c -d# -D# n min:max | planarg -v | showg -e
// into a list of ordered pairs (edges) that can be read by Mathematica.
//
// This also increments each numeral by 1,
// because showg -e starts the vertex enumeration at 0,
// but Mathematica doesn't like that and wants to start at 1.
//
///////////////////////////////////////////////////////////////////////////////



#include <iostream>
#include <string.h>
#include <limits>
using namespace std;



int main() {

    string graph_header = "";     // If it equals "Graph", read an edgelist.
    unsigned long int count = 0;  // Number of edges for an edgelist.
    unsigned long int v1 = 0;     // First vertex of edge.
    unsigned long int v2 = 0;     // Second vertex of edge.
    bool first_run = true;        // First Run.

    while(cin.peek() != -1) {
        if(isspace(cin.peek())) {
            cin.ignore();
        }
        else if(isalpha(cin.peek())) {
            cin >> graph_header;
            if(graph_header == "Graph") {
                cin.ignore(numeric_limits<streamsize>::max(), '\n');
                cin >> count;
                cin >> count;
                if(!first_run) cout << " }" << endl;
                first_run = false;
                cout << "{ ";
                for(unsigned int i = 0; i < count; ++i) {
                    cin >> v1;
                    cin >> v2;
                    v1++;
                    v2++;
                    cout << "{" << v1 << "," << v2 << "}";
                    if (i < count - 1) {cout << ",";}
                }
            }
        }
        else{
            cerr << "The input was not formatted as expected." << endl;
            return 1;
        }
    }
    cout << " }" << endl;
    return 0;
}

