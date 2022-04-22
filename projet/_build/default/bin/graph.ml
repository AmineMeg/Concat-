module type GRAPHTYPE = sig
    type label = string
    type node = int
    type vert = node * node * label list
    type graph = 
        node list * vert list

    val inter : graph -> graph -> graph
end 

module Graph : GRAPHTYPE =
struct 
    let inter g1 g2 = g1
end