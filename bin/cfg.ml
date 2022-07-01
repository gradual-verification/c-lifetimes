open Core

module type NodeSig = sig
  type node_type (* the type of the data stored *)

  val getID :
    node_type -> int (* node ids for a list of n nodes should be 0 to n *)
end

module type EdgeSig = sig
  type edge_type (* the type of the edge label *)
end

module MakeGraph (Node : NodeSig) (Edge : EdgeSig) = struct
  type edge = Node.node_type * Node.node_type * Edge.edge_type

  type graph = {
    nodes : Node.node_type array;
    edges : (int, (int * Edge.edge_type) list, Int.comparator_witness) Map.t;
  }

  let make_graph (l : Node.node_type array) =
    {
      nodes = l;
      edges =
        (let to_empty n = (Node.getID n, []) in
         Map.of_alist_exn (module Int) (Array.to_list (Array.map l ~f:to_empty)));
    }

  let check_valid_id (g : graph) (source : string) (id : int) =
    let len = Array.length g.nodes in
    if 0 <= id && id < len then ()
    else
      let msg =
        Printf.sprintf
          "while %s tried to add a node with id %i (graph has %i nodes)" source
          id len
      in
      failwith msg

  let add_edge (g : graph) (e : edge) =
    let u, v, et = e in
    let u_id = Node.getID u in
    let v_id = Node.getID v in
    let fmap = Fun.flip Option.( >>| ) in
    let check = check_valid_id g "adding edge" in
    check u_id;
    check v_id;
    { g with edges = Map.change g.edges u_id ~f:(fmap (List.cons (v_id, et))) }

  let add_edges (g : graph) (es : edge list) =
    List.fold es ~init:g ~f:(fun current_g e -> add_edge current_g e)
  
  let succ (g : graph) (n : Node.node_type) = 
    let extractNode (id, _) = Array.get g.nodes id in
    let edges = Map.find_exn g.edges (Node.getID n) in
    List.map edges ~f:extractNode
end

module CFGNode = struct
  type inst (* placeholder for now *)
  type node_type = inst * int

  let getID (_, n) = n
end

module CFGEdge = struct
  type edge_type = Direct | CondT | CondF
end

module CFG = MakeGraph (CFGNode) (CFGEdge)
