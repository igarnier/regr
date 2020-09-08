open Custom_weak

type 'prim raw = { desc : 'prim desc; tag : int; hash : int }

and 'prim desc = Prim of 'prim * 'prim raw list

module type S = sig
  type prim

  type t = prim raw

  val prim : prim -> t list -> t
end

module Make (X : Signature.S) : S with type prim = X.t = struct
  type prim = X.t

  type t = prim raw

  module Table = Make_table (struct
    type nonrec t = t

    let equal (n1 : t) (n2 : t) = n1.tag = n2.tag

    let hash (n : t) = n.hash
  end)

  let table = Table.create 101

  let new_tag =
    let x = ref ~-1 in
    fun () ->
      incr x ;
      !x

  let alloc_prim (hash : int) (prim : prim) (subterms : t list) =
    let res = { tag = new_tag (); hash; desc = Prim (prim, subterms) } in
    Table.add table res ;
    res

  let hash_empty_list = Hashtbl.hash []

  let hash_node_list (l : t list) : int =
    List.fold_left (fun h elt -> Hashtbl.hash (h, elt.hash)) hash_empty_list l

  let terms_equal (x : t) (y : t) = x.tag = y.tag

  let rec term_lists_equal (lx : t list) (ly : t list) =
    match (lx, ly) with
    | ([], _ :: _) | (_ :: _, []) -> false
    | ([], []) -> true
    | (hx :: tlx, hy :: tly) -> terms_equal hx hy && term_lists_equal tlx tly

  let prim (head : prim) (subterms : t list) =
    let subterms_hash = hash_node_list subterms in
    let head_hash = X.hash head in
    let hash = Hashtbl.hash (head_hash, subterms_hash) in
    match Table.find_all_by_hash table hash with
    | [] -> alloc_prim hash head subterms
    | bucket -> (
        let exists =
          List.find_opt
            (function
              | { desc = Prim (head', subterms'); _ } ->
                  X.compare head head' = 0
                  && term_lists_equal subterms subterms')
            bucket
        in
        match exists with
        | Some res -> res
        | None -> alloc_prim hash head subterms )
end
