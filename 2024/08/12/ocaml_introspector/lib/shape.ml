(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string
                  [@@deriving  yojson]
  include Identifiable.Make(struct
    type nonrec t = t
                      [@@deriving  yojson]
    let equal (x : t) y = x = y
    let compare (x : t) y = compare x y
    (* let hash (x : t) = Hashtbl.hash x *)

    let print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id } -> Format.fprintf fmt "%s.%d" comp_unit id

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      incr id;
      Item { comp_unit = current_unit; id = !id }

  let of_compilation_unit_id id =
    if not (Ident.persistent id) then
      Misc.fatal_errorf "Types.Uid.of_compilation_unit_id %S" (Ident.name id);
    Compilation_unit (Ident.name id)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type
                           [@@deriving  yojson]
  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Module
    | Module_type
    | Class
    | Class_type ->
        true
end

module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t
                               [@@deriving  yojson]
    let compare = compare

    let make str ns = str, ns

    let value id = Ident.name id, Sig_component_kind.Value
    let type_ id = Ident.name id, Sig_component_kind.Type
    let module_ id = Ident.name id, Sig_component_kind.Module
    let module_type id = Ident.name id, Sig_component_kind.Module_type
    let extension_constructor id =
      Ident.name id, Sig_component_kind.Extension_constructor
    let class_ id =
      Ident.name id, Sig_component_kind.Class
    let class_type id =
      Ident.name id, Sig_component_kind.Class_type

    let print fmt (name, ns) =
      Format.fprintf fmt "%S[%s]"
        name
        (Sig_component_kind.to_string ns)
  end

  include T

  module Map = Map.Make(T)
end

type var = Ident.t
                           [@@deriving  yojson]
type t = { uid: Uid.t option; desc: desc }
                           [@@deriving  yojson]
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Leaf
  | Proj of t * Item.t
  | Comp_unit of string
                           [@@deriving  yojson]

module Make_reduce(Params : sig
             type env
                                        [@@deriving  yojson]
  val fuel : int
  val read_unit_shape : unit_name:string -> t option
  val find_shape : env -> Ident.t -> t
end) = struct
  (* We implement a strong call-by-need reduction, following an
     evaluator from Nathanaelle Courant. *)

  type nf = { uid: Uid.t option; desc: nf_desc }
              [@@deriving  yojson]
  and nf_desc =
    | NVar of var
    | NApp of nf * nf
    (* | NAbs of local_env * var * t * delayed_nf *)
    (* | NStruct of delayed_nf Item.Map.t *)
    | NProj of nf * Item.t
    | NLeaf
    | NComp_unit of string
    | NoFuelLeft of desc
  [@@deriving  yojson]
  (* A type of normal forms for strong call-by-need evaluation.
     The normal form of an abstraction
       Abs(x, t)
     is a closure
       NAbs(env, x, t, dnf)
     when [env] is the local environment, and [dnf] is a delayed
     normal form of [t].

     A "delayed normal form" is morally equivalent to (nf Lazy.t), but
     we use a different representation that is compatible with
     memoization (lazy values are not hashable/comparable by default
     comparison functions): we represent a delayed normal form as
     just a not-yet-computed pair [local_env * t] of a term in a
     local environment -- we could also see this as a term under
     an explicit substitution. This delayed thunked is "forced"
     by calling the normalization function as usual, but duplicate
     computations are precisely avoided by memoization.
   *)
                             (* [@@deriving  yojson] *)
  (* and delayed_nf = Thunk of local_env * t *)
  (*                          [@@deriving  yojson] *)
  (* and local_env = delayed_nf option Ident.Map.t *)
  (* When reducing in the body of an abstraction [Abs(x, body)], we
     bind [x] to [None] in the environment. [Some v] is used for
     actual substitutions, for example in [App(Abs(x, body), t)], when
     [v] is a thunk that will evaluate to the normal form of [t]. *)
                           (* [@@deriving  yojson] *)

  type env = {
    fuel: int ref;
    global_env: Params.env;
    (* local_env: local_env; *)
    (* reduce_memo_table: (local_env * t, nf) Hashtbl.t; *)
    (* read_back_memo_table: (nf, t) Hashtbl.t; *)
  }
               [@@deriving  yojson]

end
