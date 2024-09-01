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
  end)
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
end
  
module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t
                        [@@deriving  yojson]
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
             end) = struct
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
    
    type env = {
        fuel: int ref;
        global_env: Params.env;
        (* local_env: local_env; *)
        (* reduce_memo_table: (local_env * t, nf) Hashtbl.t; *)
        (* read_back_memo_table: (nf, t) Hashtbl.t; *)
      }
                 [@@deriving  yojson]
    
  end
