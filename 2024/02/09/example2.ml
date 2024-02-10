
(*
  description of the binding
  we expect type t to be something like an openai_like_lang_model
 *)
module type BINDING_INFO = sig
  type 'a t
end

module type S = sig
  module Binding : sig
    type 'a info
    val init : 'a -> 'a 
  end
end

module Make (Binding_info : BINDING_INFO) : S
with type 'a Binding.info = 'a Binding_info.t = struct
  module Binding = struct
    type 'a info = 'a Binding_info.t
    let init x = x
  end
end
include Make (struct type 'a t = unit end)


module OllamaAgain = Hmap.Make (struct type 'a t = 'a ollama_lang_model end)
