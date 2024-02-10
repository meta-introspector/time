module type Foo =
sig
  type t
end

module type Bar1 = sig
  module F: Foo
  val f1: F.t -> F.t
end

module type Bar2 = sig
  module F: Foo
  val f2: F.t -> F.t
end

module Ex (F: Foo) (B1: Bar1 with module F = F) (B2: Bar2 with module F = F) =
struct
  let f3 x = B2.f2 (B1.f1 x)

end

module Bar1_impl (F: Foo): Bar1 with module F = F = struct
  module F = F
  let f1 x = new OpenAIClientModule2.init
end

module Bar2_impl (F: Foo): Bar2 with module F = F = struct
  module F = F
  let f2 x = new ollama_lang_model
end

module F: Foo with type t = int = struct type t = int end
module F2: Foo with type t = int = struct type t = int end

module M = Ex(F)(Bar1_impl(F))(Bar2_impl(F))
module M2 = Ex(F2)(Bar1_impl(F2))(Bar2_impl(F2))
