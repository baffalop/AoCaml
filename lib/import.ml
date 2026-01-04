include Let
include Fn

module Set = struct
  include Set

  module type Showable = sig
    include Set.S

    val pp : Format.formatter -> t -> unit
  end

  (** Set.Make but the resulting module is usable with [@@deriving show] *)
  let make_showable
    (type a)
    (module Ord: OrderedType with type t = a)
    (fmt_elt : a Fmt.t)
    : (module Showable with type elt = a)
    = (module struct
    include Make(Ord)

    let pp fmt =
      Format.fprintf fmt "{%s}"
      << Fmt.(str "%a" (list ~sep:semi fmt_elt))
      << elements
  end)
end
