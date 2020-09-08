module type S = sig
  type t

  val compare : t -> t -> int

  val hash : t -> int

  val pp : Format.formatter -> t -> unit
end
