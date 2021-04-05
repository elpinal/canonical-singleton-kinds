structure Env :> sig
  type t

  exception Unbound of EL.var

  val empty : t

  val insert : EL.var -> Syntax.FVar.t -> Syntax.Kind.t -> t -> t
  val lookup : t -> EL.var -> Syntax.FVar.t * Syntax.Kind.t
end = struct
  structure M = Map (type t = EL.var val compare = String.compare)
  type t = (Syntax.FVar.t * Syntax.Kind.t) M.t

  exception Unbound of EL.var

  val empty = M.empty

  fun insert v fv k m = M.insert v (fv, k) m

  fun lookup m v =
    case M.lookup v m of
         SOME x => x
       | NONE   => raise Unbound v
end

type env = Env.t
