local open Syntax in
  structure Operations : sig
    exception NotSubkind of Kind.t * Kind.t

    val eta : Tycon.atom -> Kind.t -> Tycon.t
    val subkind : Kind.t -> Kind.t -> Tycon.t

    (* Assume the given type constructor has the given kind. *)
    val self : Tycon.t -> Kind.t -> Kind.t
  end = struct
    exception NotSubkind of Kind.t * Kind.t

    fun eta a k =
      case Kind.proj k of
           Kind.Base        => Tycon.at a
         | Kind.Singleton b => Tycon.at b
         | Kind.DProd(x, y) =>
             let
               val fv = FVar.fresh ()
               val fv_exp = eta (Tycon.free fv) x
             in
               Tycon.abs fv (eta (Tycon.app a fv_exp) (Kind.open_by_atom 0 (Tycon.free fv) y))
             end
         | Kind.DSum(x, y) =>
             let
               val ty1 = eta (Tycon.fst a) x
               val ty2 = eta (Tycon.snd a) (Kind.open_by_atom 0 (Tycon.fst a) y)
             in
               Tycon.pair ty1 ty2
             end

    fun subkind x y =
      case (Kind.proj x, Kind.proj y) of
           (Kind.Base, Kind.Base)        => Tycon.at (Tycon.bound 0)
         | (Kind.Singleton a, Kind.Base) => Tycon.at a
         | (Kind.Singleton a, Kind.Singleton b) =>
             if a = b
             then Tycon.at b
             else raise NotSubkind(x, y)
         | (Kind.DProd(x1, y1), Kind.DProd(x2, y2)) =>
             let
               val c = subkind x2 x1
               val fv = FVar.fresh ()
               val c = Tycon.open_by_atom 0 (Tycon.free fv) c
               val d = subkind (Kind.open_ 0 c y1) (Kind.open_by_atom 0 (Tycon.free fv) y2)
               val z = FVar.fresh ()
             in
               Tycon.close 0 z (Tycon.abs fv (Tycon.open_by_atom 0 (Tycon.app (Tycon.free z) c) d))
             end
         | (Kind.DSum(x1, y1), Kind.DSum(x2, y2)) =>
             let
               val c = subkind x1 x2
               val fv = FVar.fresh ()
               val c' = Tycon.open_by_atom 0 (Tycon.free fv) c
               val d = subkind (Kind.open_by_atom 0 (Tycon.free fv) y1) (Kind.open_ 0 c' y2)
               val z = FVar.fresh ()

               val ty1 = Tycon.open_by_atom 0 (Tycon.fst (Tycon.free z)) c
               val ty2 = Tycon.subst_by_atom fv (Tycon.fst (Tycon.free z))
                           (Tycon.open_by_atom 0 (Tycon.snd (Tycon.free z)) d)
             in
               Tycon.close 0 z (Tycon.pair ty1 ty2)
             end
         | _ => raise NotSubkind(x, y)

    fun self ty k : Kind.t =
      case (Tycon.proj ty, Kind.proj k) of
           (Tycon.At a, Kind.Base)             => Kind.singleton a
         | (Tycon.At _, Kind.Singleton _)      => k
         | (Tycon.Abs ty', Kind.DProd(k1, k2)) =>
             let
               val fv = FVar.fresh ()
               val ty' = Tycon.open_by_atom 0 (Tycon.free fv) ty'
               val k2 = Kind.open_by_atom 0 (Tycon.free fv) k2
             in
               Kind.dprod fv k1 (self ty' k2)
             end
         | (Tycon.Pair(x, y), Kind.DSum(k1, k2)) =>
             Kind.product (self x k1) (self y (Kind.open_ 0 x k2))
         | _ => raise Fail "the type does not have the kind"
  end
end
