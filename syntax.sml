signature SYNTAX = sig
  structure FVar : sig
    type t

    val fresh : unit -> t
  end

  structure Tycon : sig
    eqtype t
    eqtype atom

    type fvar = FVar.t

    datatype u
      = At of atom
      | Abs of t
      | Pair of t * t

    val proj : t -> u

    val at : atom -> t
    val abs : fvar -> t -> t
    val pair : t -> t -> t

    val const : const -> atom
    val free : fvar -> atom
    val bound : int -> atom
    val app : atom -> t -> atom
    val fst : atom -> atom
    val snd : atom -> atom

    (* Assume the argument has the base kind. *)
    val get_atom : (atom, t) either -> atom

    val open_ : int -> t -> t -> t
    val open_atom : int -> t -> atom -> (atom, t) either

    val open_by_atom : int -> atom -> t -> t
    val open_atom_by_atom : int -> atom -> atom -> atom

    val close : int -> fvar -> t -> t
    val close_atom : int -> fvar -> atom -> atom

    val subst_by_atom : fvar -> atom -> t -> t
  end

  structure Kind : sig
    type t

    datatype u
      = Base
      | Singleton of Tycon.atom
      | DProd of t * t
      | DSum of t * t

    val proj : t -> u

    val base : t
    val singleton : Tycon.atom -> t
    val dprod : Tycon.fvar -> t -> t -> t
    val dsum : Tycon.fvar -> t -> t -> t

    (* Degenerate form of dependent *products*. *)
    val arrow : t -> t -> t
    (* Degenerate form of dependent *sums*. *)
    val product : t -> t -> t

    val open_ : int -> Tycon.t -> t -> t

    val open_by_atom : int -> Tycon.atom -> t -> t

    val close : int -> Tycon.fvar -> t -> t
  end
end

structure Syntax :> SYNTAX = struct
  structure FVar :> sig
    eqtype t

    val fresh : unit -> t
  end = struct
    type t = int

    val counter = ref 0

    fun fresh () =
    let
      val n = !counter
    in
      counter := n + 1;
      n
    end
  end

  structure Tycon = struct
    type fvar = FVar.t

    datatype t
      = At of atom
      | Abs of t
      | Pair of t * t

    and atom
      = Const of const
      | Bound of int
      | Free of fvar
      | App of atom * t
      | Proj of index * atom

    datatype u = datatype t

    fun proj x = x

    fun open_by_atom j a =
      fn At b       => At (open_atom_by_atom j a b)
       | Abs x      => Abs (open_by_atom (j + 1) a x)
       | Pair(x, y) => Pair(open_by_atom j a x, open_by_atom j a y)

    and open_atom_by_atom j a =
      fn Const c => Const c
       | Bound n =>
           if n = j
           then a
           else Bound n
       | Free fv    => Free fv
       | App(x, y)  => App(open_atom_by_atom j a x, open_by_atom j a y)
       | Proj(i, x) => Proj(i, open_atom_by_atom j a x)

    fun get_atom (Left a)       = a
      | get_atom (Right (At a)) = a
      | get_atom (Right _)      = raise Fail "not well-kinded"

    fun to_tycon (Left a)   = At a
      | to_tycon (Right ty) = ty

    fun open_ j (ty : t) =
      fn At a       => to_tycon (open_atom j ty a)
       | Abs x      => Abs (open_ (j + 1) ty x)
       | Pair(x, y) => Pair(open_ j ty x, open_ j ty y)

    and open_atom j (ty : t) =
      fn Const c => Left (Const c)
       | Bound n =>
           if n = j
           then Right ty
           else Left (Bound n)
       | Free fv   => Left (Free fv)
       | App(x, y) =>
           let
             val x' = open_atom j ty x
             val y' = open_ j ty y
           in
             case x' of
                  Left x''        => Left (App(x'', y'))
                | Right (Abs x'') => Right (open_ 0 y' x'')
                | _               => raise Fail "not well-kinded"
           end
       | Proj(i, x) =>
           case open_atom j ty x of
                Left x'        => Left (Proj(i, x'))
              | Right (Pair p) => Right (Index.index i p)
              | _              => raise Fail "not well-kinded"

    fun close j fv =
      fn At a       => At (close_atom j fv a)
       | Abs x      => Abs (close (j + 1) fv x)
       | Pair(x, y) => Pair(close j fv x, close j fv y)

    and close_atom j fv =
      fn Const c  => Const c
       | Bound n  => Bound n
       | Free fv' =>
           if fv = fv'
           then Bound j
           else Free fv'
       | App(x, y)  => App(close_atom j fv x, close j fv y)
       | Proj(i, x) => Proj(i, close_atom j fv x)

    fun subst_by_atom fv a ty = open_by_atom 0 a (close 0 fv ty)

    fun at a = At a
    fun abs fv x = Abs(close 0 fv x)
    fun pair x y = Pair(x, y)

    fun const c = Const c
    fun free fv = Free fv
    fun bound n = Bound n
    fun app x y = App(x, y)
    fun fst x = Proj(Fst, x)
    fun snd x = Proj(Snd, x)
  end

  structure Kind = struct
    datatype t
      = Base
      | Singleton of Tycon.atom
      | DProd of t * t
      | DSum of t * t

    datatype u = datatype t

    fun proj x = x

    fun open_by_atom j a =
      fn Base        => Base
       | Singleton b => Singleton (Tycon.open_atom_by_atom j a b)
       | DProd(x, y) => DProd(open_by_atom j a x, open_by_atom (j + 1) a y)
       | DSum(x, y)  => DSum(open_by_atom j a x, open_by_atom (j + 1) a y)

    fun open_ j ty =
      fn Base        => Base
       | Singleton a => Singleton (Tycon.get_atom (Tycon.open_atom j ty a))
       | DProd(x, y) => DProd(open_ j ty x, open_ (j + 1) ty y)
       | DSum(x, y)  => DSum(open_ j ty x, open_ (j + 1) ty y)

    fun close j fv =
      fn Base        => Base
       | Singleton a => Singleton (Tycon.close_atom j fv a)
       | DProd(x, y) => DProd(close j fv x, close (j + 1) fv y)
       | DSum(x, y)  => DSum(close j fv x, close (j + 1) fv y)

    val base = Base
    fun singleton a = Singleton a
    fun dprod fv x y = DProd(x, close 0 fv y)
    fun dsum fv x y = DSum(x, close 0 fv y)

    fun arrow x y = DProd(x, y)
    fun product x y = DSum(x, y)
  end
end
