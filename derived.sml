(* Derived EL operations in terms of the translation into IL. *)

structure Derived : sig
  exception NotIdentical of Syntax.Tycon.t * Syntax.Tycon.t

  (*
  * `equiv env x y k`
  * Since the translation essentialy performs kindchecking,
  * you don't need to concern whether `x` and `y` are well-kinded or not.
  * Similarly, you don't need to concern about well-formedness of `k`.
  * *)
  val equiv : env -> EL.tycon -> EL.tycon -> EL.kind -> unit
end = struct
  exception NotIdentical of Syntax.Tycon.t * Syntax.Tycon.t

  fun equiv env x y k =
  let
    val k1 = Translation.tycon env x
    val k2 = Translation.tycon env y
    val k = Translation.kind env k

    val c = Operations.subkind k1 k
    val d = Operations.subkind k2 k
  in
    if c = d
    then ()
    else raise NotIdentical(c, d)
  end
end
