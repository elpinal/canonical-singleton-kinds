structure Translation : sig
  exception NotSingleton of Syntax.Kind.t
  exception NotDProd of Syntax.Kind.t
  exception NotDSum of Syntax.Kind.t

  (* Returns natural, simple kinds. *)
  val const : const -> Syntax.Kind.t

  val kind : env -> EL.kind -> Syntax.Kind.t
  val tycon : env -> EL.tycon -> Syntax.Kind.t
end = struct
  open Syntax

  exception NotSingleton of Kind.t
  exception NotDProd of Kind.t
  exception NotDSum of Kind.t

  local open Kind in
    val const =
      fn Bool  => base
       | Int   => base
       | List  => arrow base base
       | Arrow => arrow base (arrow base base)
  end

  fun kind env =
    fn EL.Base => Kind.base
     | EL.Singleton ty =>
         let val k = tycon env ty in
           case Kind.proj k of
                Kind.Singleton _ => k
              | _                => raise NotSingleton k
         end
     | EL.DProd(v, x, y) =>
         let
           val k1 = kind env x
           val fv = FVar.fresh ()
           val k2 = kind (Env.insert v fv k1 env) y
         in
           Kind.dprod fv k1 k2
         end
     | EL.DSum(v, x, y) =>
         let
           val k1 = kind env x
           val fv = FVar.fresh ()
           val k2 = kind (Env.insert v fv k1 env) y
         in
           Kind.dsum fv k1 k2
         end

  and tycon env =
    fn EL.Const c =>
         let val k = const c in
           Operations.self (Operations.eta (Tycon.const c) k) k
         end
     | EL.Var v =>
         let val (fv, k) = Env.lookup env v in
           Operations.self (Operations.eta (Tycon.free fv) k) k
         end
     | EL.Abs(v, k, x) =>
         let
           val k1 = kind env k
           val fv = FVar.fresh ()
           val k2 = tycon (Env.insert v fv k1 env) x
         in
           Kind.dprod fv k1 k2
         end
     | EL.App(x, y) =>
         let
           val k1 = tycon env x
           val k2 = tycon env y
         in
           case Kind.proj k1 of
                Kind.DProd(k11, k12) =>
                  let
                    (* Since `k2` is transparent, `c` is locally closed. *)
                    val c = Operations.subkind k2 k11
                  in
                    Kind.open_ 0 c k12
                  end
              | _ => raise NotDProd k1
         end
     | EL.Pair(x, y) => Kind.product (tycon env x) (tycon env y)
     | EL.Proj(i, x) =>
         let
           val k = tycon env x
         in
           case Kind.proj k of
                Kind.DSum(k1, k2) => (* Since `k` is transparent, `k2` is locally closed. *)
                  Index.index i (k1, k2)
              | _ => raise NotDSum k
         end
end
