structure Test : sig
end = struct
  open EL
  fun equiv x y k = Derived.equiv Env.empty x y k
    handle
        Fail s => print ("Fail: " ^ s ^ "\n")
      | e      => print (exnMessage e ^ "\n")

  val () = equiv (Const Bool) (Const Bool) Base

  val () = equiv (Const Int) (Const Bool) Base

  val () = equiv (Const Int) (Const Bool) (Singleton (Const Int))

  val () = equiv (Const Int) (App(Abs("a", Base, Var "a"), Const Int)) (Singleton (Const Int))

  val () = equiv (Const Int) (App(Abs("a", Base, Var "a"), Const Bool)) (Singleton (Const Int))

  val () = equiv (Const Int) (App(Abs("a", Base, Var "a"), Const Int)) Base

  val () = equiv (Abs("a", Base, Var "a")) (Abs("b", Base, Var "b")) (DProd("c", Base, Base))

  val () = equiv (Abs("a", Base, Var "a")) (Abs("b", Base, Const Int)) (DProd("c", Base, Base))

  val () = equiv (Abs("a", Base, Var "a")) (Abs("b", Base, Const Int)) (DProd("c", Singleton (Const Int), Base))

  val () = equiv (Abs("a", Base, Var "a")) (Abs("b", Base, Const Int)) (DProd("c", Singleton (Const Bool), Base))

  val () = equiv (Abs("a", Singleton (Const Bool), Var "a")) (Abs("b", Singleton (Const Bool), Const Bool)) (DProd("c", Base, Base))
end
