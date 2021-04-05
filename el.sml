structure EL = struct
  type var = string

  datatype kind
    = Base
    | Singleton of tycon
    | DProd of var * kind * kind
    | DSum of var * kind * kind

  and tycon
    = Const of const
    | Var of var
    | Abs of var * kind * tycon
    | App of tycon * tycon
    | Pair of tycon * tycon
    | Proj of index * tycon
end
