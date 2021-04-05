datatype const
  = Bool
  | Int
  | List
  | Arrow

structure Index = struct
  datatype index
    = Fst
    | Snd

  fun index Fst (x, _) = x
    | index Snd (_, y) = y
end

datatype index = datatype Index.index
