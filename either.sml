structure Either :> sig
  datatype ('a, 'b) either
    = Left of 'a
    | Right of 'b
end = struct
  datatype ('a, 'b) either
    = Left of 'a
    | Right of 'b
end

datatype either = datatype Either.either
