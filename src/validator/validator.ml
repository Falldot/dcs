(* Copyright (c) 2022 Falldot. All rights reserved. *)

type ('a, 'b) t = 'a * 'b

let make a b : ('a, 'b) t = (a, b)
let map_left f t = (f (fst t), snd t)
let map_right f t = (fst t, f (snd t))
let map f g t = (f (fst t), g (snd t))
let fold f t = f (fst t) (snd t)
let chain (f : 'a -> 'b -> 'd * 'c) t = f (fst t) (snd t)
