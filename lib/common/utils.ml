let rec until pred f x =
  if pred x then x else until pred f @@ f x

let always_true _ _ = true

let uncurry3 f (x, y, z) = f x y z

let find_first_index pred =
  Iter.find_mapi @@ fun index elem -> if pred elem then Some index else None