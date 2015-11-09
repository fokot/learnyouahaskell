type Rope = (Int, Int)

landRight b (l,r) = if abs (r + b - l) >= 4 then Nothing else Just (l, r + b)
landLeft b (l,r) = if abs (l + b - r) >= 4 then Nothing else Just (l + b, r)
