--------------------------------------------------------

eval :: FiniteMap Id Bool -> Exp -> Bool

eval fm (App fun []) | fun == true = True
eval fm (App fun []) | fun == false = False

eval fm (App fun []) =
    case lookupFM fm fun of
	 Just v -> v
	 Nothing -> error $ "eval: not bound: " ++ show fun

eval fm (App fun [arg ]) | fun == nicht = not (eval fm arg)

eval fm e @ (App fun [x, y]) =
    let vx = eval fm x; vy = eval fm y
    in	if fun == und then vx && vy
	else if fun == oder then vx || vy
	else if fun == implies then vx <= vy
	else if fun == equiv then vx == vy
	else error $ "eval: strange expn: " ++ show e

eval fm e = error $ "eval: strange expn: " ++ show e

