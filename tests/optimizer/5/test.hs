module Test where {
f w = case w of {x:xs -> case w of {y:ys -> ys}; [] -> []};
}
