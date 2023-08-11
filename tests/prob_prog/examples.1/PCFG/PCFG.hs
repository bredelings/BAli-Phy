module PCFG where

import Probability

data Rule = Word String | PreTerminal String | Rule String

    
rules = [("start",[(1.0, [Rule "NP", Rule "VP"])]),
          ("VP",   [(0.4, [PreTerminal "V", Rule "NP"]),
                    (0.3, [PreTerminal "V"]),
                    (0.3, [Word "is", PreTerminal "A"])
                   ]),
          ("NP",   [(0.4, [PreTerminal "A", Rule "NP"]),
                    (0.6, [PreTerminal "N"])])
         ]


preterminals = [("N",[("John",0.6), ("soup",0.4)]),
                ("V",[("loves",0.3),("hates",0.3),("runs",0.4)]),
                ("A",[("tall",0.6),("salty",0.4)])]
    

pcfg w@(Word s)        = return [w]

pcfg (PreTerminal s) = case lookup s preterminals of
                         Nothing -> error $ "no word class '"++s++"'"
                         Just words -> do w <- sample $ categorical_on words
                                          return [Word w]

pcfg (Rule s)        = case lookup s rules of
                         Nothing -> error $ "no rule '"++s++"'"
                         Just possibilities-> do let probs = map fst possibilities
                                                     rules = map snd possibilities
                                                 i <- sample $ categorical probs
                                                 let next = rules!!i
                                                 groups <- mapM pcfg next
                                                 return (concat groups)

model = do
  words <- pcfg (Rule "start")
  let sentence = map (\w -> case w of (Word s) -> s) words
  return ["sentence" %=% sentence]

main = return model

-- question: if the sentence starts with "tall John", what's next?
