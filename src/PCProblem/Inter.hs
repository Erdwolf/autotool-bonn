module PCProblem.Inter where

import PCProblem.Type
import PCProblem.Verify

import Step
import Interactive.Type
import Component hiding ( join )
import Reporter
import ToDoc
import List (isPrefixOf)

-----------------------------------------------------------------------

instance Partial PCProblem PCP Folge where

    initial PCProblem pcp = 
        []
    partial PCProblem pcp lsg = do
        let (l,r) = lr pcp lsg
        assert ( isPrefixOf l r || isPrefixOf r l )
	    $ text "Ist das eine Wort ein Präfix des anderen?"
    total PCProblem pcp lsg = do
        let (l, r) = lr pcp lsg
	    c = common l r
	    [cl, cr] = map (drop (length c)) [ l, r ]
	when ( not $ null cl ) $ reject 
	     $ text "Oberer Vorsprung:" <+> text cl
	when ( not $ null cr ) $ reject 
	     $ text "Unterer Vorsprung:" <+> text cr
	when ( null lsg ) $ reject
	     $ text "Das Lösungswort darf nicht leer sein."


instance  Step PCProblem PCP Folge ( Select Int ) where
        step PCProblem pcp xs ( Pick x ) =
            xs ++ [fromIntegral x]

instance  Interactive PCProblem PCP Folge ( Select Int ) where
    interactive PCProblem = inter 

------------------------------------------------------------------------

inter :: PCP 
      -> IO (  Listener ( Select Int ) -> ( Component, Listener Folge ))
inter (PCP pcp) = do

    let 
	top0 = ( mkLabel "oberes Wort:" ) { ident = Ident "top0" }
        top = ( mkLabel "leer" ) { ident = Ident "top" }
	bot0 = ( mkLabel "unteres Wort:" ) { ident = Ident "bot0" }
        bot = ( mkLabel "leer" ) { ident = Ident "bot" }

        topword m = do x <- m; fst (pcp !! fromIntegral (x-1))
        botword m = do x <- m; snd (pcp !! fromIntegral (x-1))

        set = splits
	    [ translate ( \ m -> [ Text $ topword m ] ) $ changeL top
	    , translate ( \ m -> [ Text $ botword m ] ) $ changeL bot
	    ]

        kname k = Ident $ "P" ++ show k

	pairs0 = ( mkLabel "Wählen Sie:" ) { ident = Ident "pairs0" }
        pairs listener = do
           (k, (l,r)) <- zip [1 ..] pcp
	   return $ ( mkButton ( l ++ "/" ++ r ) )
		    { ident = kname k
		    , action = emit (Pick k) $ listener 
		    }

    return $ \ listener -> ( grid [ pairs0 : pairs listener 
			          , [ top0, top ]
				  , [ bot0, bot ] 
				  ]
			   , set 
			   )













