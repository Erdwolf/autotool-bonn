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


instance  Step PCProblem PCP Folge ( Select Int ) where
        step PCProblem pcp xs ( Pick x ) =
            xs ++ [fromIntegral x]

instance  Interactive PCProblem PCP Folge ( Select Int ) where
    interactive PCProblem = inter 

------------------------------------------------------------------------

inter :: PCP 
      -> IO (  Listener ( Select Int ) -> ( Component, Listener Folge ))
inter (PCP pcp) = do

    let top = ( mkLabel "leer" ) { ident = Ident "top" }
        mid = ( mkLabel "leer" ) { ident = Ident "mid" }
        bot = ( mkLabel "leer" ) { ident = Ident "bot" }

        topword m = do x <- m; fst (pcp !! fromIntegral (x-1))
        botword m = do x <- m; snd (pcp !! fromIntegral (x-1))

        set = splits
	    [ translate ( \ m -> [ Text $ topword m ] ) $ changeL top
	    , translate ( \ m -> [ Text $ show m    ] ) $ changeL mid
	    , translate ( \ m -> [ Text $ botword m ] ) $ changeL bot
	    ]

        kname k = Ident $ "P" ++ show k

        pairs listener = do
           (k, (l,r)) <- zip [1 ..] pcp
	   return $ ( mkButton ( l ++ "/" ++ r ) )
		    { ident = kname k
		    , action = emit (Pick k) $ listener 
		    }

    return $ \ listener -> ( above ( row $ pairs listener )
			           ( column [ top
					    , mid
					    , bot ] )
			   , set 
			   )













