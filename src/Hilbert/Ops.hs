module Hilbert.Ops

( implies, equiv
, und, oder
, nicht


, assign
, einsetz
, mopo

, reserved
, ops
)

where

import Ids
import IdStack

assign = mkid ":=" (Passive ":=") (Just 2) Op Op (Just 5) Nn


implies = mkid "->" (Passive "\\rightarrow") (Just 2) Op Op (Just 10) Nn
equiv = mkid "<->" (Passive "\\leftrightarrow") (Just 2) Op Op (Just 15) Nn

oder = mkid "|"  (Passive "\\vee ")    (Just 2) Op Op (Just 20) Nn
und  = mkid "&" (Passive "\\wedge")    (Just 2) Op Op (Just 25) Nn

nicht = mkid "not" (Passive "\\neg"     ) (Just 1) Fn Fn Nothing Nn

einsetz = mkid "sub" (Passive "\\mathsf{sub}"     ) (Just 2) Fn Fn Nothing Nn
mopo = mkid "mopo" (Passive "\\mathsf{mp}"     ) (Just 2) Fn Fn Nothing Nn

reserved =   [ implies, equiv
	     , und, oder
	     , nicht
	     , assign
	     , einsetz, mopo
	     ]


ops = globIS . inIts $ reserved

