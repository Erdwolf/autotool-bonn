-- -*- mode: haskell -*-

{-# LANGUAGE PackageImports #-}

module Local (
    debug,
    cgi_name
) where

import "autotool-collection" Local (debug)

cgi_name :: String
cgi_name = "Super.cgi"
