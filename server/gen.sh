#! /bin/bash
rm -rf out
runhaskell "-i gen" gen/Gen.hs src/Types/*.hs
