#! /bin/bash
rm -rf out
runhaskell "-i gen" gen/Gen.hs src/Types/Basic.hs src/Types/Config.hs src/Types/Description.hs src/Types/Documented.hs src/Types/Instance.hs src/Types/ServerInfo.hs src/Types/Signed.hs src/Types/Solution.hs src/Types/TaskTree.hs src/Types/Version.hs
