import IO
import PCProblem.Generator
import PCProblem.Param

main = sequence_ $ repeat $ do
    pf <- generator g
    print pf
    hFlush stdout

