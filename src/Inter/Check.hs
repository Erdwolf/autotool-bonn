-- this is a main module

import Inter.CGI

main :: IO ()
main = execute "Check.cgi" $ do
    a <- textfield "a" "0"
    b <- textarea "b" "0"
    submit "foo" "bar"
    pre $ "Summe ist " ++ show (read a + read b)

