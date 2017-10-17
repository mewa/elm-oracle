module Helper exposing (..)

import Test exposing (..)
import Expect


testEq : String -> a -> a -> Test
testEq name a a_ =
    test name <|
        \() -> Expect.equal a a_
