{-# LANGUAGE FlexibleContexts #-}

module Util
    ( shouldParse
    , shouldNotParse
    )
where

import           Data.Either
import           Test.Hspec
import           Text.Parsec

shouldParse p l s a =
    it ("parses " ++ l ++ " without errors")
        $ (parse p "" s `shouldSatisfy` isRight)

shouldNotParse p l s =
    it ("parses " ++ l ++ " with errors")
        $ (parse p "" s `shouldSatisfy` isLeft)
