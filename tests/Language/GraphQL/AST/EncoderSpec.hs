{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.AST.EncoderSpec
    ( spec
    ) where

import Language.GraphQL.AST
import Language.GraphQL.AST.Encoder
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
    describe "value" $ do
        context "null value" $ do
            let testNull formatter = value formatter Null `shouldBe` "null"
            it "minified" $ testNull minified
            it "pretty" $ testNull pretty

        context "minified" $ do
            it "escapes \\" $
                value minified (String "\\") `shouldBe` "\"\\\\\""
            it "escapes double quotes" $
                value minified (String "\"") `shouldBe` "\"\\\"\""
            it "escapes \\n" $
                value minified (String "\n") `shouldBe` "\"\\n\""
            it "escapes \\r" $
                value minified (String "\r") `shouldBe` "\"\\r\""
            it "escapes \\f" $
                value minified (String "\f") `shouldBe` "\"\\f\""
            it "escapes backspace" $
                value minified (String "a\bc") `shouldBe` "\"a\\bc\""
            context "escapes Unicode for chars less than 0010" $ do
                it "0000" $ value minified (String "\0") `shouldBe` "\"\\u0000\""
                it "0009" $ value minified (String "\0009") `shouldBe` "\"\\u0009\""
--            context "escapes Unicode for char less than 0020" $ do
--                it "0010" $ value minified (String "\0010") `shouldBe` "\"\\u0010\""
--                it "0019" $ value minified (String "\0019") `shouldBe` "\"\\u0019\""
--                it "0085" $ value minified (String "\0085") `shouldBe` "\"\n\""

        context "pretty" $ do
            it "uses strings for short string values" $
                value pretty (String "Short text") `shouldBe` "\"Short text\""
            it "uses block strings for text with new lines" $
                value pretty (String "Line 1\nLine 2")
                    `shouldBe` "\"\"\"\n  Line 1\n  Line 2\n\"\"\""
            it "escapes \\ in short strings" $
                value pretty (String "\\") `shouldBe` "\"\\\\\""

    describe "definition" $
        it "indents block strings in arguments" $
            let arguments = [Argument "message" (String "line1\nline2")]
                field = Field Nothing "field" arguments [] []
                operation = DefinitionOperation $ SelectionSet $ pure field
             in definition pretty operation `shouldBe` [r|{
  field(message: """
    line1
    line2
  """)
}
|]
