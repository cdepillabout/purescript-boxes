module Test.Main where

import Prelude

import Effect (Effect)
import Text.PrettyPrint.Boxes (render, align, center1, text)
import Test.Spec (Spec, it)
import Test.Spec.Reporter as Test.Spec.Reporter
import Test.Spec.Runner as Test.Spec.Runner
import Test.Spec.Assertions (shouldEqual)
import Effect.Aff (launchAff_)

allTests :: Spec Unit
allTests = it "text" do
  render (align center1 center1 5 5 $ text "x") `shouldEqual` """

  x

"""

main :: Effect Unit
main = do
  launchAff_ $ Test.Spec.Runner.runSpec' Test.Spec.Runner.defaultConfig [ Test.Spec.Reporter.consoleReporter ] allTests
