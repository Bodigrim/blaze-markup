-- | This is a module which runs the 'HtmlBenchmarks' module using the different
-- renderers available.
--
module Main where

import Test.Tasty.Bench
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB

import qualified Text.Blaze.Renderer.Utf8 as Utf8
import qualified Text.Blaze.Renderer.String as String
import qualified Text.Blaze.Renderer.Text as Text

import HtmlBenchmarks (HtmlBenchmark (..), benchmarks)

-- | Function to run the benchmarks using criterion
--
main :: IO ()
main = defaultMain $ map benchHtml benchmarks
  where
    benchHtml (HtmlBenchmark name f x _) =
      bench name $ nf (LT.length . Text.renderMarkup . f) x
