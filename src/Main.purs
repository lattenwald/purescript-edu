module Main where

import Prelude hiding (div)

import Data.Array
import Data.Foldable (traverse_)
import Data.Int
import Data.Traversable (traverse)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random

import Text.Smolder.HTML (html, head, meta, link, title, body, h1, p, div, tr, td, table)
import Text.Smolder.HTML.Attributes (lang, charset, httpEquiv, content, name, rel, href, className)
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup (text, (!), MarkupM)
import Text.Smolder.Renderer.String (render)

import AsBody

data ProblemDivMod = ProblemDivMod
  { dividend  :: Int
  , divider   :: Int
  }

data Query a = Noop a

instance showProblemDivMod :: Show ProblemDivMod where
  show (ProblemDivMod p) = show p.dividend ++ " / "
                        ++ show p.divider ++ " = "
                        ++ show (p.dividend / p.divider) ++ " + "
                        ++ show (p.dividend `mod` p.divider) ++ " / "
                        ++ show p.divider

genProblemsDivMod n = traverse genProblem (0 .. n)
  where
    genProblem _ = do
      a <- randomInt 2 10
      b <- randomInt 2 10
      c <- randomInt 0 (b - 1)
      return $ ProblemDivMod { dividend  : a*b+c
                             , divider   : b }

renderProblems :: Array ProblemDivMod -> String
renderProblems problems = render $ div ! A.id "content" $ table $
  traverse_ renderRow problemRows
  where
    problemRows = zipWith (\a b -> [a, b]) part1 part2
      where
        len = (length problems)/2
        part1 = take len problems
        part2 = drop len problems

    renderRow probs = tr $ traverse_ renderCell probs

    renderCell (ProblemDivMod prob) = td $ do
      text $ show prob.dividend
      text " &divide; "
      text $ show prob.divider
      text " = "

main = do
  problems <- genProblemsDivMod 50
  asBody $ renderProblems problems
