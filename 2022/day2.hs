import System.IO  
import System.Environment
import Control.Monad

data RPS = Rock | Paper | Scisors deriving Show
data Result = Lose | Tie | Win deriving Show

main = let filedata = readFile "day2.input"
       in
         do
           print . matchup . setup . map convertInput . words =<< filedata
           print . matchup . translate . setup . words =<< filedata

convertInput "X" = Rock
convertInput "Y" = Paper
convertInput "Z" = Scisors
convertInput "A" = Rock
convertInput "B" = Paper
convertInput "C" = Scisors

convertOutcome "X" = Lose
convertOutcome "Y" = Tie
convertOutcome "Z" = Win

value Rock = 1
value Paper = 2
value Scisors = 3

score Rock Paper = 6
score Rock Rock = 3
score Rock Scisors = 0
score Paper Paper = 3
score Paper Rock = 0
score Paper Scisors = 6
score Scisors Paper = 0
score Scisors Rock = 6
score Scisors Scisors = 3

setup (x:y:xs) = (x,y) : setup xs
setup [] = []

matchup ((x,y) : xs) = score x y + value y + matchup xs
matchup [] = 0

translate ((x,y) : xs) = convert (convertInput x) (convertOutcome y) : translate xs
translate [] = []

convert x Tie = (x, x)
convert Rock Lose = (Rock, Scisors)
convert Rock Win = (Rock, Paper)
convert Paper Lose = (Paper, Rock)
convert Paper Win = (Paper, Scisors)
convert Scisors Lose = (Scisors, Paper)
convert Scisors Win = (Scisors, Rock)
