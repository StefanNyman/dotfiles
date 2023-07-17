module Keyboard
  ( swapLayout,
    Layout (..),
    setLayout,
  )
where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Text.Read
import           XMonad          hiding (Layout, setLayout)
import           XMonad.Util.Run

iif' :: Bool -> a -> a -> a
iif' c a b
  | c = a
  | otherwise = b

isMember :: Eq a => a -> [a] -> Bool
isMember _ [] = False
isMember n (x : xs)
  | n == x = True
  | otherwise = isMember n xs

data DList a = DLNode (DList a) a (DList a)

mkDList :: Eq a => [a] -> DList a
mkDList [] = error "must have at least one element"
mkDList xs =
  let (first, last) = go last (nub xs) first
   in first
  where
    go :: DList a -> [a] -> DList a -> (DList a, DList a)
    go prev [] next = (next, prev)
    go prev (x : xs) next =
      let this = DLNode prev x rest
          (rest, last) = go this xs next
       in (this, last)

getDLVal :: DList a -> a
getDLVal (DLNode _ x _) = x

takeDLl :: Integer -> DList a -> [a]
takeDLl 0 _                 = []
takeDLl n (DLNode _ x next) = x : takeDLl (n -1) next

nextDL :: Layout -> [Layout] -> [Layout] -> DList Layout -> Maybe Layout
nextDL cl _ checked _
  | isMember cl checked = Nothing
nextDL cl layouts _ _
  | not $ isMember cl layouts = Nothing
nextDL current layouts checked (DLNode _ x n) =
  iif'
    (x == current)
    (Just $ getDLVal n)
    (nextDL current layouts (checked ++ [x]) n)

nextLayout :: Layout -> [Layout] -> Maybe Layout
nextLayout cl [] = Nothing
nextLayout cl l
  | not (isMember cl l) = Just $ head l
  | otherwise =
    let lst = mkDList l
        first = head $ takeDLl 1 lst
     in nextDL cl l [] lst

getCurrentLayout :: IO Layout
getCurrentLayout = do
  out <- runProcessWithInput "/usr/bin/setxkbmap" ["-query"] ""
  let stripped = map (dropWhile (== ' ') . drop 1 . dropWhile (/= ':')) $ lines out
  pure $ fromMaybe US (readMaybe (map toUpper (stripped !! 2)) :: Maybe Layout)

swapLayout' :: Layout -> [Layout] -> X ()
swapLayout' d layouts = do
  curr <- liftIO getCurrentLayout
  setLayout $ fromMaybe d (nextLayout curr layouts)

swapLayout :: [Layout] -> X ()
swapLayout = swapLayout' US

setLayout :: Layout -> X ()
setLayout l = do
  spawn $ "setxkbmap " ++ map toLower (show l)

data Layout
  = AF
  | AL
  | AM
  | ARA
  | AT
  | AU
  | AZ
  | BA
  | BD
  | BE
  | BG
  | BR
  | BT
  | BW
  | BY
  | CA
  | CD
  | CH
  | CM
  | CN
  | CZ
  | DE
  | DK
  | DZ
  | EE
  | EPO
  | ES
  | ET
  | FI
  | FO
  | FR
  | GB
  | GE
  | GH
  | GN
  | GR
  | HR
  | HU
  | ID
  | IE
  | IL
  | IN
  | IQ
  | IR
  | IS
  | IT
  | JP
  | JV
  | KE
  | KG
  | KH
  | KR
  | KZ
  | LA
  | LATAM
  | LK
  | LT
  | LV
  | MA
  | MAO
  | MD
  | ME
  | MK
  | ML
  | MM
  | MN
  | MT
  | MV
  | MY
  | NG
  | NL
  | NO
  | NP
  | PH
  | PK
  | PL
  | PT
  | RO
  | RS
  | RU
  | SE
  | SI
  | SK
  | SN
  | SY
  | TG
  | TH
  | TJ
  | TM
  | TR
  | TW
  | TZ
  | UA
  | US
  | UZ
  | VN
  | ZA
  deriving (Show, Read, Eq)
