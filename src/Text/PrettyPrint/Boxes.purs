module Text.PrettyPrint.Boxes
  ( -- * Constructing boxes
    Box
  , nullBox
  , emptyBox
  , char
  , text
  , para
  , columns

    -- * Layout of boxes

  , appendHorizTop
  , appendHorizTopExtraCol
  , (<<>>)
  , (<<+>>)
  , hcat
  , hsep

  , appendVertLeft
  , appendVertLeftExtraCol
  , (//)
  , (/+/)
  , vcat
  , vsep

  , punctuateH, punctuateV

  -- * Alignment

  , Alignment

  , left, right
  , top, bottom
  , center1, center2

  , moveLeft
  , moveRight
  , moveUp
  , moveDown

  , alignHoriz
  , alignVert
  , align

  -- * Inspecting boxes

  , rows
  , cols

  -- * Rendering boxes

  , render

  ) where

import Prelude hiding (bottom, top)

import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr, sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong (first, (***))
import Data.String (joinWith, length, take) as String
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray) as String
import Data.String.Utils (words)
import Data.Tuple (Tuple(..), uncurry)

-- | The basic data type.  A box has a specified size and some sort of
-- | contents.
newtype Box = Box { rows    :: Int
                  , cols    :: Int
                  , content :: Content
                  }

derive instance genericBox :: Generic Box _
derive instance eqBox :: Eq Box
derive instance newtypeBox :: Newtype Box _
instance showBox :: Show Box where
  show (Box b) = "Box " <> show b

cols :: Box -> Int
cols = _.cols <<< unwrap

rows :: Box -> Int
rows = _.rows <<< unwrap

createBox :: Int -> Int -> Content -> Box
createBox r c content = Box { rows: r, cols: c, content }

-- | Data type for specifying the alignment of boxes.
data Alignment = AlignFirst    -- ^ Align at the top/left.
               | AlignCenter1  -- ^ Centered, biased to the top/left.
               | AlignCenter2  -- ^ Centered, biased to the bottom/right.
               | AlignLast     -- ^ Align at the bottom/right.

derive instance genericAlignment :: Generic Alignment _
derive instance eqAlignment :: Eq Alignment
instance showAlignment :: Show Alignment where
  show = genericShow

-- | Align boxes along their tops.
top :: Alignment
top        = AlignFirst

-- | Align boxes along their bottoms.
bottom :: Alignment
bottom     = AlignLast

-- | Align boxes to the left.
left :: Alignment
left       = AlignFirst

-- | Align boxes to the right.
right :: Alignment
right      = AlignLast

-- | Align boxes centered, but biased to the left/top in case of
--   unequal parities.
center1 :: Alignment
center1    = AlignCenter1

-- | Align boxes centered, but biased to the right/bottom in case of
--   unequal parities.
center2 :: Alignment
center2   = AlignCenter2

-- | Contents of a box.
data Content = Blank             -- ^ No content.
             | Text String       -- ^ A raw string.
             | Row (Array Box)   -- ^ A row of sub-boxes.
             | Col (Array Box)   -- ^ A column of sub-boxes.
             | SubBox Alignment Alignment Box
                                 -- ^ A sub-box with a specified alignment.

derive instance genericContent :: Generic Content _
derive instance eqContent :: Eq Content
instance showContent :: Show Content where
  show = genericShow

-- | The null box, which has no content and no size.  It is quite
-- | useless.
nullBox :: Box
nullBox = emptyBox 0 0

-- | `emptyBox r c` is an empty box with `r` rows and `c` columns.
-- | Useful for effecting more fine-grained positioning of other
-- | boxes, by inserting empty boxes of the desired size in between
-- | them.
emptyBox :: Int -> Int -> Box
emptyBox r c = createBox r c Blank

-- | A `1x1` box containing a single character.
char :: Char -> Box
char c = createBox 1 1 (Text $ String.singleton c)

-- | A (`1 x len`) box containing a string of length `len`.
text :: String -> Box
text t = createBox 1 (String.length t) (Text t)

-- | Paste two boxes together horizontally, using a default (top)
-- | alignment.
appendHorizTop :: Box -> Box -> Box
appendHorizTop l r = hcat top [ l, r ]

infixl 7 appendHorizTop as <<>>

-- | Paste two boxes together horizontally with a single intervening
-- | column of space, using a default (top) alignment.
appendHorizTopExtraCol :: Box -> Box -> Box
appendHorizTopExtraCol l r = hcat top [l, emptyBox 0 1, r]

infixl 7 appendHorizTopExtraCol as <<+>>

-- | Paste two boxes together vertically, using a default (left)
-- | alignment.
appendVertLeft :: Box -> Box -> Box
appendVertLeft t b = vcat left [ t, b ]

infixl 7 appendVertLeft as //

-- | Paste two boxes together vertically with a single intervening row
-- | of space, using a default (left) alignment.
appendVertLeftExtraCol :: Box -> Box -> Box
appendVertLeftExtraCol t b = vcat left [ t, emptyBox 1 0, b ]

infixl 7 appendVertLeftExtraCol as /+/

-- | Glue a list of boxes together horizontally, with the given alignment.
hcat :: forall f. Foldable f => Alignment -> f Box -> Box
hcat a bs = createBox h w (Row $ map (alignVert a h) bsl)
  where
    bsl = Array.fromFoldable bs
    Tuple w h = sumMax cols 0 rows bsl

-- | `hsep sep a bs` lays out `bs` horizontally with alignment `a`,
-- | with `sep` amount of space in between each.
hsep :: forall f. Foldable f => Int -> Alignment -> f Box -> Box
hsep sep a bs = punctuateH a (emptyBox 0 sep) bs

-- | Glue a list of boxes together vertically, with the given alignment.
vcat :: forall f. Foldable f => Alignment -> f Box -> Box
vcat a bs = createBox h w (Col $ map (alignHoriz a w) bsl)
  where
    bsl = Array.fromFoldable bs
    Tuple h w = sumMax rows 0 cols bsl

-- Calculate a sum and a maximum over an Foldable of boxes in one pass. If the
-- list is empty, the maximum is reported as the given default.
sumMax
  :: forall f
   . Foldable f
  => (Box -> Int)
  -> Int
  -> (Box -> Int)
  -> f Box
  -> Tuple Int Int
sumMax f defaultMax g as = foldr go (Tuple 0 defaultMax) as
  where
    go a (Tuple n b) = Tuple (f a + n) (g a `max` b)

-- | `vsep sep a bs` lays out `bs` vertically with alignment `a`,
-- | with `sep` amount of space in between each.
vsep :: forall f. Foldable f => Int -> Alignment -> f Box -> Box
vsep sep a bs = punctuateV a (emptyBox sep 0) bs

-- | `punctuateH a p bs` horizontally lays out the boxes `bs` with a
-- | copy of `p` interspersed between each.
punctuateH :: forall f. Foldable f => Alignment -> Box -> f Box -> Box
punctuateH a p bs = hcat a (intersperse p (Array.fromFoldable bs))

-- | A vertical version of 'punctuateH'.
punctuateV :: forall f. Foldable f => Alignment -> Box -> f Box -> Box
punctuateV a p bs = vcat a (intersperse p (Array.fromFoldable bs))

--------------------------------------------------------------------------------
--  Paragraph flowing  ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | `para algn w t` is a box of width `w`, containing text `t`,
-- | aligned according to `algn`, flowed to fit within the given
-- | width.
para :: Alignment -> Int -> String -> Box
para a n t = (\ss -> mkParaBox a (Array.length ss) ss) $ flow n t

-- | `columns w h t` is a list of boxes, each of width `w` and height
-- | at most `h`, containing text `t` flowed into as many columns as
-- | necessary.
columns :: Alignment -> Int -> Int -> String -> Array Box
columns a w h t = map (mkParaBox a h) <<< chunksOf h $ flow w t

-- | `mkParaBox a n s` makes a box of height `n` with the text `s`
-- | aligned according to `a`.
mkParaBox :: Alignment -> Int -> Array String -> Box
mkParaBox a n = alignVert top n <<< vcat a <<< map text

-- | Flow the given text into the given width.
flow :: Int -> String -> Array String
flow n t = map (String.take n)
         <<< getLines
         $ foldl addWordP (emptyPara n) (map mkWord <<< words $ t)

newtype Para = Para { paraWidth   :: Int
                    , paraContent :: ParaContent
                    }

newtype ParaContent = Block { fullLines :: Array Line
                            , lastLine  :: Line
                            }

emptyPara :: Int -> Para
emptyPara pw = Para
  { paraWidth: pw
  , paraContent: Block
      { fullLines: []
      , lastLine: Line { lLen: 0, words: [] }
      }
  }

getLines :: Para -> Array String
getLines (Para { paraContent: Block { fullLines: ls, lastLine: l@(Line ll) } })
  | ll.lLen == 0 = getLines' ls
  | otherwise   = getLines' (Array.cons l ls)

getLines' :: Array Line -> Array String
getLines' = map (unwords <<< Array.reverse <<< map (_.word <<< unwrap) <<< _.words <<< unwrap) <<< Array.reverse

newtype Line = Line { lLen :: Int, words :: Array Word }
derive instance newtypeLine :: Newtype Line _

mkLine :: Array Word -> Line
mkLine ws = Line { lLen: sum (map ((_ + 1) <<< _.wLen <<< unwrap) ws) - 1, words: ws }

startLine :: Word -> Line
startLine = mkLine <<< Array.singleton

newtype Word = Word { wLen :: Int, word  :: String }
derive instance newtypeWord :: Newtype Word _

mkWord :: String -> Word
mkWord w = Word { wLen: String.length w, word: w }

addWordP :: Para -> Word -> Para
addWordP (Para { paraWidth: pw, paraContent: Block { fullLines: fl, lastLine: l } }) w
  | wordFits pw w l = Para { paraWidth: pw, paraContent: Block { fullLines: fl, lastLine: addWordL w l } }
  | otherwise       = Para { paraWidth: pw, paraContent: Block { fullLines: Array.cons l fl, lastLine: startLine w } }

addWordL :: Word -> Line -> Line
addWordL w'@(Word w) (Line l) = Line { lLen: l.lLen + w.wLen + 1, words: Array.cons w' l.words }

wordFits :: Int -> Word -> Line -> Boolean
wordFits pw (Word w) (Line l) = l.lLen == 0 || l.lLen + w.wLen + 1 <= pw

--------------------------------------------------------------------------------
--  Alignment  -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | `alignHoriz algn n bx` creates a box of width `n`, with the
-- | contents and height of `bx`, horizontally aligned according to
-- | `algn`.
alignHoriz :: Alignment -> Int -> Box -> Box
alignHoriz a c b = align a AlignFirst (rows b) c b

-- | `alignVert algn n bx` creates a box of height `n`, with the
-- | contents and width of `bx`, vertically aligned according to
-- | `algn`.
alignVert :: Alignment -> Int -> Box -> Box
alignVert a r b = align AlignFirst a r (cols b) b

-- | `align ah av r c bx` creates an `r` x `c` box with the contents
-- | of `bx`, aligned horizontally according to `ah` and vertically
-- | according to `av`.
align :: Alignment -> Alignment -> Int -> Int -> Box -> Box
align ah av r c = createBox r c <<< SubBox ah av

-- | Move a box \"up\" by putting it in a larger box with extra rows,
-- | aligned to the top.  See the disclaimer for 'moveLeft'.
moveUp :: Int -> Box -> Box
moveUp n b = alignVert top (rows b + n) b

-- | Move a box down by putting it in a larger box with extra rows,
-- | aligned to the bottom.  See the disclaimer for 'moveLeft'.
moveDown :: Int -> Box -> Box
moveDown n b = alignVert bottom (rows b + n) b

-- | Move a box left by putting it in a larger box with extra columns,
-- | aligned left.  Note that the name of this function is
-- | something of a white lie, as this will only result in the box
-- | being moved left by the specified amount if it is already in a
-- | larger right-aligned context.
moveLeft :: Int -> Box -> Box
moveLeft n b = alignHoriz left (cols b + n) b

-- | Move a box right by putting it in a larger box with extra
-- | columns, aligned right.  See the disclaimer for 'moveLeft'.
moveRight :: Int -> Box -> Box
moveRight n b = alignHoriz right (cols b + n) b

--------------------------------------------------------------------------------
--  Implementation  ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Render a `Box` as a String, suitable for writing to the screen or
-- | a file.
render :: Box -> String
render = unlines <<< renderBox

-- XXX make QC properties for takeP

-- | \"Padded take\": `takeP a n xs` is the same as `take n xs`, if `n
-- | <= length xs`; otherwise it is `xs` followed by enough copies of
-- | `a` to make the length equal to `n`.
takeP :: forall a. a -> Int -> Array a -> Array a
takeP _ n _      | n <= 0 = []
takeP b n xs =
  let l = Array.length xs
  in if n <= l
       then Array.take n xs
       else xs <> Array.replicate (n - l) b

-- | `takePA ` is like 'takeP', but with alignment.  That is, we
-- | imagine a copy of `xs` extended infinitely on both sides with
-- | copies of `a`, and a window of size `n` placed so that `xs` has
-- | the specified alignment within the window; `takePA algn a n xs`
-- | returns the contents of this window.
takePA :: forall a . Alignment -> a -> Int -> Array a -> Array a
takePA c b n = glue <<< (takeP b (numRev c n) *** takeP b (numFwd c n)) <<< split
  where
    split :: Array a -> Tuple (Array a) (Array a)
    split t = first Array.reverse $ splitAt (numRev c (Array.length t)) t

    splitAt :: Int -> Array a -> Tuple (Array a) (Array a)
    splitAt m arr = Tuple (Array.take m arr) (Array.drop m arr)

    glue :: Tuple (Array a) (Array a) -> Array a
    glue = uncurry (<>) <<< first Array.reverse

    numFwd :: Alignment -> Int -> Int
    numFwd AlignFirst    nn = nn
    numFwd AlignLast     _ = 0
    numFwd AlignCenter1  nn = nn `div` 2
    numFwd AlignCenter2  nn = (nn+1) `div` 2

    numRev :: Alignment -> Int -> Int
    numRev AlignFirst    _ = 0
    numRev AlignLast     nn = nn
    numRev AlignCenter1  nn = (nn+1) `div` 2
    numRev AlignCenter2  nn = nn `div` 2

-- | Generate a string of spaces.
blanks :: Int -> String
blanks = String.fromCharArray <<< flip Array.replicate ' '

-- | Render a box as a list of lines.
renderBox :: Box -> Array String
renderBox (Box { rows: r,  cols: c, content: Blank })         = resizeBox r c [ "" ]
renderBox (Box { rows: r, cols: c, content: Text t })         = resizeBox r c [ t ]
renderBox (Box { rows: r, cols: c, content: Row bs })         = resizeBox r c
                                       <<< merge
                                       <<< map (renderBoxWithRows r)
                                       $ bs

renderBox (Box { rows: r, cols: c, content: Col bs })         = resizeBox r c
                                       <<< Array.concatMap (renderBoxWithCols c)
                                       $ bs

renderBox (Box { rows: r, cols: c, content: SubBox ha va b }) = resizeBoxAligned r c ha va
                                       <<< renderBox
                                       $ b

merge :: Array (Array String) -> Array String
merge as = foldr (Array.zipWith (<>)) (Array.replicate l "") as
  where
    l = fromMaybe 0 (map Array.length (Array.head as))

-- | Render a box as a list of lines, using a given number of rows.
renderBoxWithRows :: Int -> Box -> Array String
renderBoxWithRows r (Box b) = renderBox $ Box b { rows = r }

-- | Render a box as a list of lines, using a given number of columns.
renderBoxWithCols :: Int -> Box -> Array String
renderBoxWithCols c (Box b) = renderBox (Box b { cols = c })

-- | Resize a rendered list of lines.
resizeBox :: Int -> Int -> Array String -> Array String
resizeBox r c = takeP (blanks c) r <<< map (String.fromCharArray <<< takeP ' ' c <<< String.toCharArray)

-- | Resize a rendered list of lines, using given alignments.
resizeBoxAligned :: Int -> Int -> Alignment -> Alignment -> Array String -> Array String
resizeBoxAligned r c ha va = takePA va (blanks c) r <<< map (String.fromCharArray <<< takePA ha ' ' c <<< String.toCharArray)

intersperse :: forall a. a -> Array a -> Array a
intersperse sep as =
    case Array.uncons as of
        Nothing -> []
        Just { head, tail } -> Array.cons head (tail >>= \a -> [ sep, a ])

unlines :: Array String -> String
unlines = String.joinWith "\n"

unwords :: Array String -> String
unwords = String.joinWith " "

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n as =
  Array.range 0 ((Array.length as - 1) / n) <#> \i ->
    Array.slice (i*n) (i*n + n) as
