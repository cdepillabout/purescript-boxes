
module Text.PrettyPrint.Boxes where
-- module Text.PrettyPrint.Boxes
--     ( -- * Constructing boxes
--       Box(Box, content)
--     , nullBox
--     , emptyBox
--     , char
--     , text
--     , para
--     , columns

--       -- * Layout of boxes

--     , (<>)
--     , (<+>)
--     , hcat
--     , hsep

--     , (//)
--     , (/+/)
--     , vcat
--     , vsep

--     , punctuateH, punctuateV

--     -- * Alignment

--     , Alignment(..)

--     , Content(..)
--     , left, right
--     , top, bottom
--     , center1, center2

--     , moveLeft
--     , moveRight
--     , moveUp
--     , moveDown

--     , alignHoriz
--     , alignVert
--     , align

--     -- * Inspecting boxes

--     , rows
--     , cols

--     -- * Rendering boxes

--     , render
--     , printBox

--     ) where

import Prelude hiding (bottom, top)

import Data.Array as Array
import Data.Char
import Data.Foldable
import Data.Generic
import Data.Maybe
import Data.Profunctor.Strong
import Data.String
import Data.Tuple

-- import Data.String (words, unwords)
-- import Data.List (words, unwords)

-- import Data.String (IsString(..))

-- import Control.Arrow ((***), first)
-- import Data.List (foldl', intersperse)

-- import Data.List.Split (chunksOf)

-- | The basic data type.  A box has a specified size and some sort of
-- | contents.
newtype Box = Box { rows    :: Int
                  , cols    :: Int
                  , content :: Content
                  }

derive instance genericBox :: Generic Box
instance showBox :: Show Box where show = gShow
instance eqBox :: Eq Box where eq = gEq

createBox :: Int -> Int -> Content -> Box
createBox rows cols content = Box { rows: rows, cols: cols, content: content }

-- | Convenient ability to use bare string literals as boxes.
-- instance IsString Box where
--   fromString = text

-- | Data type for specifying the alignment of boxes.
data Alignment = AlignFirst    -- ^ Align at the top/left.
               | AlignCenter1  -- ^ Centered, biased to the top/left.
               | AlignCenter2  -- ^ Centered, biased to the bottom/right.
               | AlignLast     -- ^ Align at the bottom/right.

derive instance genericAlignment :: Generic Alignment
instance showAlignment :: Show Alignment where show = gShow
instance eqAlignment :: Eq Alignment where eq = gEq

-- | Align boxes along their tops.
top :: Alignment
top = AlignFirst

-- | Align boxes along their bottoms.
bottom :: Alignment
bottom = AlignLast

-- | Align boxes to the left.
left :: Alignment
left = AlignFirst

-- | Align boxes to the right.
right :: Alignment
right = AlignLast

-- | Align boxes centered, but biased to the left/top in case of
--   unequal parities.
center1 :: Alignment
center1 = AlignCenter1

-- | Align boxes centered, but biased to the right/bottom in case of
--   unequal parities.
center2 :: Alignment
center2 = AlignCenter2

-- | Contents of a box.
data Content = Blank              -- ^ No content.
             | Text String        -- ^ A raw string.
             | Row (Array Box)    -- ^ A row of sub-boxes.
             | Col (Array Box)    -- ^ A column of sub-boxes.
             | SubBox Alignment Alignment Box
                                  -- ^ A sub-box with a specified alignment.

derive instance genericContent :: Generic Content
instance showContent :: Show Content where show = gShow
instance eqContent :: Eq Content where eq = gEq

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
char = createBox 1 1 <<< Text <<< toString

-- | A (`1 x len`) box containing a string of length `len`.
text :: String -> Box
text t = createBox 1 (length t) (Text t)

-- | Paste two boxes together horizontally, using a default (top)
--   alignment.
-- (<>) :: Box -> Box -> Box
-- l <> r = hcat top [l,r]

-- -- | Paste two boxes together horizontally with a single intervening
-- -- | column of space, using a default (top) alignment.
-- (<+>) :: Box -> Box -> Box
-- l <+> r = hcat top [l, emptyBox 0 1, r]

-- -- | Paste two boxes together vertically, using a default (left)
-- -- | alignment.
-- (//) :: Box -> Box -> Box
-- t // b = vcat left [t,b]

-- -- | Paste two boxes together vertically with a single intervening row
-- -- | of space, using a default (left) alignment.
-- (/+/) :: Box -> Box -> Box
-- t /+/ b = vcat left [t, emptyBox 1 0, b]

-- -- | Glue a list of boxes together horizontally, with the given alignment.
-- hcat :: Foldable f => Alignment -> f Box -> Box
-- hcat a bs = Box h w (Row $ map (alignVert a h) bsl)
--   where
--     (w, h) = sumMax cols 0 rows bsl
--     bsl = toList bs

-- -- | `hsep sep a bs` lays out `bs` horizontally with alignment `a`,
-- -- | with `sep` amount of space in between each.
-- hsep :: Foldable f => Int -> Alignment -> f Box -> Box
-- hsep sep a bs = punctuateH a (emptyBox 0 sep) bs

-- -- | Glue a list of boxes together vertically, with the given alignment.
-- vcat :: Foldable f => Alignment -> f Box -> Box
-- vcat a bs = Box h w (Col $ map (alignHoriz a w) bsl)
--   where
--     (h, w) = sumMax rows 0 cols bsl
--     bsl = toList bs

-- -- Calculate a sum and a maximum over a list in one pass. If the list is
-- -- empty, the maximum is reported as the given default. This would
-- -- normally be done using the foldl library, but we don't want that
-- -- dependency.
-- sumMax :: (Num n, Ord b, Foldable f) => (a -> n) -> b -> (a -> b) -> f a -> (n, b)
-- sumMax f defaultMax g as = foldr go (,) as 0 defaultMax
--   where
--     go a r n b = (r $! f a + n) $! g a `max` b

-- -- | `vsep sep a bs` lays out `bs` vertically with alignment `a`,
-- -- | with `sep` amount of space in between each.
-- vsep :: Foldable f => Int -> Alignment -> f Box -> Box
-- vsep sep a bs = punctuateV a (emptyBox sep 0) (toList bs)

-- -- | `punctuateH a p bs` horizontally lays out the boxes `bs` with a
-- -- | copy of `p` interspersed between each.
-- punctuateH :: Foldable f => Alignment -> Box -> f Box -> Box
-- punctuateH a p bs = hcat a (intersperse p (toList bs))

-- -- | A vertical version of 'punctuateH'.
-- punctuateV :: Foldable f => Alignment -> Box -> f Box -> Box
-- punctuateV a p bs = vcat a (intersperse p (toList bs))

-- --------------------------------------------------------------------------------
-- --  Paragraph flowing  ---------------------------------------------------------
-- --------------------------------------------------------------------------------

-- -- | `para algn w t` is a box of width `w`, containing text `t`,
-- -- | aligned according to `algn`, flowed to fit within the given
-- -- | width.
-- para :: Alignment -> Int -> String -> Box
-- para a n t = (\ss -> mkParaBox a (length ss) ss) $ flow n t

-- -- | `columns w h t` is a list of boxes, each of width `w` and height
-- -- | at most `h`, containing text `t` flowed into as many columns as
-- -- | necessary.
-- columns :: Alignment -> Int -> Int -> String -> [Box]
-- columns a w h t = map (mkParaBox a h) . chunksOf h $ flow w t

-- -- | `mkParaBox a n s` makes a box of height `n` with the text `s`
-- -- | aligned according to `a`.
-- mkParaBox :: Alignment -> Int -> [String] -> Box
-- mkParaBox a n = alignVert top n . vcat a . map text

-- -- | Flow the given text into the given width.
-- flow :: Int -> String -> [String]
-- flow n t = map (take n)
--          . getLines
--          $ foldl' addWordP (emptyPara n) (map mkWord . words $ t)

-- data Para = Para { paraWidth   :: Int
--                  , paraContent :: ParaContent
--                  }
-- data ParaContent = Block { fullLines :: [Line]
--                          , lastLine  :: Line
--                          }

-- emptyPara :: Int -> Para
-- emptyPara pw = Para pw (Block [] (Line 0 []))

-- getLines :: Para -> [String]
-- getLines (Para _ (Block ls l))
--   | lLen l == 0 = process ls
--   | otherwise   = process (l:ls)
--   where process = map (unwords . reverse . map getWord . getWords) . reverse

-- data Line = Line { lLen :: Int, getWords :: [Word] }

-- mkLine :: [Word] -> Line
-- mkLine ws = Line (sum (map ((+1) . wLen) ws) - 1) ws

-- startLine :: Word -> Line
-- startLine = mkLine . (:[])

-- data Word = Word { wLen :: Int, getWord  :: String }

-- mkWord :: String -> Word
-- mkWord w = Word (length w) w

-- addWordP :: Para -> Word -> Para
-- addWordP (Para pw (Block fl l)) w
--   | wordFits pw w l = Para pw (Block fl (addWordL w l))
--   | otherwise       = Para pw (Block (l:fl) (startLine w))

-- addWordL :: Word -> Line -> Line
-- addWordL w (Line len ws) = Line (len + wLen w + 1) (w:ws)

-- wordFits :: Int -> Word -> Line -> Bool
-- wordFits pw w l = lLen l == 0 || lLen l + wLen w + 1 <= pw

-- --------------------------------------------------------------------------------
-- --  Alignment  -----------------------------------------------------------------
-- --------------------------------------------------------------------------------

-- | `alignHoriz algn n bx` creates a box of width `n`, with the
-- | contents and height of `bx`, horizontally aligned according to
-- | `algn`.
alignHoriz :: Alignment -> Int -> Box -> Box
alignHoriz a c (Box b) = align a AlignFirst (b.rows) c (Box b)

-- | `alignVert algn n bx` creates a box of height `n`, with the
-- | contents and width of `bx`, vertically aligned according to
-- | `algn`.
alignVert :: Alignment -> Int -> Box -> Box
alignVert a r (Box b) = align AlignFirst a r (b.cols) (Box b)

-- | `align ah av r c bx` creates an `r` x `c` box with the contents
-- | of `bx`, aligned horizontally according to `ah` and vertically
-- | according to `av`.
align :: Alignment -> Alignment -> Int -> Int -> Box -> Box
align ah av r c bx = createBox r c (SubBox ah av bx)

-- | Move a box \"up\" by putting it in a larger box with extra rows,
-- | aligned to the top.  See the disclaimer for 'moveLeft'.
moveUp :: Int -> Box -> Box
moveUp n (Box b) = alignVert top (b.rows + n) (Box b)

-- | Move a box down by putting it in a larger box with extra rows,
-- | aligned to the bottom.  See the disclaimer for 'moveLeft'.
moveDown :: Int -> Box -> Box
moveDown n (Box b) = alignVert bottom (b.rows + n) (Box b)

-- | Move a box left by putting it in a larger box with extra columns,
-- | aligned left.  Note that the name of this function is
-- | something of a white lie, as this will only result in the box
-- | being moved left by the specified amount if it is already in a
-- | larger right-aligned context.
moveLeft :: Int -> Box -> Box
moveLeft n (Box b) = alignHoriz left (b.cols + n) (Box b)

-- | Move a box right by putting it in a larger box with extra
-- | columns, aligned right.  See the disclaimer for 'moveLeft'.
moveRight :: Int -> Box -> Box
moveRight n (Box b) = alignHoriz right (b.cols + n) (Box b)

-- --------------------------------------------------------------------------------
-- --  Implementation  ------------------------------------------------------------
-- --------------------------------------------------------------------------------

-- -- | Render a 'Box' as a String, suitable for writing to the screen or
-- -- | a file.
-- render :: Box -> String
-- render = unlines . renderBox

-- XXX make QC properties for takeP

-- | \"Padded take\": `takeP a n xs` is the same as `take n xs`, if `n
-- | <= length xs`; otherwise it is `xs` followed by enough copies of
-- | `a` to make the length equal to `n`.
takeP :: forall a . a -> Int -> Array a -> Array a
takeP _ n _      | n <= 0 = []
takeP b n array =
    case Array.uncons array of
        Just { head: head, tail: tail } -> head `Array.cons` takeP b (n - 1) tail
        Nothing -> Array.replicate n b

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
    numFwd AlignFirst    n = n
    numFwd AlignLast     _ = 0
    numFwd AlignCenter1  n = n `div` 2
    numFwd AlignCenter2  n = (n+1) `div` 2

    numRev :: Alignment -> Int -> Int
    numRev AlignFirst    _ = 0
    numRev AlignLast     n = n
    numRev AlignCenter1  n = (n+1) `div` 2
    numRev AlignCenter2  n = n `div` 2

-- | Generate a string of spaces.
blanks :: Int -> String
blanks = fromCharArray <<< flip Array.replicate ' '

-- | Render a box as a list of lines.
renderBox :: Box -> Array String
renderBox (Box { rows: r, cols: c, content: Blank }) =
    resizeBox r c [""]
renderBox (Box { rows: r, cols: c, content: Text t }) =
    resizeBox r c [t]
renderBox (Box { rows: r, cols: c, content: Row bs }) =
    resizeBox r c <<< merge $ map (renderBoxWithRows r) bs
  where
    merge :: Array (Array String) -> Array String
    merge = foldr (zipWithDefaults "" "" (<>)) []
renderBox (Box { rows: r, cols: c, content: Col bs }) =
    resizeBox r c $ Array.concatMap (renderBoxWithCols c) bs
renderBox (Box { rows: r, cols: c, content: SubBox ha va b}) =
    resizeBoxAligned r c ha va $ renderBox b

-- | Render a box as a list of lines, using a given number of rows.
renderBoxWithRows :: Int -> Box -> Array String
renderBoxWithRows r (Box b) = renderBox $ Box b { rows = r }

-- | Render a box as a list of lines, using a given number of columns.
renderBoxWithCols :: Int -> Box -> Array String
renderBoxWithCols c (Box b) = renderBox (Box b { cols = c })

-- | Resize a rendered list of lines.
resizeBox :: Int -> Int -> Array String -> Array String
resizeBox r c = takeP (blanks c) r <<< map (fromCharArray <<< takeP ' ' c <<< toCharArray)

-- | Resize a rendered list of lines, using given alignments.
resizeBoxAligned :: Int -> Int -> Alignment -> Alignment -> Array String -> Array String
resizeBoxAligned r c ha va = takePA va (blanks c) r <<< map (fromCharArray <<< takePA ha ' ' c <<< toCharArray)

zipWithDefaults :: forall a b c . a -> b -> (a -> b -> c) -> Array a -> Array b -> Array c
zipWithDefaults defaultA defaultB f xs ys = Array.reverse $ go xs ys []
  where
    go :: Array a -> Array b -> Array c -> Array c
    go as bs acc =
        case Tuple (Array.uncons as) (Array.uncons bs) of
            Tuple Nothing Nothing ->
                acc
            Tuple (Just { head: aHead, tail: aTail }) Nothing ->
                go aTail [] $ Array.cons (f aHead defaultB) acc
            Tuple Nothing (Just { head: bHead, tail: bTail }) ->
                go [] bTail $ Array.cons (f defaultA bHead) acc
            Tuple (Just { head: aHead, tail: aTail }) (Just { head: bHead, tail: bTail }) ->
                go aTail bTail $ Array.cons (f aHead bHead) acc
