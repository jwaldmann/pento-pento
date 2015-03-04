-- | http://www.iread.it/Poly/P55.html

{-# language PatternSignatures #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

import Satchmo.Boolean
import Prelude hiding ( not, and, or )
import Satchmo.SAT.Mini
import qualified Satchmo.Counting.Binary as C
import Satchmo.Code

import qualified Data.Array as A
import qualified Satchmo.Array as SA

import qualified Data.Map.Strict as M
import qualified Satchmo.Map as SM

import Control.Monad ( guard, forM, void, when )
import System.Environment
import qualified System.Console.ANSI as ANSI
import Data.List ( tails )

main :: IO ()
main = do [ ps, w ] <- getArgs ; mainf ps (read w)

test = mainf "NX" 12

newtype Nocode c = Nocode c
instance Monad m => Decode m (Nocode c) c where
  decode (Nocode c) = return c

mainf ps w = do
  out <- solve $ do
    pic <- SA.unknown ((0,0),(w-1,w-1)) boolean
    assert $ SA.elems pic
    let cover p = do
          place <-
            SA.unknown (((0,0),(0,False)),((w-1,w-1),(3,True))) boolean
          let cover :: M.Map (Int,Int) [Boolean]
              cover = M.fromListWith (++) $ do
                (i@(offset,orientation),v) <- SA.assocs place
                let q = realize i p

                guard $ all ( \ o ->
                   q /= realize (offset, o) p
                            )
                  $ init $ A.range ((0,False),orientation)
                
                guard $ all ( \ i -> A.inRange (SA.bounds pic) i )
                      $ A.indices q
                (i,True) <- A.assocs q                
                return (i,[v ])
          void $ forM (M.assocs cover) $ \ (i :: (Int,Int),vs) -> do
            when True  $ do ok <- C.atmost 1 vs ; assert [ ok ]
            when False $ void $ sequence $ do
              (v : ws) <- tails vs ; w <- ws
              return $ assert $ map not [v,w]
            present <- or vs
            ok <- equals2 present (pic SA.! i) ; assert [ok]
          return (Nocode p , place)
    places <- forM ps $ \ p -> cover (shapes M.! p)
    return $ decode (pic,places)
  case out of
    Just ( pic :: A.Array (Int,Int) Bool
         , places :: [(A.Array (Int,Int) Bool
                      ,A.Array ((Int,Int),(Int, Bool)) Bool)  ]
         ) -> do
      void $ forM places $ \ ppl -> putStrLn $ unlines $ form pic ppl

realize ((dx,dy),(r,m)) p = shift (dx,dy)
                      $ ( if m then mirror else id )
                      $ ( iterate rotate p !! r )

form pic (shape,place) = do
  let ((o,l),(u,r)) = A.bounds pic
      m = M.fromList $ do
        (n,i) <- zip[0..] $ map fst $ filter snd $ A.assocs place
        let q = realize i shape
        (j,True) <- A.assocs q
        return (j, n)
  row <- [ o .. u ]
  return $ do
      col <- [ l .. r ]
      case M.lookup (row,col) m of
        Just n ->
          let color n = toEnum $ 1 + mod n 7
          in  ANSI.setSGRCode
                [ ANSI.SetColor ANSI.Background ANSI.Dull $ color n ]
              ++ "  "
              ++ ANSI.setSGRCode
              [ ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.White ]
        Nothing -> ". "

shift :: (Int,Int) -> A.Array (Int,Int) v -> A.Array (Int,Int) v
shift (dx,dy) a = 
  let ((o,l),(u,r)) = A.bounds a
  in A.array ((dx+o,dy+l),(dx+u,dy+r)) $ do
        ((x,y),v) <- A.assocs a
        return $ ((dx+x,dy+y),v)

mirror :: A.Array (Int,Int) v -> A.Array (Int,Int) v 
mirror a =
  let ((o,l),(u,r)) = A.bounds a
  in A.array ((l,o),(r,u)) $ do
        ((x,y),v) <- A.assocs a
        return $ ((y,x),v)

rotate :: A.Array (Int,Int) v -> A.Array (Int,Int) v 
rotate a = 
  let ((o,l),(u,r)) = A.bounds a
  in A.array ((l,o),(r,u)) $ do
        ((x,y),v) <- A.assocs a
        return $ ((r-(y-l),x),v)

shapes = M.fromList
  [ ('F', parse [ "XX.", ".XX", ".X." ] )
  , ('I', parse [ "XXXXX" ] )
  , ('L', parse [ "XXXX", "X..." ] )
  , ('N', parse [ "XX..", ".XXX" ] )
  , ('P', parse [ "XXX", "XX." ] )
  , ('T', parse [ "XXX", ".X.", ".X." ] )  
  , ('U', parse [ "X.X", "XXX" ] )
  , ('V', parse [ "X..", "X..", "XXX" ] )
  , ('W', parse [ "XX.", ".XX", "..X" ] )
  , ('X', parse [ ".X.", "XXX", ".X." ] )
  , ('Y', parse [ "XXXX", ".X.." ] )
  , ('Z', parse [ "X..", "XXX", "..X" ] )
  ]

parse ss =
  let h = length ss ; w = length $ head ss
  in  A.array ((0,0),(h-1,w-1)) $ do
        (x,row) <- zip [0..] ss
        (y,c)   <- zip [0..] row
        return ( (x,y) , case c of '.' -> False ; 'X' -> True )

