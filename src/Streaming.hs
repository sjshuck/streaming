{-# LANGUAGE RankNTypes #-}

module Streaming
   (
   -- * An iterable streaming monad transformer
   -- $stream
   Stream,
   -- * Constructing a 'Stream' on a given functor
   yields,
   effect,
   wrap,
   replicates,
   repeats,
   repeatsM,
   unfold,
   never,
   untilJust,
   streamBuild,
   delays,

   -- * Transforming streams
   maps,
   mapsPost,
   mapsM,
   mapsMPost,
   mapped,
   mappedPost,
   hoistUnexposed,
   distribute,
   groups,

   -- * Inspecting a stream
   inspect,

   -- * Splitting and joining 'Stream's
   splitsAt,
   takes,
   chunksOf,
   concats,
   intercalates,
   cutoff,
   -- period,
   -- periods,


   -- * Zipping, unzipping, separating, and rejoining streams
   zipsWith,
   zipsWith',
   zips,
   unzips,
   interleaves,
   separate,
   unseparate,
   decompose,
   expand,
   expandPost,


   -- * Eliminating a 'Stream'
   mapsM_,
   run,
   streamFold,
   iterTM,
   iterT,
   destroy,

   -- * Base functor for streams of individual items
   Of (..),
   lazily,
   strictly,

   -- * Re-exports
   MFunctor(..),
   MMonad(..),
   MonadTrans(..),
   MonadIO(..),
   Compose(..),
   Sum(..),
   Identity(..),
   Alternative((<|>)),
   Bifunctor(..),

   join,
   liftM,
   liftM2,
   liftA2,
   liftA3,
   void,
   (<>)
   )
   where
import Streaming.Internal
import Streaming.Prelude
import Control.Monad.Morph
import Control.Monad
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad.Trans
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Bifunctor


{- $stream

    The @`Stream` f m r@ data type can be used to represent any effectful
    succession of steps arising in some monad.
    The form of the steps is determined by the first, `Functor`ial
    parameter @f@, and the monad of the underlying effects
    is the second parameter @m@.

    This module exports combinators that pertain to the general case.
    Some of these are quite abstract and pervasive in use of the library,
    e.g.

>   maps    :: (forall x . f x -> g x)     -> Stream f m r -> Stream g m r
>   mapped  :: (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
>   hoist   :: (forall x . m x -> n x)     -> Stream f m r -> Stream f n r -- from the MFunctor instance
>   concats :: Stream (Stream f m) m r     -> Stream f m r

    (It is assumed throughout that @(Functor f, Functor g, Monad m, Monad n)@.)

    Others combinators are more concrete in functionality:

>   chunksOf     :: Int -> Stream f m r -> Stream (Stream f m) m r
>   splitsAt     :: Int -> Stream f m r -> Stream f m (Stream f m r)
>   zipsWith     :: (forall x y. f x -> g y -> h (x, y))
>                -> Stream f m r -> Stream g m r -> Stream h m r
>   zipsWith'    :: (forall x y p. (x -> y -> p) -> f x -> g y -> h p)
>                -> Stream f m r -> Stream g m r -> Stream h m r
>   intercalates :: Stream f m () -> Stream (Stream f m) m r -> Stream f m r
>   unzips       :: Stream (Compose f g) m r ->  Stream f (Stream g m) r
>   separate     :: Stream (Sum f g) m r -> Stream f (Stream g m) r  -- see partitionEithers
>   unseparate   :: Stream f (Stream g) m r -> Stream (Sum f g) m r
>   groups       :: Stream (Sum f g) m r -> Stream (Sum (Stream f m) (Stream g m)) m r

    One benefit to a streaming library that such a general type confers is
    the ability to represent stream segmentation, for example
    to implement @Prelude/Data.List@ combinators that involve
    \'lists of lists\' etc. See for example
    <http://www.haskellforall.com/2013/09/perfect-streaming-using-pipes-bytestring.html this post>
    on the optimal expression of a streaming @lines@ function.

    The module 'Streaming.Prelude' exports combinators relating to

> Stream (Of a) m r

    where @Of a r = !a :> r@, a left-strict pair.


   This corresponds to a @Producer@ or @Source@ or @Generator@ and
   easily interoperates with types bearing those or similar names in
   @conduit@, @io-streams@, and @pipes@.
-}
