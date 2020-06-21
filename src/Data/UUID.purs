module Data.UUID
  ( UUID
  , emptyUUID
  , genUUID
  , parseUUID
  , genv3UUID
  , genv5UUID
  , toString
  , toBytes
  , parseBytesUUID
  ) where

import Prelude
import Data.Maybe (Maybe(Nothing, Just))
import Data.Either (note, Either(Right))
import Data.Generic.Rep (class Generic)
import Data.Int (hexadecimal, fromStringAs, toStringAs)
import Data.String (splitAt, length)
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.List.NonEmpty (singleton)
import Data.Vec (Vec, fill, index)
import Data.Array.NonEmpty ((!!))
import Data.Typelevel.Num (D16, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15)
import Data.Typelevel.Num.Ops (class Lt)
import Data.Typelevel.Num.Sets (class Nat)
import Control.Monad.Except.Trans (except)
import Effect (Effect)
import Foreign (ForeignError(..))
import Foreign as Foreign
import Foreign.Class (class Encode, class Decode)
import Partial.Unsafe (unsafePartial)

newtype UUID
  = UUID String

emptyUUID :: UUID
emptyUUID = UUID "00000000-0000-0000-0000-000000000000"

foreign import getUUIDImpl :: Effect String

-- | Generates a v4 UUID
genUUID :: Effect UUID
genUUID = getUUIDImpl >>= pure <<< UUID

foreign import validateV4UUID :: String -> Boolean

-- | Validates a String as a v4 UUID
parseUUID :: String -> Maybe UUID
parseUUID str = case validateV4UUID str of
  true -> Just $ UUID str
  _ -> Nothing

foreign import getUUID3Impl :: String -> String -> String

genv3UUID :: String -> UUID -> UUID
genv3UUID s (UUID n) = UUID $ getUUID3Impl s n

foreign import getUUID5Impl :: String -> String -> String

genv5UUID :: String -> UUID -> UUID
genv5UUID s (UUID n) = UUID (getUUID5Impl s n)

instance showUUID :: Show UUID where
  show (UUID uuid) = "(UUID " <> uuid <> ")"

derive instance eqUUID :: Eq UUID

derive instance ordUUID :: Ord UUID

toString :: UUID -> String
toString (UUID uuid) = uuid

derive instance genericUUID :: Generic UUID _

instance decodeUUID :: Decode UUID where
  decode x = Foreign.readString x >>=
               parseUUID
               >>> note (singleton $ ForeignError "Failed to parse foreign UUID")
               >>> except

instance encodeUUID :: Encode UUID where
  encode = toString >>> Foreign.unsafeToForeign

toBytes :: UUID -> Vec D16 Int
toBytes uuid = unsafePartial $ case match uuidRegex (toString uuid) of
  Just (mXs :: _ (Maybe String)) ->
    let blocks :: Maybe (Vec D16 String)
        blocks = do
          block1 <- join (mXs !! 1)
          block2 <- join (mXs !! 2)
          block3 <- join (mXs !! 3)
          block4 <- join (mXs !! 4)
          block5 <- join (mXs !! 5)
          let {before: byte0, after: block1'} = splitAt 2 block1
              {before: byte1, after: block1''} = splitAt 2 block1'
              {before: byte2, after: byte3} = splitAt 2 block1''
              {before: byte4, after: byte5} = splitAt 2 block2
              {before: byte6, after: byte7} = splitAt 2 block3
              {before: byte8, after: byte9} = splitAt 2 block4
              {before: byte10, after: block5'} = splitAt 2 block5
              {before: byte11, after: block5''} = splitAt 2 block5'
              {before: byte12, after: block5'''} = splitAt 2 block5''
              {before: byte13, after: block5''''} = splitAt 2 block5'''
              {before: byte14, after: byte15} = splitAt 2 block5''''
          pure $ fill \i -> unsafePartial $ case i of
            0 -> byte0
            1 -> byte1
            2 -> byte2
            3 -> byte3
            4 -> byte4
            5 -> byte5
            6 -> byte6
            7 -> byte7
            8 -> byte8
            9 -> byte9
            10 -> byte10
            11 -> byte11
            12 -> byte12
            13 -> byte13
            14 -> byte14
            15 -> byte15
    in  unsafePartial $ case blocks of
          Just b ->
            let go byte = unsafePartial $ case fromStringAs hexadecimal byte of
                  Just x -> x
            in  map go b
  where
    hexRegex :: String
    hexRegex = "[0-9a-fA-F]"
    uuidRegexString :: String
    uuidRegexString =
      "^(" <> hexRegex
        <> "{8})-(" <> hexRegex
        <> "{4})-(" <> hexRegex
        <> "{4})-(" <> hexRegex
        <> "{4})-(" <> hexRegex
        <> "{12})$"
    uuidRegex :: Regex
    uuidRegex = unsafePartial $ case regex uuidRegexString noFlags of
      Right r -> r

parseBytesUUID :: Vec D16 Int -> Maybe UUID
parseBytesUUID bytes = parseUUID shownBytes
  where
    shownBytes =
      let toHexString x =
            let str = toStringAs hexadecimal x
            in  if length str == 1
                  then "0" <> str
                  else str
          bytes' = map toHexString bytes
          get :: forall i. Nat i => Lt i D16 => i -> String
          get = index bytes'
          block1 =
            get d0 <> get d1 <> get d2 <> get d3
          block2 =
            get d4 <> get d5
          block3 =
            get d6 <> get d7
          block4 =
            get d8 <> get d9
          block5 =
            get d10 <> get d11 <> get d12 <> get d13 <> get d14 <> get d15
      in  block1 <> "-" <> block2 <> "-" <> block3 <> "-" <> block4 <> "-" <> block5
