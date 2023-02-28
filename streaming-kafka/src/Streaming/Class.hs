module Streaming.Class where

import           RIO
import           Kafka.Producer
import           Kafka.Consumer
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString      as ByteString
import           Data.Aeson

class ToKafka k v where
  toKafka :: TopicName -> k -> v -> ProducerRecord

class FromKafka k v where
  fromKafka :: ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> Maybe (k, v)

instance (ToJSON a, ToJSON b) => ToKafka a b where
  toKafka topic k v =
      ProducerRecord topic UnassignedPartition encodedKey encodedValue (headersFromList [])
    where
      encodedValue = asKey v
      encodedKey   = asKey k

asKey :: (ToJSON a) => a -> Maybe ByteString.ByteString
asKey = Just . BS.toStrict . encode

fromKey :: (FromJSON a) => Maybe ByteString.ByteString -> Maybe a
fromKey (Just bs) = (decode . BS.fromStrict) bs
fromKey _         = Nothing