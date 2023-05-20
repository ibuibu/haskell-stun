module Lib (runServer, sendBindingRequest) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put (Put, putWord16be, putWord32be, runPut)
import Data.Bits (Bits (shiftR, testBit, xor, (.&.), (.|.)))
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Char
import Data.Word
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Numeric
import System.Random

runServer :: HostName -> ServiceName -> IO ()
runServer addr port = do
  addrinfos <- getAddrInfo Nothing (Just addr) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  forever $ do
    (message, from) <- recvFrom sock 4096
    let t = runGetOrFail getRequest $ L.fromChunks [message]
    case t of
      Left errorMessage -> print errorMessage
      Right (_, _, (Request, Binding, transactionId)) -> sendResponse sock from transactionId
      Right (_, _, _) -> print "not Binding Request"

getRequest :: Get (StunClass, StunMethod, TransactionId)
getRequest = do
  type' <- getWord16be
  when (testBit type' 15 || testBit type' 14) $ fail "First two binary must be 0."
  let class' = getStunClass type'
  let method = getStunMethod type'

  msgLen <- getWord16be
  unless (msgLen `mod` 4 == 0) $ fail "Length not multiple of 4"

  mc <- getWord32be
  unless (mc == magicCookie) $ fail "Magic cookie 0x2112A442 not found"

  transactionId <- getTransactionId

  return (class', method, transactionId)

sendResponse :: Socket -> SockAddr -> TransactionId -> IO ()
sendResponse sock addr transactionId = do
  let message = runPut $ putStunMessage SuccessResponse Binding transactionId (XorMappedAddress addr)
  _ <- sendTo sock (B.concat $ L.toChunks message) addr
  return ()

sendBindingRequest :: HostName -> ServiceName -> IO ()
sendBindingRequest targetAddr port = do
  addrinfos <- getAddrInfo Nothing (Just targetAddr) (Just port)
  let serveraddr = head addrinfos
  let targetSockAddr = addrAddress serveraddr
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  transactionId <- generateTransactionId
  let request = runPut $ putStunMessage Request Binding transactionId Software
  _ <- sendTo sock (B.concat $ L.toChunks request) targetSockAddr
  return ()


data StunMessage = StunMessage StunHeader StunAttribute deriving (Show)

data StunHeader = StunHeader StunType StunMessageLength TransactionId deriving (Show)

magicCookie :: Word32
magicCookie = 0x2112A442

magicCookieFront :: Word16
magicCookieFront = fromIntegral $ shiftR magicCookie 16

type StunMessageLength = Word16

putStunHeader :: StunHeader -> Put
putStunHeader (StunHeader (StunType class' method) len transactionId) = do
  putStunType (StunType class' method)
  putWord16be len
  putWord32be magicCookie
  putTransactionId transactionId

type TransactionId = (Word32, Word32, Word32)

putTransactionId :: TransactionId -> Put
putTransactionId (id1, id2, id3) = mapM_ putWord32be [id1, id2, id3]

-- TODO
putStunMessage class' method transactionId attr = do
  putStunHeader (StunHeader (StunType class' method) 12 transactionId)
  putStunAttribute attr

data StunType = StunType StunClass StunMethod deriving (Show)

data StunClass = Request | Indication | SuccessResponse | ErrorResponse deriving (Show, Eq)

putStunType :: StunType -> Put
putStunType (StunType stunClass stunMethod) = do
  let class' = findKey stunClass stunClassDic
  let method = findKey stunMethod stunMethodDic
  putWord16be $ fromIntegral (class' .|. method)

stunClassDic :: [(StunClass, Integer)]
stunClassDic =
  [ (Request, 0b0000000000000000),
    (Indication, 0b0000000000010000),
    (SuccessResponse, 0b0000000100000000),
    (ErrorResponse, 0b0000000100010000)
  ]

data StunMethod = Binding | Allocate | Refresh | Send | Data | CreatePermission | ChannelBind deriving (Show, Eq)

stunMethodDic :: [(StunMethod, Integer)]
stunMethodDic = [(Binding, 1)]

-- TODO
data StunAttribute = MappedAddress | XorMappedAddress SockAddr | Software deriving (Show)

type StunAttributeLength = Word16

type StunAttributeValue = Int

-- TODO
putStunAttribute (XorMappedAddress sockAddr) = do
  putWord16be 0x0020 -- xor mapped address
  putWord16be 8
  putXorMappedAddress sockAddr

-- TODO
putStunAttribute (Software) = do
  putWord16be 0x8022
  putWord16be 8
  putWord16be 8

putXorMappedAddress :: SockAddr -> Put
putXorMappedAddress sa = do
  case sa of
    SockAddrInet port addr -> do
      let xorPort = xor (fromIntegral port) magicCookieFront
      let xorAddr = xor (changeEndianAddress addr) magicCookie
      putWord16be 0x0001 -- ipv4
      putWord16be xorPort
      putWord32be xorAddr
    _ -> putWord16be 0

changeEndianAddress :: HostAddress -> HostAddress
changeEndianAddress addr = tupleToHostAddress $ (\(a, b, c, d) -> (d, c, b, a)) $ hostAddressToTuple addr

getStunClass :: Integral p => p -> StunClass
getStunClass type' = findVal masked stunClassDic
  where
    t = toInteger type'
    masked = t .&. 0b0000000100010000

getStunMethod :: Integral p => p -> StunMethod
getStunMethod type' = findVal masked stunMethodDic
  where
    t = toInteger type'
    masked = t .&. 0b1111111011101111

getTransactionId :: Get TransactionId
getTransactionId = do
  id1 <- getWord32be
  id2 <- getWord32be
  id3 <- getWord32be
  return (id1, id2, id3)

-- TODO Either
findKey :: Eq a => a -> [(a, c)] -> c
findKey key = snd . head . filter (\(k, _) -> k == key)

-- TODO Either
findVal :: Eq a => a -> [(c, a)] -> c
findVal val = fst . head . filter (\(_, v) -> v == val)

generateTransactionId :: IO (Word32, Word32, Word32)
generateTransactionId = do
  let max' = 2 ^ 96
  let maxMinus1 = 2 ^ 96 - 1
  let randWord32 ma = randomRIO (0, ma) :: IO Word32
  r1 <- randWord32 max'
  r2 <- randWord32 max'
  r3 <- randWord32 maxMinus1
  return (r1,r2,r3)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

toBinary :: Int -> String
toBinary x = showIntAtBase 2 intToDigit x ""

pad0 :: Int -> String -> String
pad0 x y = replicate (x - length y) '0' ++ y

join' :: [String] -> String
join' [] = ""
join' (x : xs) = x ++ join xs
