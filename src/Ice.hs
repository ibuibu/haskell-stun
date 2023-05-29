module Ice (test, getHostAddresses) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as LB
import Data.Digest.Pure.MD5
import Data.List
import Network.Info
import Network.Socket

test :: IO ()
test = do
  a <- getHostAddresses
  b <- mapM getComponentCandidates a
  print $ map toSdp b
  print $ map (fromSdp . toSdp) b

-- TODO Add ipv6
-- 0x0100007f = 127.0.0.1
getHostAddresses :: IO [String]
getHostAddresses = do
  map show . filter (\x -> notElem x $ map IPv4 [0, 0x0100007f]) . map ipv4 <$> getNetworkInterfaces

getComponentCandidates :: HostName -> IO Candidate
getComponentCandidates address = do
  addrinfos <- getAddrInfo Nothing (Just address) Nothing
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  sockName <- getSocketName sock
  case sockName of
    SockAddrInet port' _ -> do
      return Candidate {foundation = candidateFoundation "host" "udp" address, component = 1, transport = "udp", priority = candidatePriority 1 "host" 65535, host = address, port = fromIntegral port', type' = "host"}
    _ -> fail "hoge" -- TODO

data Candidate = Candidate {foundation :: String, component :: Int, transport :: String, priority :: Int, host :: String, port :: Int, type' :: String} deriving (Show)

candidateFoundation :: [Char] -> [Char] -> [Char] -> String
candidateFoundation type' transport baseAddress = show $ md5 $ LB.fromStrict $ C8.pack $ type' ++ transport ++ baseAddress

candidatePriority :: Num a => a -> String -> a -> a
candidatePriority component type' localPref = (2 ^ (24 :: Int)) * typePref type' + (2 ^ (8 :: Int)) * localPref + (256 - component)
  where
    typePref "host" = 126
    typePref "prflx" = 110
    typePref "srflx" = 100
    typePref _ = 0

toSdp :: Candidate -> String
toSdp c = unwords $ map ($ c) [foundation, show . component, transport, show . priority, host, show . port, typ, type']

typ :: p -> String
typ _ = "typ"

fromSdp :: String -> Candidate
fromSdp candidate = Candidate (head c) (read (c !! 1):: Int) (c !! 2) (read (c !! 3)::Int) (c !! 4) (read (c !! 5)::Int) (last c)
  where c = words candidate

