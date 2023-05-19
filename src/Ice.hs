module Ice (test, getHostAddresses) where

import Network.Info
import Network.Socket
import Numeric
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5

test = do
  a <- getHostAddresses
  a <- getComponentCandidates a
  print a

-- TODO Add ipv6
getHostAddresses :: IO [String]
getHostAddresses = do
  map show . filter (\x -> notElem x $ map IPv4 [0, 0x0100007f]) . map ipv4 <$> getNetworkInterfaces

getComponentCandidates addresses= do
  addrinfos <- mapM (\x -> getAddrInfo Nothing (Just x) (Just "0")) addresses
  return addrinfos

data Candidate = Candidate {foundation :: String, component::Int,transport::String, priority::Int, host::String, port::Int, type'::String}

candidateFoundation type' transport baseAddress = md5 $ LB.fromStrict $ C8.pack $ type' ++ transport ++ baseAddress 


