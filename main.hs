{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network (listenOn, PortID(PortNumber))
import Network.Socket (accept, withSocketsDo, sClose) 
import Network.Socket.ByteString (sendAll)
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy.Char8 as LazyByte (toChunks)
import qualified Data.ByteString as WordByte (concat)
import qualified Data.Text.Lazy as LazyText (Text, length, pack, concat)
import Data.Text.Lazy.Encoding (encodeUtf8)

main = withSocketsDo $ do 
    sock <- listenOn (PortNumber 8080)
    loop sock

loop sock = do 
    (conn, _) <- accept sock
    forkIO $ body conn
    loop sock

  where -- "hh as = 66 kk"
    body c = do 
        let resp = okResp "hello world" --how would I actually read from Socket?
        sendAll c (WordByte.concat . LazyByte.toChunks $ toByteString resp)
        sClose c

data Response = TextResponse {
    version :: LazyText.Text,
    status :: Int,
    reason :: LazyText.Text,
    text :: LazyText.Text
} deriving (Show, Eq)

okResp text = TextResponse { 
    version = "1.1", status = 200, reason = "Ok", text = text }

toByteString (TextResponse {version = ver,
    status = stat,
    reason = reason,
    text = text}) = encodeUtf8 . LazyText.concat $ [
        "HTTP/", ver, LazyText.pack . show $ stat, reason, "\r\n",
        "Content-Length: ", LazyText.pack . show $ LazyText.length text, "\r\n",
        "\r\n",
        text]
