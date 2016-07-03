{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception.Base (assert)
import Control.Monad
import Data.Default (def)
import Data.String (fromString)
import Data.Maybe (isNothing)
import Database.MySQL.Simple
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.MySQL

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main :: IO ()
main = do
    conn <- dbconnect
    conn' <- fromSimpleConnection conn
    store <- dbStore conn' testSettings
    purger conn' testSettings

    ((lookupSess1, insertSess1), mknewsessid) <- store Nothing
    sessid <- mknewsessid

    insertSess1 ("foo" :: B.ByteString) ("bar" :: B.ByteString)

    l1 <- lookupSess1 "foo"
    assert (l1 == Just "bar") it

    l2 <- lookupSess1 "bar"
    assert (isNothing l2) it

    ((lookupSess2, insertSess2), mknewsessid) <- store $ Just sessid
    newsessid <- mknewsessid

    l3 <- lookupSess2 "foo"
    assert (l3 == Just "bar") it

    assert (newsessid == sessid) it

    let invalidsessid = "foobar"
    ((lookupSess3, insertSess3), mknewsessid) <- store $ Just invalidsessid
    newsessid2 <- mknewsessid

    assert (newsessid2 /= newsessid) it
    assert (newsessid2 /= invalidsessid) it

    l4 <- lookupSess3 "foo"
    assert (isNothing l4) it

    ((lookupSess4, insertSess4), mknewsessid) <- store $ Just sessid
    l5 <- lookupSess4 "foo"
    assert (l5 == Just "bar") it

    threadDelay 6000000

    ((lookupSess5, insertSess5), mknewsessid) <- store $ Just sessid
    l6 <- lookupSess5 "foo"
    assert (isNothing l6) it

it :: IO ()
it = return ()

dbconnect :: IO Connection
dbconnect = connect defaultConnectInfo { connectDatabase = "test" }

testSettings :: StoreSettings
testSettings = def { storeSettingsSessionTimeout=5 }
