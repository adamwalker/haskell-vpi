{-# LANGUAGE RecordWildCards, ExistentialQuantification #-}
module VPITest where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Data.Word
import Control.Monad
import Data.Maybe

import Pipes

import VPI

setDataCompile :: IO (Word64 -> IO ())
setDataCompile = do
    res <- runMaybeT $ do
        iterate          <- MaybeT $ vpiIterate ModuleObject nullVPIHandle --get a module iterator
        modH             <- MaybeT $ getHandle iterate "main"
        registerIterator <- MaybeT $ vpiIterate RegObject modH
        dataH            <- MaybeT $ getHandle registerIterator "data"
        return $ \dat -> void $ vpiPutValue dataH (numericValue dat)

    return $ fromMaybe (error "set data compilation error") res

producer :: IO (Producer Word64 IO ())
producer = return $ forever $ do
    yield 0
    yield 1
    yield 2
    yield 3

printSigCompile :: IO (IO ())
printSigCompile = do
    res <- runMaybeT $ do
        iterate          <- MaybeT $ vpiIterate ModuleObject nullVPIHandle --get a module iterator
        modH             <- MaybeT $ getHandle iterate "main"
        registerIterator <- MaybeT $ vpiIterate RegObject modH
        dataH            <- MaybeT $ getHandle registerIterator "data"
        return $ do
            val <- getValueHex dataH
            putStrLn $ "Val is: " ++ show val
    return $ fromMaybe (error "print sigs compilation error") res

printSigCall :: IO () -> () -> IO ()
printSigCall doIt _ = doIt

doIt :: IO ()
doIt = do
    producer <- producer
    putStrLn "registering $setdata..."
    registerFunc "$setdata" (SysTF setDataCompile writePipe) producer
    putStrLn "done registering..."

    putStrLn "registering $printsigs..."
    registerFunc "$printsigs" (SysTF printSigCompile printSigCall) ()
    putStrLn "done registering..."

foreign export ccall doIt :: IO ()

