{- |
Module      : Kanbanned.SmokeSpec
Description : Smoke test — start, render, quit via PTY
-}
module Kanbanned.SmokeSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Exit (ExitCode (..))
import System.IO
    ( hClose
    , hFlush
    , hSetBinaryMode
    )
import System.Process
    ( CreateProcess (..)
    , StdStream (..)
    , createProcess
    , proc
    , waitForProcess
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Smoke test" $ do
    it "starts in a PTY and quits on 'q'" $ do
        exePath <- findExe
        let cp = ptyProc exePath
        (Just hIn, Just hOut, _mErr, ph) <-
            createProcess cp
        hSetBinaryMode hIn True
        hSetBinaryMode hOut True
        -- Start reading output in background
        outputAsync <-
            async $ LBS.hGetContents hOut
        -- Wait for initial render
        threadDelay 1_000_000
        -- Send 'q' to quit
        BS.hPut hIn "q"
        hFlush hIn
        -- Wait for exit
        exitCode <- waitForProcess ph
        output <- LBS.toStrict <$> wait outputAsync
        hClose hIn
        -- Should exit cleanly
        exitCode `shouldBe` ExitSuccess
        -- Output should contain alt screen sequence
        BS.isInfixOf "\ESC[?1049h" output
            `shouldBe` True

    it "navigates columns and quits" $ do
        exePath <- findExe
        let cp = ptyProc exePath
        (Just hIn, _mOut, _mErr, ph) <-
            createProcess cp
        hSetBinaryMode hIn True
        threadDelay 1_000_000
        -- Navigate: l (next col), h (prev col), q (quit)
        mapM_
            ( \c -> do
                BS.hPut hIn (BS.singleton c)
                hFlush hIn
                threadDelay 200_000
            )
            [0x6C, 0x68, 0x71] -- l, h, q
        exitCode <- waitForProcess ph
        hClose hIn
        exitCode `shouldBe` ExitSuccess

-- | Create a process wrapped in script(1) for PTY
ptyProc :: FilePath -> CreateProcess
ptyProc exe =
    ( proc
        "script"
        [ "-qc"
        , exe
        , "/dev/null"
        ]
    )
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env =
            Just
                [ ("TERM", "xterm-256color")
                , ("HOME", "/tmp/kanbanned-smoke")
                ,
                    ( "XDG_CONFIG_HOME"
                    , "/tmp/kanbanned-smoke-cfg"
                    )
                , ("COLUMNS", "80")
                , ("LINES", "24")
                ]
        }

-- | Find the built executable
findExe :: IO FilePath
findExe = do
    let cp =
            (proc "cabal" ["list-bin", "kanbanned"])
                { std_out = CreatePipe
                }
    (_, Just hOut, _, ph) <- createProcess cp
    output <- BS.hGetContents hOut
    _ <- waitForProcess ph
    let path =
            filter (/= '\n') $
                map (toEnum . fromIntegral) $
                    BS.unpack output
    pure path
