import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  -- Setup sockets, handles
  sock <- socket AF_INET Stream 0
  connect sock (SockAddrInet 4242 iNADDR_ANY)
  sHandle <- socketToHandle sock ReadWriteMode
  hSetBuffering sHandle NoBuffering

  -- Inbound messages
  forkIO $ fix $ \loop -> do
    msgIn <- hGetLine sHandle
    putStrLn(msgIn)
    loop

  -- Outbound messages
  fix $ \loop -> do
    msgOut <- getLine
    hPutStrLn sHandle (msgOut)
    loop

  hClose sHandle
