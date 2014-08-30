import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)

-- (Connection number, Message)
type Msg = (Int, String)

-- There is an original master queue that stores all incoming and outgoing
-- messages. Each socket connection has its own duplicate queue. This is for
-- thread safety: if a connection pops off a message, that message should
-- still be available to other connections that have not read that message.

main :: IO ()
main = do
    msgQueue <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2

    -- Read original channel (prevents memory leak, otherwise
    -- the messages keep getting pushed onto the original queue
    -- and never get popped, since each connection only pops its
    -- own queue)
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan msgQueue
        loop

    mainLoop sock msgQueue 0

-- Loop that listens for new connections
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock msgQueue connectionNumber = do
    connection <- accept sock
    putStrLn("New connection: " ++ show(snd(connection)))

    -- New thread running each socket connection
    forkIO (runConn connection msgQueue connectionNumber)

    mainLoop sock msgQueue $! connectionNumber+1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) msgQueue connectionNumber = do
    -- Declare pushToChan function
    let pushToChan msg = writeChan msgQueue(connectionNumber, msg)

    -- Set up socket handle
    sHandle <- socketToHandle sock ReadWriteMode
    hSetBuffering sHandle NoBuffering

    -- Initial prompt to client
    hPutStrLn sHandle "> Enter your name."
    name <- hGetLine sHandle
    pushToChan ("> " ++ name ++ " entered the server.")
    hPutStrLn sHandle ("> Welcome, " ++ name ++ "! Enter your message.")

    -- Duplicate the queue that stores the messages. Pushes are stored for
    -- both, but pops only affects that particular queue.
    msgQueue' <- dupChan msgQueue

    -- Loop to read and show messages from the connection queue
    readerThread <- forkIO $ fix $ \loop -> do
        (connectionNumber', msg) <- readChan msgQueue'
        -- Only show messages when not self
        when (connectionNumber /= connectionNumber') $ hPutStrLn sHandle msg
        loop

    -- Concurrent loop to push client->server messages for this connection
    -- onto the queue
    --handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    fix $ \loop -> do
        -- Get message from client via socket handle
        msg <- hGetLine sHandle
        pushToChan ("> " ++ name ++ ": " ++ msg)
        loop

    -- Only executes when above loop is interrupted by an exception/broken pipe
    killThread readerThread
    pushToChan ("> " ++ name ++ " left.")
    hClose sHandle
