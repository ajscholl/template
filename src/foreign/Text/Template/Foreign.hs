module Text.Template.Foreign where

import Text.Template.Main (runTemplate)

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as BL

import Foreign
import Foreign.C

hsRunTemplate :: CString -> CString -> CString -> CString -> CString -> CString -> Ptr CInt -> IO CString
hsRunTemplate tmplFileP varFileP bsP luaP varP mJsonP resultP = do
    tmplFile <- if tmplFileP /= nullPtr then peekCString tmplFileP else pure "<memory>.tmpl"
    varFile  <- if varFileP /= nullPtr then peekCString varFileP else pure "<memory>.var"
    bs       <- if bsP /= nullPtr then BL.fromStrict <$> BS.unsafePackCString bsP else pure mempty
    lua      <- if luaP /= nullPtr then BS.unsafePackCString luaP else pure mempty
    var      <- if varP /= nullPtr then BL.fromStrict <$> BS.unsafePackCString varP else pure mempty
    mJson    <- if mJsonP /= nullPtr then Just . BL.fromStrict <$> BS.unsafePackCString mJsonP else pure Nothing
    result   <- runTemplate tmplFile varFile bs lua var mJson
    case result of
        Left err -> do
            when (resultP /= nullPtr) $ poke resultP (-1)
            newCString err
        Right str -> do
            when (resultP /= nullPtr) $ poke resultP 0
            ptr <- mallocBytes $ fromIntegral (BL.length str) + 1
            pokeCByteString (BL.toChunks str) ptr
            pure ptr
    where
        pokeCByteString :: [BS.ByteString] -> Ptr CChar -> IO ()
        pokeCByteString [] ptr = poke ptr 0
        pokeCByteString (x:xs) ptr = do
            BS.unsafeUseAsCStringLen x $ \ (xPtr, xLen) -> copyBytes ptr xPtr xLen
            pokeCByteString xs (ptr `plusPtr` BS.length x)

hsFreeTemplate :: CString -> IO ()
hsFreeTemplate p = when (p /= nullPtr) $ free p

foreign export ccall hsRunTemplate :: CString -> CString -> CString -> CString -> CString -> CString -> Ptr CInt -> IO CString

foreign export ccall hsFreeTemplate :: CString -> IO ()
