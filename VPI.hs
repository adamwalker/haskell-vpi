{-# LANGUAGE RecordWildCards #-}
module VPI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Functor
import Data.Int
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Data.Bool
import Numeric

--Task registration

data SysTFType
    = SysTask
    | SysFunc 

sysTFTypeToInt32 SysTask = 1
sysTFTypeToInt32 SysFunc = 2

data VPISystfData = VPISystfData {
    typ         :: SysTFType,
    sysFuncType :: Int32,
    tfName      :: String,
    callTF      :: Ptr CUChar -> IO CInt,
    compileTF   :: Maybe (Ptr CUChar -> IO CInt),
    sizeTF      :: Maybe (Ptr CUChar -> IO CInt),
    userData    :: Ptr CUChar
}

foreign import ccall "wrapper" 
    tfWrap :: (Ptr CUChar -> IO CInt) -> IO (FunPtr (Ptr CUChar -> IO CInt))

instance Storable VPISystfData where
    sizeOf    _                    = 48
    alignment _                    = alignment (0 :: Float)
    peek                           = undefined
    poke      ptr VPISystfData{..} = do

        poke (ptr `plusPtr` 0) (sysTFTypeToInt32 typ :: CInt)
        poke (ptr `plusPtr` 4) (fromIntegral sysFuncType :: CInt)

        tfNamePtr <- newCString tfName
        poke (ptr `plusPtr` 8) tfNamePtr

        callTFPtr <- tfWrap callTF
        poke (ptr `plusPtr` 16) callTFPtr

        compileTFPtr <- maybe (return nullFunPtr) tfWrap compileTF
        poke (ptr `plusPtr` 24) compileTFPtr

        sizeTFPtr <- maybe (return nullFunPtr) tfWrap sizeTF
        poke (ptr `plusPtr` 32) sizeTFPtr

        poke (ptr `plusPtr` 40) userData


data CVPIHandle
newtype VPIHandle = VPIHandle (Ptr CVPIHandle)

nullVPIHandle = VPIHandle nullPtr

foreign import ccall safe "vpi_register_systf"
    c_vpiRegisterTF :: Ptr VPISystfData -> IO (Ptr CVPIHandle)

registerTF :: VPISystfData -> IO VPIHandle
registerTF tf = alloca $ \ptr -> do
    poke ptr tf
    VPIHandle <$> c_vpiRegisterTF ptr

--Object handling

data ObjectCode
    = RegObject
    | ModuleObject

objCodeToInt32 RegObject    = 48
objCodeToInt32 ModuleObject = 32

nothingOnNull :: Ptr a -> Maybe (Ptr a)
nothingOnNull ptr 
    | ptr == nullPtr = Nothing
    | otherwise      = Just ptr

foreign import ccall safe "vpi_iterate"
    c_vpiIterate :: CInt -> Ptr CVPIHandle -> IO (Ptr CVPIHandle)

vpiIterate :: ObjectCode -> VPIHandle -> IO (Maybe VPIHandle)
vpiIterate typ (VPIHandle hdl) = fmap VPIHandle <$> nothingOnNull <$> (c_vpiIterate (objCodeToInt32 typ) hdl)

foreign import ccall safe "vpi_scan"
    c_vpiScan :: Ptr CVPIHandle -> IO (Ptr CVPIHandle)

vpiScan :: VPIHandle -> IO (Maybe VPIHandle)
vpiScan (VPIHandle hdl) = fmap VPIHandle <$> nothingOnNull <$> c_vpiScan hdl

foreign import ccall safe "vpi_get_str"
    c_vpiGetString :: CInt -> Ptr CVPIHandle -> IO CString

data Property
    = NameProperty

propertyToInt32 NameProperty = 2

vpiGetString :: Property -> VPIHandle -> IO String
vpiGetString prop (VPIHandle ref) = do
    res <- c_vpiGetString (propertyToInt32 prop) ref
    peekCString res

--VPI values
data VPIValue
    = VPIInteger   CInt
    | VPIHexString String
    | VPIBinString String

instance Storable VPIValue where
    sizeOf    _                  = 12
    alignment _                  = alignment (0 :: Float)
    peek                         = undefined

    poke      ptr (VPIInteger x) = do
        poke (ptr `plusPtr` 0) (6 :: CInt)
        poke (ptr `plusPtr` 8) x

    poke      ptr (VPIHexString x) = do
        str <- newCString x --TODO: free it
        poke (ptr `plusPtr` 0) (4 :: CInt)
        poke (ptr `plusPtr` 8) str

    poke      ptr (VPIBinString x) = do
        str <- newCString x --TODO: free it
        poke (ptr `plusPtr` 0) (1 :: CInt)
        poke (ptr `plusPtr` 8) str

foreign import ccall safe "vpi_put_value"
    c_vpiPutValue :: VPIHandle -> Ptr VPIValue -> Ptr () -> CInt -> IO VPIHandle

data DelayMode
    = NoDelay

delayModeToInt32 NoDelay = 1

vpiPutValue :: VPIHandle -> VPIValue -> IO VPIHandle
vpiPutValue hdl value = alloca $ \ptr -> do
    poke ptr value
    c_vpiPutValue hdl ptr nullPtr (delayModeToInt32 NoDelay)

--Higher level helpers

getSignalHandle :: VPIHandle -> String -> IO (Maybe VPIHandle)
getSignalHandle modH nm = do
    Just registerIterator <- vpiIterate RegObject modH
    runMaybeT $ findIt registerIterator
    where
    findIt :: VPIHandle -> MaybeT IO VPIHandle
    findIt registerIterator = do
        regHandle <- MaybeT $ vpiScan registerIterator
        thisNm    <- lift   $ vpiGetString NameProperty regHandle
        case thisNm == nm of
            True  -> return regHandle
            False -> findIt registerIterator

boolValue :: Bool -> VPIValue
boolValue = VPIInteger . bool 0 1 

numericValue :: (Integral a, Show a) => a -> VPIValue
numericValue = VPIHexString . flip showHex ""

