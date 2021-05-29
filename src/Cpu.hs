{-# LANGUAGE LambdaCase #-}
module Cpu(RegisterType(..), Register(..), Cpu(..), armCortexM0, getRegistersByType) where



    data RegisterType = GeneralPurpose
        | Data
        | StackPointer
        | ProgramCounter
        | LinkRegister

    data Register = Register{
        registerIndex :: Int,
        registerName :: String,
        size :: Int,
        registerType :: RegisterType
    }

    data Cpu = Cpu{
        cpuName :: String,
        dataSize :: Int,
        addrSize :: Int,
        registers :: [ Register ]
    }


    armCortexM0 :: Cpu
    armCortexM0 = Cpu{
        cpuName="Arm CORTEX-M0",
        dataSize = 32,
        addrSize = 32,
        registers = [
            Register { registerIndex=0,  registerName="R0",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=1,  registerName="R1",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=2,  registerName="R2",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=3,  registerName="R3",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=4,  registerName="R4",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=5,  registerName="R5",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=6,  registerName="R6",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=7,  registerName="R7",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=8,  registerName="R8",  size=32, registerType=Data },
            Register { registerIndex=9,  registerName="R9",  size=32, registerType=Data },
            Register { registerIndex=10, registerName="R10", size=32, registerType=Data },
            Register { registerIndex=11, registerName="R11", size=32, registerType=Data },
            Register { registerIndex=12, registerName="R12", size=32, registerType=Data },
            Register { registerIndex=13, registerName="SP",  size=32, registerType=StackPointer },
            Register { registerIndex=14, registerName="LR",  size=32, registerType=ProgramCounter },
            Register { registerIndex=15, registerName="PC",  size=32, registerType=LinkRegister }
        ]
    }

    getRegistersByType :: RegisterType -> Cpu -> [Register]
    getRegistersByType regType Cpu{registers=r} = filter (\case
                Register{registerType=regType} -> True
                _ -> False) r

    
    countRegisters ::  RegisterType -> Cpu -> Int
    countRegisters regType cpu = length (getRegistersByType regType cpu)

    
    



    



    -- countRegisters :: Cpu -> Int
    -- countRegisters Cpu{registers=r} = length filter (\case Register{registerType=GeneralPurpose}) r








