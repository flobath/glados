module Disassembly (disassemble, disassembleFile) where
import StackMachine (StackProgram)

disassembleFile :: (FilePath, StackProgram) -> String
disassembleFile (file, stack) = "\n"
    ++ file ++ ":\n"
    ++ unlines (map (("  " ++) . show) stack)

disassemble :: StackProgram -> String
disassemble = unlines . map show
