module Disassembly (disassemble, disassembleFile) where
import StackMachine (StackProgram)
import Text.Printf (printf)
import Data.List.Index(indexed)

disassembleFile :: (FilePath, StackProgram) -> String
disassembleFile (file, stack) = "\n"
    ++ file ++ ":\n"
    ++ unlines (map showLine (indexed stack))
    where
        showLine (idx, instr) = printf "%-*i| %s" longest idx (show instr)
        longest = length $ show $ length stack

disassemble :: StackProgram -> String
disassemble = unlines . map show
