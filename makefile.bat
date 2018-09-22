windres -O coff rsa-tool.rc rsa-tool.res
ghc --make rsa-tool.hs rsa-tool.res -o ../rsa-tool -O