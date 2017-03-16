import Distribution.Simple
import Distribution.Simple.Setup          ( BuildFlags           )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo       )
import Distribution.PackageDescription    ( PackageDescription
                                          , HookedBuildInfo
                                          , emptyHookedBuildInfo )
import System.Exit                        ( die                  )
import System.Process                     ( readProcess          )
import System.Directory                   ( doesFileExist        )

main = defaultMainWithHooks $ simpleUserHooks {preBuild = buildSassFiles}

buildSassFiles :: Args -> BuildFlags -> IO HookedBuildInfo
buildSassFiles _ _ = do
    let sassBuildFile = "makeSASS"
    foundSassBuild <- doesFileExist sassBuildFile
    if foundSassBuild
    then do
        result <- readProcess "sh" [sassBuildFile] ""
        putStr result
        return emptyHookedBuildInfo
    else die $ "Could not find SASS build script \"" ++ sassBuildFile ++ "\""
