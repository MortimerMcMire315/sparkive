import Distribution.Simple
import Distribution.Simple.Setup          ( BuildFlags         )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo     )
import Distribution.PackageDescription    ( PackageDescription )
import System.Exit                        ( die                )
import System.Process                     ( readProcess        )
import System.Directory                   ( doesFileExist      )

main = defaultMainWithHooks $ simpleUserHooks {postBuild = buildSassFiles}

buildSassFiles :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
buildSassFiles _ _ _ _ = do
    let sassBuildFile = "makeSASS"
    foundSassBuild <- doesFileExist sassBuildFile
    if foundSassBuild
    then do
        result <- readProcess "sh" [sassBuildFile] ""
        putStr result
    else die $ "Could not find SASS build script \"" ++ sassBuildFile ++ "\""
