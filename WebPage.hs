-- |Web interface to control autobuilder.
module Main where

import		 Debian.Control
import		 Debian.Repo
import		 Debian.URI

import qualified Debian.Config as Config
import		 Control.Monad
import		 Control.Monad.Trans(liftIO)	-- required despite warning
import		 Data.List
import qualified Data.Map as Map
import		 Data.Maybe
import		 Extra.TIO
import qualified MyHtml
import qualified Network.CGI as CGI
import qualified Debian.AutoBuilder.Params as Params
import		 System.Directory
import		 System.Environment
import		 System.IO
import		 Text.Regex
import		 Text.XHtml.Transitional

main :: IO ()
main = CGI.runCGI (CGI.handleErrors $ cgiMain)

cgiMain :: CGI.CGI CGI.CGIResult
cgiMain = 
    do
      name <- CGI.scriptName
      inputs <- CGI.getInputs
      return (("SCRIPT_NAME", name) : inputs) >>= liftIO . runTIO defStyle . run aptIOStyle . application >>= CGI.output

application :: [(String,String)] -> AptIO String
application cgivars =
    do -- Use the same application name as the autobuilder so we
       -- see the same configuration files.
      args <- io getArgs
      let verbosity = length (filter (== "-v") args)
      let flags = Config.Value "Use" "web-config" : Config.seedFlags appName Params.optSpecs args
      params <- Params.params verbosity "autobuilder" flags >>= return . head
      html <-
          case lookup "page" cgivars of
            Nothing -> io $ topPage params cgivars
            Just "params" -> io $ flagPage params
            Just "env" -> io $ envPage
            Just "dist" -> distPage params cgivars
            Just "source-package" -> sourcePackagePage params cgivars
            Just "binary-package" -> io $ binaryPackagePage params cgivars
            Just "apt-get-update" -> io $ aptGetUpdate params cgivars
            Just page -> io $ errorPage params cgivars page
      return $ show $ concatHtml [heading params cgivars, html]

appName :: String
appName = "autobuilder"

aptGetUpdate :: Params.Params -> [(String,String)] -> IO Html
aptGetUpdate params cgivars =
    do
      let dist = parseReleaseName . fromJust . lookup "dist" $ cgivars
      let dir = Params.cleanRootOfRelease params dist
      let cmd = "sudo chroot " ++ rootPath dir ++ " apt-get update"
      -- Can't do this until we convince Apache it is ok
      -- (output, code) <- My.processOutput cmd
      -- return $ pre (stringToHtml (cmd ++ "\n" ++ output ++ "\n" ++ show code))
      return $ stringToHtml cmd

heading :: Params.Params -> [(String,String)] -> Html
heading params cgivars =
    let nav = td (concatHtml topNav) in
    let info = td (concatHtml [stringToHtml "Upload Host: ", stringToHtml (maybe "None" id (Params.uploadHost params))])
               ! [strAttr "align" "right"] in
    (table . tr . concatHtml) [nav, info] ! [strAttr "width" "100%"]
    where
      topNav = [MyHtml.linkTo cgivars (stringToHtml "Top") []] ++ pageNav
      pageNav =
          case lookup "page" cgivars of
            Just "dist" ->
                case lookup "dist" cgivars of
                  Nothing -> []
                  Just dist ->
                      [stringToHtml " > ", linkToDist cgivars dist]
            _ -> []

topPage :: Params.Params -> [(String,String)] -> IO Html
topPage params cgivars =
    do dists <- runTIO defStyle (run aptIOStyle (distros params)) >>= return . map (sliceName . sliceListName)
       return (concatHtml
               [h3 (stringToHtml "Dists"),
                ulist (concatHtml (map (li . linkToDist cgivars . show) dists)),
	        h3 (MyHtml.linkTo cgivars (stringToHtml "Parameters") [("page", "params")]),
	        h3 (MyHtml.linkTo cgivars (stringToHtml "Environment") [("page", "env")])])

flagPage :: Params.Params -> IO Html
flagPage params =
    return (concatHtml 
            [h3 (stringToHtml "Parameters"),
             -- FIXME: format as html, replace newlines with <br>, etc
             ulist (concatHtml (map (li . stringToHtml . show) (Map.assocs (Params.flags params))))])

envPage :: IO Html
envPage =
    do
      env <- getEnvironment
      return (concatHtml 
              [h3 (stringToHtml "Environment"),
               -- FIXME: format as html, replace newlines with <br>, etc
               ulist (concatHtml (map (li . stringToHtml . show) env))])

{-
environment :: IO Html
environment =
    do
      env <- getEnvironment
      return $ concatHtml (map (\ (name, value) -> concatHtml [stringToHtml (name ++ "=" ++ value), br]) env)
-}

distPage :: Params.Params -> [(String,String)] -> AptIO Html
distPage params cgivars =
    do distro <- distros params >>= return . find (isDist dist)
       case distro of
         Nothing ->
             error ("Unknown dist: " ++ sliceName dist)
         Just distro ->
             do (Control sourcePackages) <- sourcePackageInfo params root (Params.uploadHost params) distro
                return (form
                        (concatHtml
                         [input ! [strAttr "type" "submit",  strAttr "name" "page", strAttr "value" "apt-get-update"],
                          hidden "dist" (sliceName dist),
                          h3 (center (stringToHtml ("ReleaseName " ++ sliceName dist ++ " source packages"))),
		          sourcePackageTable sourcePackages,
                          h3 (stringToHtml ("ReleaseName " ++ sliceName dist ++ " - Sources")),
                          ulist (concatHtml (map (li . showSource params) (map sliceSource (slices . sliceList $ distro)))),
		          h3 (stringToHtml "Binary Package Lists"),
                          ulist (concatHtml (map (li . stringToHtml . ((rootPath root) ++))
                                             (concat (map (archFiles (Binary "i386"))
                                                      (map sliceSource (slices . sliceList $ distro)))))),
		          h3 (stringToHtml "Source Package Lists"),
                          ulist (concatHtml (map (li . stringToHtml . ((rootPath root) ++))
                                             (concat (map (archFiles Source)
                                                      (map sliceSource (slices . sliceList $ distro))))))]))
    where
      sourcePackageTable packages =
          let rows = map (tr . sourcePackageHtml) (sortBy cmpPackages packages) in
          table (concatHtml (tr (concatHtml (map (th . stringToHtml) ["name", "version", "binary"])) :
                             rows)) ! MyHtml.thinborder
      sourcePackageHtml info =
          concatHtml [(td . linkToSourcePackage . fromJust) (fieldValue "Package" info),
                      (td . stringToHtml . fromJust) (fieldValue "Version" info),
                      (td . concatHtml . intersperse br . map linkToBinaryPackage . splitCommaList . fromJust) (fieldValue "Binary" info)]
      linkToSourcePackage name =
          MyHtml.linkTo cgivars (stringToHtml name) [("page", "source-package"), ("package", name), ("dist", sliceName dist)]
      linkToBinaryPackage name =
          MyHtml.linkTo cgivars (stringToHtml name) [("page", "binary-package"), ("package", name), ("dist", sliceName dist)]
      splitCommaList s = splitRegex (mkRegex "[, ]+") s
      cmpPackages a b = compare (fieldValue "Package" a) (fieldValue "Package" b)
      dist = maybe (error "No dist name") SliceName (lookup "dist" cgivars)
      root = cacheRootDir (Params.topDir params) (either (error . show) id (Params.findSlice params dist))
      isDist dist distro = dist == sliceListName distro

sourcePackagePage :: Params.Params -> [(String, String)] -> AptIO Html
sourcePackagePage params cgivars =
    do
      let package = fromJust (lookup "package" cgivars)
      let dist = maybe (error "No dist name") SliceName (lookup "dist" cgivars)
      let distro = either (error . show) id (Params.findSlice params dist)
      let root = cacheRootDir (Params.topDir params) distro
      (Control control) <- sourcePackageInfo params root (Params.uploadHost params) distro
      let (Paragraph info) = fromJust (find (\ info -> fieldValue "Package" info == Just package) control)
      return $ concatHtml (intersperse br (map (stringToHtml . show) info))

binaryPackagePage :: Params.Params -> [(String, String)] -> IO Html
binaryPackagePage _ cgivars =
    let package = fromJust (lookup "package" cgivars) in
    return $ stringToHtml ("binaryPackagePage: " ++ package)

sourcePackageInfo :: Params.Params -> EnvRoot -> (Maybe String) -> NamedSliceList -> AptIO Control
sourcePackageInfo _ root uploadHost distro =
    do
      tio (vPutStrBl 0 ("sourcePackageFiles: " ++ show sourcePackageFiles))
      filterM (io . doesFileExist) sourcePackageFiles >>=
              mapM (io . parseControlFromFile) >>=
              return . map (either (\ e -> error (show e)) id) >>=
              return . mergeControls
    where
      sourcePackageFiles = map ((rootPath root) ++) $ concat (map (archFiles Source) (map sliceSource . slices $ sourceSources))
      sourceSources = sourceSlices uploadSources
      uploadSources = releaseSlices dist (sliceList distro)
      dist = parseReleaseName (sliceName . sliceListName $ distro)

showSource :: Params.Params -> DebSource -> Html
showSource params src@(DebSource DebSrc uri _)
    | uriHost uri == (Params.uploadHost params) =
        concatHtml [stringToHtml "uploadable source: ", (stringToHtml . show) src]
showSource params src@(DebSource Deb uri _)
    | uriHost uri == (Params.uploadHost params) =
        concatHtml [stringToHtml "uploadable binary: ", (stringToHtml . show) src]
showSource _ src = (stringToHtml . show) src

errorPage :: Params.Params -> [(String,String)] -> String -> IO Html
errorPage _ _ page =
    return (concatHtml
            [h3 (stringToHtml ("Unknown page type: " ++ page))])

distros :: Params.Params -> AptIO [NamedSliceList]
distros params = mapM parseNamedSliceList' (Params.sources params)

linkToDist :: [(String,String)] -> String -> Html
linkToDist cgivars dist =
    MyHtml.linkTo cgivars (stringToHtml dist) [("page", "dist"), ("dist", dist)]

uriHost :: URI -> Maybe String
uriHost uri = maybe Nothing (Just . uriRegName) (uriAuthority uri)
