import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withCms)

main :: IO ()
main = defaultMain fromArgs withCms
