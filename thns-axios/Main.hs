import Servant.JS
import Web as Web

main :: IO ()
main = writeJSForAPI Web.crud (axios defAxiosOptions) "thns-server/static/thns-crud.js"