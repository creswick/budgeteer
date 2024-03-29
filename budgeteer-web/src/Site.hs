{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Monoid
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple (initPostgresAuth)
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application

import           API.Meta (Meta(..), metaInit)
import           API.Items (Items(..), itemsInit)

-- ------------------------------------------------------------------------------
-- -- | Render login form
-- handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
-- handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
--   where
--     errs = maybe mempty splice authError
--     splice err = "loginError" ## I.textSplice err


-- ------------------------------------------------------------------------------
-- -- | Handle login submit
-- handleLoginSubmit :: Handler App (AuthManager App) ()
-- handleLoginSubmit =
--     loginUser "login" "password" Nothing
--               (\_ -> handleLogin err) (redirect "/")
--   where
--     err = Just "Unknown user or password"


-- ------------------------------------------------------------------------------
-- -- | Logs out and redirects the user to the site index.
-- handleLogout :: Handler App (AuthManager App) ()
-- handleLogout = logout >> redirect "/"


-- ------------------------------------------------------------------------------
-- -- | Handle new user form submit
-- handleNewUser :: Handler App (AuthManager App) ()
-- handleNewUser = method GET handleForm <|> method POST handleFormSubmit
--   where
--     handleForm = render "new_user"
--     handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [--  ("/login",    with auth handleLoginSubmit)
          -- , ("/logout",   with auth handleLogout)
          -- , ("/new_user", with auth handleNewUser)
          ("",          serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    m <- nestSnaplet "meta" meta metaInit
    i <- nestSnaplet "items" items itemsInit

    addRoutes routes
    return $ App s a d m i

