{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Control.Monad.Logger (LogSource)
import Data.Map.Strict qualified as Map
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation hiding (requestHeaders)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth.OAuth2.MyTwitch

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Encrypt (EncryptionFailure (UnableToEncrypt))
import Data.Text.Conversions
import Data.Text.Encoding qualified as T
import Encryption (encryptText, runEncryptM)
import Yesod.Auth.Message
import Yesod.Core.Types (Logger)
import Yesod.Core.Unsafe qualified as Unsafe
import Yesod.EmbeddedStatic (EmbeddedStatic, embedStaticContent)

{- | The foundation datatype for your application. This can be a good place to
 keep settings and values requiring initialization before your application
 starts running, such as database connections. Every handler will have
 access to the data present here.
-}
data App = App
  { appSettings :: AppSettings
  , -- | Settings for static file serving.
    appStatic :: EmbeddedStatic
  , -- | Database connection pool.
    appConnPool :: ConnectionPool
  , appHttpManager :: Manager
  , appLogger :: Logger
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text
  , menuItemRoute :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a =
  forall (m :: * -> *).
  (MonadUnliftIO m) =>
  ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  errorHandler :: ErrorResponse -> Handler TypedContent
  errorHandler = \case
    InternalError e -> do
      $logErrorS "yesod-core" e
      selectRep . provideRep $ pure ("Internal Server Error" :: Text)
    e -> defaultErrorHandler e

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> envClientSessionBackend
        180 -- 3 hours in minutes
        "YESOD_CLIENT_SESSION_AES_KEY"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute

    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    (title, parents) <- breadcrumbs

    -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft $
              MenuItem
                { menuItemLabel = "Home"
                , menuItemRoute = HomeR
                , menuItemAccessCallback = True
                }
          , NavbarLeft $
              MenuItem
                { menuItemLabel = "Profile"
                , menuItemRoute = ProfileR
                , menuItemAccessCallback = isJust muser
                }
          , NavbarRight $
              MenuItem
                { menuItemLabel = "Login"
                , menuItemRoute = AuthR LoginR
                , menuItemAccessCallback = isNothing muser
                }
          , NavbarRight $
              MenuItem
                { menuItemLabel = "Logout"
                , menuItemRoute = AuthR LogoutR
                , menuItemAccessCallback = isJust muser
                }
          ]

    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute ::
    App ->
    Maybe (Route App)
  authRoute _ = Just $ AuthR LoginR

  isAuthorized ::
    -- | The route the user is visiting.
    Route App ->
    -- | Whether or not this is a "write" request.
    Bool ->
    Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = pure Authorized
  isAuthorized HomeR _ = pure Authorized
  isAuthorized OverlayR _ = pure Authorized
  isAuthorized FaviconR _ = pure Authorized
  isAuthorized RobotsR _ = pure Authorized
  isAuthorized (StaticR _) _ = pure Authorized
  isAuthorized ServerSentEventsR _ = pure Authorized
  -- See https://dev.twitch.tv/docs/eventsub/handling-webhook-events#verifying-the-event-message
  isAuthorized TwitchWebhookR _ = pure Authorized
  -- the profile route requires that the user is authenticated, so we
  -- delegate to that function
  isAuthorized ProfileR _ = isAuthenticated
  isAuthorized FollowersR _ = isAuthenticated
  isAuthorized SubscribersR _ = isAuthenticated
  -- Admin actions
  isAuthorized (AdminReplayWebhookR _) _ = isAuthenticatedAdmin
  -- Admin pages
  isAuthorized AdminWebhooksR _ = isAuthenticatedAdmin
  isAuthorized AdminWebhooksSubscribeFollowR _ = isAuthenticatedAdmin
  isAuthorized AdminWebhooksSubscribeSubscribeR _ = isAuthenticatedAdmin
  isAuthorized AdminWebhooksSubscribeCheerR _ = isAuthenticatedAdmin
  isAuthorized AdminWebhooksSubscribeRaidR _ = isAuthenticatedAdmin

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    -- | The file extension
    Text ->
    -- | The MIME content type
    Text ->
    -- | The contents of the file
    LByteString ->
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent = do
    embedStaticContent appStatic StaticR minifym

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    pure $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = pure . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  -- Takes the route that the user is currently on, and returns a tuple
  -- of the 'Text' that you want the label to display, and a previous
  -- breadcrumb route.
  breadcrumb ::
    -- | The route the user is visiting currently.
    Route App ->
    Handler (Text, Maybe (Route App))
  breadcrumb HomeR = pure ("Home", Nothing)
  breadcrumb (AuthR _) = pure ("Login", Just HomeR)
  breadcrumb ProfileR = pure ("Profile", Just HomeR)
  breadcrumb _ = pure ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = TwitchUserId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = HomeR

  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR

  -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True

  authenticate ::
    (MonadHandler m, HandlerSite m ~ App) =>
    Creds App ->
    m (AuthenticationResult App)
  authenticate Creds {..} = do
    -- TODO: Move some of this logic out of 'authenticate' so it is testable
    AppSettings {..} <- getsYesod appSettings
    if appAuthDummyLogin && credsPlugin == "dummy"
      then do
        twitchUserId <-
          liftHandler . runDB $
            insert TwitchUser {twitchUserIdent = credsIdent, twitchUserLogin = "dummy"}
        pure $ Authenticated twitchUserId
      else do
        let TwitchSettings {..} = appTwitchSettings
            credsExtraMap = Map.fromList credsExtra
            mUserResponseBS :: Maybe LBS.ByteString
            mUserResponseBS = unUTF8 . fromText <$> Map.lookup "userResponse" credsExtraMap
            mUserResponse = join $ decode @UserResponse <$> mUserResponseBS
            mTwitchUserLogin = Map.lookup "login" credsExtraMap

        eAccessTokenEncrypted <-
          case Map.lookup "accessToken" credsExtraMap of
            Nothing -> pure . Left $ UnableToEncrypt "No access token"
            Just accessToken ->
              liftIO $ runEncryptM appEncryptionSettings $ encryptText accessToken
        eRefreshTokenEncrypted <-
          case Map.lookup "refreshToken" credsExtraMap of
            Nothing -> pure . Left $ UnableToEncrypt "No refresh token"
            Just accessToken ->
              liftIO $ runEncryptM appEncryptionSettings $ encryptText accessToken
        case (eAccessTokenEncrypted, eRefreshTokenEncrypted) of
          (Right accessTokenEncrypted, Right refreshTokenEncrypted) -> liftHandler $ do
            -- Determine if user already exists in the database and build the
            -- 'TwitchCredentials' record if possible
            mTwitchUserIdent <- runDB $ getBy $ UniqueTwitchUser credsIdent
            let twitchCredentials userId =
                  TwitchCredentials
                    { twitchCredentialsAccessToken = accessTokenEncrypted
                    , twitchCredentialsRefreshToken = refreshTokenEncrypted
                    , twitchCredentialsTwitchUserId = userId
                    }
            -- 'mUserResponse' contains scopes, only streamer should have elevated scopes
            case mUserResponse of
              Nothing -> pure $ ServerError "Unable to decode user response from Twitch"
              Just UserResponse {..} -> do
                if credsIdent /= twitchSettingsStreamerId && userResponseScopes /= ["user:read:email"]
                  then pure . UserError $ IdentifierNotFound "Log in as user"
                  else case mTwitchUserIdent of
                    -- If the user exists,
                    -- - update the Twitch login name
                    -- - update the Twitch credentials
                    Just (Entity uid _) -> do
                      runDB . void $ do
                        forM_ mTwitchUserLogin $ \twitchUserLogin ->
                          void $ updateGet uid [TwitchUserLogin =. twitchUserLogin]
                        upsert
                          (twitchCredentials uid)
                          [ TwitchCredentialsAccessToken =. accessTokenEncrypted
                          , TwitchCredentialsRefreshToken =. refreshTokenEncrypted
                          ]
                      pure $ Authenticated uid
                    -- If the user doesn't exist,
                    -- - insert the 'TwitchUser' entity
                    -- - insert the 'TwitchCredential' entity
                    Nothing -> do
                      case mTwitchUserLogin of
                        Nothing -> pure $ ServerError "Unable to determine Twitch login handle"
                        Just twitchUserLogin -> runDB $ do
                          twitchUserId <-
                            insert
                              TwitchUser
                                { twitchUserIdent = credsIdent
                                , twitchUserLogin = twitchUserLogin
                                }
                          insert_ (twitchCredentials twitchUserId)
                          pure $ Authenticated twitchUserId
          _ -> pure $ ServerError "Unable to encrypt tokens"

  -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins app =
    -- TODO: Build out our API before allowing users to log in
    -- [ oauth2TwitchScoped
    --     "Login with Twitch as user (this is you)"
    --     "twitch-user"
    --     ["user:read:email"]
    --     twitchSettingsClientId
    --     twitchSettingsClientSecret
    oauth2TwitchScoped
      "Login via Twitch as streamer (this is not you)"
      "twitch-streamer"
      ["user:read:email", "channel:read:subscriptions", "bits:read"]
      twitchSettingsClientId
      twitchSettingsClientSecret :
    extraAuthPlugins
    where
      TwitchSettings {..} = appTwitchSettings $ appSettings app
      extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  pure $ case muid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _ -> Authorized

-- | Access function to determine if a user is logged in.
isAuthenticatedAdmin :: Handler AuthResult
isAuthenticatedAdmin = do
  TwitchSettings {..} <- appTwitchSettings <$> getsYesod appSettings
  mEntity <- maybeAuth
  pure $ case mEntity of
    Nothing -> Unauthorized "You must login to access this page"
    Just (Entity _ TwitchUser {..}) ->
      if twitchUserIdent == twitchSettingsStreamerId
        then Authorized
        else Unauthorized "Only the streamer can use this endpoint"

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
