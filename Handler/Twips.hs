module Handler.Twips where

import Import hiding (on, (==.)) -- These conflict with similarly named functions from Esqueleto used below
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Database.Esqueleto


getTwipsR :: Handler Html
getTwipsR = do
  twips <- runDB twipsWithAuthors
  defaultLayout $ do
    setTitle "Twips"
    $(widgetFile "twips/index")


getNewTwipR :: Handler Html
getNewTwipR = do
  userId <- requireAuthId
  now <- liftIO getCurrentTime
  (formWidget, formEnctype) <- generateFormPost $ twipForm userId now
  defaultLayout $ do
    setTitle "New Twip"
    $(widgetFile "twips/new")


postTwipsR :: Handler Html
postTwipsR = do
  userId <- requireAuthId
  now <- liftIO getCurrentTime
  ((result, formWidget), formEnctype) <- runFormPost $ twipForm userId now
  case result of
    FormSuccess twip -> do
      twipId <- runDB $ insert twip
      setMessage "Twip created"
      redirect TwipsR
    _ -> defaultLayout $ do
        setTitle "New Twip"
        $(widgetFile "twips/new")



twipForm :: UserId -> UTCTime -> Form Twip
twipForm userId now = renderBootstrap3 BootstrapBasicForm $ Twip
  <$> pure userId
  <*> areq textField "Title" Nothing
  <*> areq textareaField "Body" Nothing
  <*> pure now


twipsWithAuthors :: MonadIO m => SqlPersistT m [(Entity Twip, Entity User)]
twipsWithAuthors = select $
  from $ \(twip `InnerJoin` author) -> do
    on $ twip ^. TwipAuthorId ==. author ^. UserId
    return $ (twip, author)
