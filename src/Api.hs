
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Api where
import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Servant  
import qualified Db as D
import Database.Esqueleto (Entity(entityVal))
import Types

type MovieGetAll        =  Get '[JSON] [Movie]
type MovieGetByName     =  Capture "name" String :> Get '[JSON] [Movie]
type MoviePost          =  ReqBody '[JSON] Movie :> Post '[JSON] Movie
type MovieUpdate        = "update" :> ReqBody '[JSON] Movie :> Put '[JSON] ()
type MovieDelete        =   "delete" :>Capture "name" String :> Delete '[JSON] ()
type MovieApi           = MovieGetAll  :<|> MovieGetByName :<|> MoviePost :<|> MovieUpdate :<|> MovieDelete

----Use
type UserRegister       =  "register" :> ReqBody '[JSON] User :> Post '[JSON] User
type UserLogin          =  "login"    :> QueryParam "email" String :> QueryParam "password" String :> Get '[JSON] String
type AddFavouriteMovie  =  Capture "email" String :> Capture "movieName" String :> Post '[JSON, PlainText] String
type UserApi            = UserRegister :<|> UserLogin :<|> AddFavouriteMovie


serverMovie :: Server MovieApi
serverMovie = getAllMovies :<|> getMovieByName :<|> postMovie :<|> updateMovieByName :<|> deleteMovieByName
          where 
                getAllMovies :: Handler [Movie]
                getAllMovies = do
                               map (\ b -> Movie {mname = D.movieMname $ entityVal b,rating = D.movieRating $ entityVal b,genre = D.movieGenre $ entityVal b}) <$> liftIO D.queryfetchAllMovies
                getMovieByName ::  String -> Handler [Movie]
                getMovieByName movieName = do
                                           x <- liftIO $ D.queryfetchOneByName movieName
                                           return $ map (\ b -> Movie {mname = D.movieMname $ entityVal b,rating = D.movieRating $ entityVal b,genre = D.movieGenre $ entityVal b}) x
                postMovie ::  Movie -> Handler Movie
                postMovie movie = do
                                  x <-  liftIO $ D.queryinsertMovie movie
                                  case x of
                                    Just _ -> return movie
                                    Nothing -> throwError err400

                updateMovieByName :: Movie -> Handler ()
                updateMovieByName movie = do
                                            liftIO $ D.queryupdateMovie movie
                deleteMovieByName :: String ->  Handler ()
                deleteMovieByName movieName = do
                                               liftIO $ D.querydeleteMovie movieName


serverUser :: Server UserApi
serverUser = registerUser :<|> loginUser :<|> addFavMovie
        where
          registerUser :: User -> Handler User
          registerUser user = do
                            x <- liftIO $ D.queryinsertUser user
                            case x of
                              Just _ -> return user
                              Nothing -> throwError err400
          loginUser :: Maybe String -> Maybe String ->  Handler String
          loginUser email password = do
                                      case email of
                                        Just emailx -> case password of
                                                         Just passwordx -> do
                                                          x <- liftIO $ D.querygetUserByEmail emailx
                                                          case x of
                                                            Just user -> if D.usersPassword (entityVal user) == passwordx then return $  D.usersEmail (entityVal user) ++ ":" ++ D.usersPassword (entityVal user) else throwError err401
                                                            Nothing -> throwError err400
                                                         Nothing -> throwError err400
                                        Nothing -> throwError err400

          addFavMovie :: String -> String -> Handler String
          addFavMovie email movie = do
                                k <- liftIO $ D.querycheckIfMovieExists movie
                                if k
                                then  do
                                    user <- liftIO $ D.querygetUserByEmail email
                                    case user of
                                        Just userdb -> do 
                                            liftIO $ D.queryupdateUserFavMovie (((entityVal userdb){D.usersFavouriteMovie = movie : D.usersFavouriteMovie (entityVal userdb) }))
                                            pure $ "Movie Added Successfully"
                                        Nothing -> throwError err400
                                else throwError $ err400 

                                   



type CombinedAPI = "user" :> UserApi
              :<|> ("movie" :> MovieApi)

server10 :: Server CombinedAPI
server10 = serverUser :<|> serverMovie

combinedProxy :: Proxy CombinedAPI
combinedProxy = Proxy