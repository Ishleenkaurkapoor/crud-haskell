{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
module Db where
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Reader
import Data.Time (Day (ModifiedJulianDay))
import qualified Database.Esqueleto as E
import Control.Monad.Logger
import qualified Types as T









share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Movie
      mname String 
      genre String
      rating Double
      Primary mname
      deriving Show

Users
      name  String
      age   Int
      email String
      registration_date Day
      password String
      favouriteMovie [String]
      Primary email
      deriving Show
|]

-- loadData :: MonadIO m => ReaderT SqlBackend m ()
-- loadData = do
--   insert_ $ Movie "Harry Potter" "Fantasy" 5.6
--   insert_ $ Users "Aayush" 22 "aayush.k@gn.com" (ModifiedJulianDay 100000) "abcd" []




fetchAllMovies :: SqlPersistM [Entity Movie]
fetchAllMovies = E.select $
                   E.from $ \movie -> do
                    return movie


fetchOneByName :: String -> SqlPersistM [Entity Movie]
fetchOneByName movieName =  E.select $
                             E.from $ \movie -> do
                              E.where_ (movie E.^. MovieMname E.==. E.val movieName )
                              E.limit 1
                              return movie

insertMovie :: MonadIO m => T.Movie ->ReaderT SqlBackend m (Maybe ())
insertMovie m = do
      insertUnique_ $ Movie (T.mname m) (T.genre m) (T.rating m)


updateMovie :: (BaseBackend backend ~ SqlBackend, MonadIO m,
 PersistQueryWrite backend) => T.Movie -> ReaderT backend m ()
updateMovie movie = updateWhere [MovieMname ==. T.mname movie] [MovieRating =. T.rating movie]
-- dbConnect = do
--             runStderrLoggingT $ withPostgresqlPool "host=localhost dbname=test_db user=root password=root port=5432" 10 (liftSqlPersistMPool fetchAllMovies)



deleteMovie :: (MonadIO m, BackendCompatible SqlBackend backend,
 PersistQueryWrite backend, PersistUniqueWrite backend) =>String -> ReaderT backend m ()
deleteMovie movieName =  E.delete $
                         E.from $ \p -> do
                         E.where_ (p E.^. MovieMname E.==. E.val movieName)

insertUser :: (BaseBackend backend ~ SqlBackend, MonadIO m,PersistUniqueWrite backend) =>T.User -> ReaderT backend m (Maybe ())
insertUser user = insertUnique_ $ Users (T.name user) (T.age user) (T.email user) (T.registration_date user) (T.password user) (T.favouriteMovie user)

updateUserFavMovie :: (BaseBackend backend ~ SqlBackend, MonadIO m,
 PersistQueryWrite backend) => Users -> ReaderT backend m ()
updateUserFavMovie  user = updateWhere [UsersEmail ==. usersEmail user] [UsersFavouriteMovie =. usersFavouriteMovie user]


getUserByEmail :: (BaseBackend backend ~ SqlBackend, MonadIO m,
 PersistQueryRead backend) =>String -> ReaderT backend m (Maybe (Entity Users))
getUserByEmail email = selectFirst [UsersEmail ==. email] []

checkIfMovieExists :: String -> SqlPersistM Bool
checkIfMovieExists movie = E.existsBy $ MoviePrimaryKey movie

queryfetchAllMovies :: IO [Entity Movie]
queryfetchAllMovies = do
             runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
              liftIO (runSqlPersistM fetchAllMovies b))


queryfetchOneByName::  String -> IO [Entity Movie]
queryfetchOneByName movieName = do
                  runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
                       liftIO (runSqlPersistM (fetchOneByName movieName) b))

queryinsertMovie ::  T.Movie -> IO (Maybe ())
queryinsertMovie movie = do
                    runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
                        runSqlConn (insertMovie movie) b)

queryupdateMovie :: T.Movie -> IO ()
queryupdateMovie  movie = do
                    runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
                        runSqlConn (updateMovie movie) b)

querydeleteMovie :: String -> IO ()
querydeleteMovie name = do
                   runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
                        runSqlConn (deleteMovie name) b)

querygetUserByEmail :: String -> IO (Maybe (Entity Users))
querygetUserByEmail email = do
                     runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
                       runSqlConn (getUserByEmail email) b)

queryinsertUser :: T.User -> IO (Maybe ())
queryinsertUser user = do
                    runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
                        runSqlConn (insertUser user) b)

queryupdateUserFavMovie:: Users -> IO()
queryupdateUserFavMovie user =  do
                    runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
                        runSqlConn (updateUserFavMovie user) b)

migrationScript :: IO ()
migrationScript = do
                  runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
                        runSqlConn (runMigration migrateAll) b)

querycheckIfMovieExists :: String -> IO Bool
querycheckIfMovieExists movieName = do
             runNoLoggingT $ withPostgresqlConn "host=localhost dbname=crud-haskell user=postgres password=Qwerty@052 port=5432" (\b -> do
              liftIO (runSqlPersistM (checkIfMovieExists movieName) b))