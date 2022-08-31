import           Control.Exception.Safe         ( SomeException
                                                , throwIO
                                                , try
                                                )
import           Control.Monad                  ( (<=<)
                                                , void
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , eitherDecode
                                                , encode
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Either                    ( fromRight
                                                , isRight
                                                )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime(UTCTime)
                                                , fromGregorian
                                                , getCurrentTime
                                                )
import           GHC.Generics                   ( Generic )
import           Servant                        ( ServerError(..) )
import           System.Environment             ( setEnv )
import           System.Exit                    ( ExitCode(..) )

import           Hedgehog
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import           App
import           Handlers.Admin
import           Handlers.BlogPosts
import           Models.DB
import           Models.Utils                   ( slugify )
import           Test.Setup
import           Utils

import qualified Data.Text                     as T
import qualified Database.Persist.Sql          as P
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range


main :: IO ()
main = do
    -- Run the pure tests with default parallelism
    Left pureExit <- try @_ @ExitCode $ defaultMain pureTests
    -- Restrict to single threaded testing for database tests
    setEnv "TASTY_NUM_THREADS" "1"
    Left dbExit <- try @_ @ExitCode $ defaultMain dbTests
    -- Rethrow any ExitFailure exceptions
    case (pureExit, dbExit) of
        (ExitFailure _, _) -> throwIO pureExit
        _                  -> throwIO dbExit

pureTests :: TestTree
pureTests = testGroup "Pure Tests" [unitTests, properties]


-- | Placeholder / example for pure unit tests we may want to add.
unitTests :: TestTree
unitTests = testGroup "Unit Tests" [testCase "2+2 = 4" testAddition]
  where
    testAddition :: Assertion
    testAddition = (2 + 2) @?= (4 :: Integer)

properties :: TestTree
properties = testGroup
    "Properties"
    [ testProperty "(prefixParseJSON . prefixToJSON) is idempotent"
      $ property
      $ do
            obj <- forAll genPrefixJSONHelper
            eitherDecode (encode obj) === Right obj
    ]

data PrefixJSONHelper = PrefixJSONHelper
    { pjhFieldOne   :: Bool
    , pjhFieldTwo   :: Int
    , pjhFieldThree :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON PrefixJSONHelper where
    toJSON = prefixToJSON "pjh"

instance FromJSON PrefixJSONHelper where
    parseJSON = prefixParseJSON "pjh"

genPrefixJSONHelper :: MonadGen m => m PrefixJSONHelper
genPrefixJSONHelper =
    PrefixJSONHelper
        <$> Gen.bool
        <*> Gen.int Range.linearBounded
        <*> Gen.text (Range.linear 0 20) Gen.unicode



-- | All tests that hit the database should live under here. 'main'
-- restricts this this tree to running in a single thread, preventing
-- errors caused by multiple parallel tests making conflicting changes to
-- the database.
dbTests :: TestTree
dbTests = testGroup "DB Tests" [blogSidebarTests, adminTests]

adminTests :: TestTree
adminTests = testGroup "Admin" [adminBlogTests]

adminBlogTests :: TestTree
adminBlogTests = testGroup
    "Blog"
    [ getBlogPostsAdminTests
    , getBlogPostAdminTests
    , updateBlogPostTests
    , createBlogPostTests
    ]

getBlogPostsAdminTests :: TestTree
getBlogPostsAdminTests = testGroup
    "getBlogPostsAdmin"
    [ testCase "Returns Published & Unpublished Posts" $ do
          result <- testRunner $ do
              uid <- runDB $ do
                  uid <- P.entityKey <$> makeUser
                  cid <- P.entityKey <$> makeBlogCategory "category"
                  void $ makeBlogPost "p1" "" uid cid Nothing
                  void $ makeBlogPost "p2" "" uid cid $ Just $ UTCTime
                      (fromGregorian 2020 04 20)
                      0
                  return uid
              getBlogPostsAdmin uid
          length . abplPosts <$> result @?= Right 2
    ]

getBlogPostAdminTests :: TestTree
getBlogPostAdminTests = testGroup
    "getBlogPostAdmin"
    [ testCase "Throws a 404 when post does not exist" $ do
        result <- testRunner $ do
            uid <- runDB $ P.entityKey <$> makeUser
            let invalidPostId = P.fromBackendKey 9001
            getBlogPostAdmin uid invalidPostId
        first errHTTPCode result @?= Left 404
    , testCase "Succeeds for valid post IDs" $ do
        result <- testRunner $ do
            (uid, pid) <- runDB $ do
                uid <- P.entityKey <$> makeUser
                cid <- P.entityKey <$> makeBlogCategory "category"
                (uid, )
                    .   P.entityKey
                    <$> makeBlogPost "title" "" uid cid Nothing
            getBlogPostAdmin uid pid
        result `satisfies` isRight
    ]

updateBlogPostTests :: TestTree
updateBlogPostTests = testGroup
    "updateBlogPost"
    [ testCase "Throws a 404 when post does not exist" $ do
        result <- testRunner $ do
            uid <- runDB $ P.entityKey <$> makeUser
            let invalidPostId = P.fromBackendKey 9001
            updateBlogPost uid invalidPostId emptyUpdate
        first errHTTPCode result @?= Left 404
    , testCase "Does nothing when passed an empty update" $ do
        result <- testRunner $ do
            (uid, pid, post) <- runDB $ do
                uid <- P.entityKey <$> makeUser
                cid <- P.entityKey <$> makeBlogCategory "category"
                pid <- P.entityKey <$> makeBlogPost "title" "" uid cid Nothing
                (uid, pid, ) . fromJust <$> P.get pid
            void $ updateBlogPost uid pid emptyUpdate
            (post, ) . fromJust <$> runDB (P.get pid)
        result `satisfies` isRight
        let (initial, afterUpdate) = fromRight (error "checked") result
        afterUpdate @?= initial
    , testCase "Sets the UpdatedAt field if an update occurs" $ do
        result <- testRunner $ do
            (uid, pid, post) <- runDB $ do
                uid <- P.entityKey <$> makeUser
                cid <- P.entityKey <$> makeBlogCategory "category"
                pid <- P.entityKey <$> makeBlogPost "title" "" uid cid Nothing
                (uid, pid, ) . fromJust <$> P.get pid
            void $ updateBlogPost
                uid
                pid
                emptyUpdate { abpuContent = Just "something" }
            (blogPostUpdatedAt post, ) . blogPostUpdatedAt . fromJust <$> runDB
                (P.get pid)
        result `satisfies` isRight
        let (initial, afterUpdate) = fromRight (error "checked") result
        afterUpdate @?/= initial
    , testCase "Autogenerates a slug if left blank" $ do
        result <- testRunner $ insertAndUpdatePost emptyUpdate
            { abpuTitle = Just "new title"
            , abpuSlug  = Just ""
            }
        fmap blogPostSlug <$> result @?= Right (Just "new-title")
    , testCase "Publishes the post if told to" $ do
        result <- testRunner
            $ insertAndUpdatePost emptyUpdate { abpuPublished = Just True }
        ((>>= blogPostPublishedAt) <$> result)
            `satisfies` either (const False) isJust
    , testCase "Does not update publish or updated times if already published"
        $ do
              result <- testRunner $ do
                  now              <- liftIO getCurrentTime
                  (uid, pid, post) <- runDB $ do
                      uid <- P.entityKey <$> makeUser
                      cid <- P.entityKey <$> makeBlogCategory "category"
                      p   <- mkBlogPost "title" uid cid
                      pid <- P.insert p { blogPostPublishedAt = Just now }
                      (uid, pid, ) . fromJust <$> P.get pid
                  void $ updateBlogPost
                      uid
                      pid
                      emptyUpdate { abpuPublished = Just True }
                  (post, ) . fromJust <$> runDB (P.get pid)
              result `satisfies` isRight
              let (initial, afterUpdate) = fromRight (error "checked") result
              blogPostPublishedAt afterUpdate `satisfies` isJust
              blogPostPublishedAt afterUpdate @?= blogPostPublishedAt initial
              blogPostUpdatedAt afterUpdate @?= blogPostUpdatedAt initial
    , testCase "Can unpublish a Post" $ do
        result <- testRunner $ do
            now        <- liftIO getCurrentTime
            (uid, pid) <- runDB $ do
                uid <- P.entityKey <$> makeUser
                cid <- P.entityKey <$> makeBlogCategory "category"
                (uid, ) . P.entityKey <$> makeBlogPost "title"
                                                       ""
                                                       uid
                                                       cid
                                                       (Just now)
            void $ updateBlogPost uid
                                  pid
                                  emptyUpdate { abpuPublished = Just False }
            runDB $ P.get pid
        fmap blogPostPublishedAt <$> result @?= Right (Just Nothing)
    ]
  where
    emptyUpdate :: AdminBlogPostUpdate
    emptyUpdate =
        AdminBlogPostUpdate Nothing Nothing Nothing Nothing Nothing Nothing
    insertAndUpdatePost :: AdminBlogPostUpdate -> TestM (Maybe BlogPost)
    insertAndUpdatePost updateParams = do
        (uid, pid) <- runDB $ do
            uid <- P.entityKey <$> makeUser
            cid <- P.entityKey <$> makeBlogCategory "category"
            (uid, ) . P.entityKey <$> makeBlogPost "title" "" uid cid Nothing
        void $ updateBlogPost uid pid updateParams
        runDB $ P.get pid


-- | TODO: Move these to Handlers/BlogPostSpec.hs
blogSidebarTests :: TestTree
blogSidebarTests = testGroup
    "Blog Sidebar"
    [sidebarArchiveTests, sidebarTagTests, sidebarCategoryTests]

sidebarArchiveTests :: TestTree
sidebarArchiveTests = testGroup
    "Archive"
    [ testCase "Returns empty list when no posts exist" $ do
        result <- testRunner $ runDB getBlogSidebarArchive
        result @?= Right []
    , testCase "Does not error when only unpublished posts exist" $ do
        safeResult <- try @_ @SomeException . testRunner $ runDB $ do
            uid <- P.entityKey <$> makeUser
            cid <- P.entityKey <$> makeBlogCategory "category"
            void $ makeBlogPost "title" "" uid cid Nothing
            getBlogSidebarArchive
        isRight safeResult @? "Does not throw an exception"
        let result = fromRight (error "checked") safeResult
        isRight result @? "Does not return a server error"
        let archiveRows = fromRight (error "checked") result
        archiveRows @?= []
    , testCase "Returns post counts by year/month partitions" $ do
        result <- testRunner $ runDB $ do
            uid <- P.entityKey <$> makeUser
            cid <- P.entityKey <$> makeBlogCategory "some category"
            let firstMonth = UTCTime (fromGregorian 2022 01 02) 0
                monthAfter = UTCTime (fromGregorian 2022 02 02) 0
                thirdMonth = UTCTime (fromGregorian 2022 03 02) 0
            void $ makeBlogPost "p1" "" uid cid $ Just firstMonth
            void $ makeBlogPost "p2" "" uid cid $ Just firstMonth
            void $ makeBlogPost "p3" "" uid cid $ Just monthAfter
            void $ makeBlogPost "p4" "" uid cid $ Just thirdMonth
            void $ makeBlogPost "p5" "" uid cid $ Just thirdMonth
            void $ makeBlogPost "p6" "" uid cid $ Just thirdMonth
            getBlogSidebarArchive
        isRight result @? "Does not return a server error"
        result @?= Right
            [ BlogArchiveYearData 2022 3 3
            , BlogArchiveYearData 2022 2 1
            , BlogArchiveYearData 2022 1 2
            ]
    ]

sidebarTagTests :: TestTree
sidebarTagTests = testGroup
    "Tags"
    [ testCase "Returns empty list when no posts exist" $ do
        result <- testRunner $ runDB getBlogSidebarTags
        result @?= Right []
    , testCase "Does not error when only unpublished posts exist" $ do
        safeResult <- try @_ @SomeException . testRunner . runDB $ do
            uid <- P.entityKey <$> makeUser
            cid <- P.entityKey <$> makeBlogCategory "category"
            bp  <- mkBlogPost "title" uid cid
            P.insert_ bp { blogPostTags = "tag1, tag2" }
            getBlogSidebarTags
        isRight safeResult @? "Does not throw an exception"
        let result = fromRight (error "checked") safeResult
        isRight result @? "Does not return a server error"
        let tagRows = fromRight (error "checked") result
        tagRows @?= []
    , testCase "Ignores empty tags" $ do
        result <- testRunner $ runDB $ do
            uid <- P.entityKey <$> makeUser
            cid <- P.entityKey <$> makeBlogCategory "category"
            bp  <- mkBlogPost "title" uid cid
            P.insert_ bp { blogPostTags        = ", "
                         , blogPostPublishedAt = Just $ blogPostCreatedAt bp
                         }
            getBlogSidebarTags
        result @?= Right []
    , testCase "Returns tags with counts alphabetically" $ do
        result <- testRunner $ runDB $ do
            uid  <- P.entityKey <$> makeUser
            cid1 <- P.entityKey <$> makeBlogCategory "category 1"
            cid2 <- P.entityKey <$> makeBlogCategory "category 2"
            bp   <- mkBlogPost "title" uid cid1
            P.insert_ bp { blogPostTags        = "clydefrog, artmeis"
                         , blogPostPublishedAt = Just $ blogPostCreatedAt bp
                         }
            P.insert_ bp { blogPostSlug        = "1"
                         , blogPostTags        = "goku, elohel"
                         , blogPostPublishedAt = Just $ blogPostCreatedAt bp
                         }
            P.insert_ bp { blogPostSlug        = "2"
                         , blogPostTags        = "elohel"
                         , blogPostCategoryId  = cid2
                         , blogPostPublishedAt = Just $ blogPostCreatedAt bp
                         }
            P.insert_ bp { blogPostSlug        = "3"
                         , blogPostTags        = "hidden"
                         , blogPostPublishedAt = Nothing
                         }
            getBlogSidebarTags
        result @?= Right
            [ BlogTagData "artmeis"   1
            , BlogTagData "clydefrog" 1
            , BlogTagData "elohel"    2
            , BlogTagData "goku"      1
            ]
    ]

sidebarCategoryTests :: TestTree
sidebarCategoryTests = testGroup
    "Categories"
    [ testCase "Returns empty list when no posts exist" $ do
        result <- testRunner $ runDB $ do
            void $ P.entityKey <$> makeBlogCategory "category"
            getBlogSidebarCategories
        result @?= Right []
    , testCase "Returns empty list when only unpublished posts exist" $ do
        result <- testRunner $ runDB $ do
            uid  <- P.entityKey <$> makeUser
            cid1 <- P.entityKey <$> makeBlogCategory "category1"
            cid2 <- P.entityKey <$> makeBlogCategory "category2"
            void $ P.entityKey <$> makeBlogCategory "category3"
            void $ makeBlogPost "p1" "" uid cid1 Nothing
            void $ makeBlogPost "p2" "" uid cid1 Nothing
            void $ makeBlogPost "p3" "" uid cid2 Nothing
            getBlogSidebarCategories
        result @?= Right []
    , testCase "Returns categories with published posts" $ do
        result <- testRunner $ runDB $ do
            uid  <- P.entityKey <$> makeUser
            cid1 <- P.entityKey <$> makeBlogCategory "category V"
            cid2 <- P.entityKey <$> makeBlogCategory "category C"
            cid3 <- P.entityKey <$> makeBlogCategory "category Z"
            void $ P.entityKey <$> makeBlogCategory "category A"
            now <- liftIO getCurrentTime
            void $ makeBlogPost "p1" "" uid cid1 $ Just now
            void $ makeBlogPost "p2" "" uid cid1 $ Just now
            void $ makeBlogPost "p3" "" uid cid2 $ Just now
            void $ makeBlogPost "p4" "" uid cid2 Nothing
            void $ makeBlogPost "p5" "" uid cid3 Nothing
            getBlogSidebarCategories
        result @?= Right
            [ BlogSidebarCategoryData "category C" "category-c" 1
            , BlogSidebarCategoryData "category V" "category-v" 2
            ]
    ]

createBlogPostTests :: TestTree
createBlogPostTests = testGroup
    "createBlogPost"
    [ testCase "Creates a new Blog Post" $ do
        result <- testRunner $ do
            (P.entityKey -> uid, P.entityKey -> cid) <-
                runDB $ (,) <$> makeUser <*> makeBlogCategory "category"
            pid <- createBlogPost
                uid
                NewBlogPost { nbpTitle       = "The Title"
                            , nbpSlug        = Just "the-title"
                            , nbpDescription = Just "short text"
                            , nbpContent     = "Post's full content"
                            , nbpTags        = "t1, t2"
                            , nbpPublish     = False
                            , nbpCategoryId  = cid
                            }
            runDB $ (,) <$> P.get pid <*> P.count @_ @_ @BlogPost []
        snd <$> result @?= Right 1
        (blogPostPublishedAt <=< fst) <$> result @?= Right Nothing
    , testCase "Can publish the new post when creating" $ do
        result <- testRunner $ do
            (P.entityKey -> uid, P.entityKey -> cid) <-
                runDB $ (,) <$> makeUser <*> makeBlogCategory "category"
            pid <- createBlogPost
                uid
                NewBlogPost { nbpTitle       = "The Title"
                            , nbpSlug        = Just "the-title"
                            , nbpDescription = Just "short text"
                            , nbpContent     = "Post's full content"
                            , nbpTags        = "t1, t2"
                            , nbpPublish     = True
                            , nbpCategoryId  = cid
                            }
            runDB $ P.get pid
        result `satisfies` isRight
        let post = fromJust $ fromRight (error "checked") result
        blogPostPublishedAt post `satisfies` isJust
    , testCase "Can auto-generate the slug" $ do
        result <- testRunner $ do
            (P.entityKey -> uid, P.entityKey -> cid) <-
                runDB $ (,) <$> makeUser <*> makeBlogCategory "category"
            pid <- createBlogPost
                uid
                NewBlogPost { nbpTitle       = "The Title"
                            , nbpSlug        = Nothing
                            , nbpDescription = Just "short text"
                            , nbpContent     = "Post's full content"
                            , nbpTags        = "t1, t2"
                            , nbpPublish     = True
                            , nbpCategoryId  = cid
                            }
            fmap fromJust . runDB $ P.get pid
        blogPostSlug <$> result @?= Right "the-title"
    , testCase "Slug generation respects uniqueness" $ do
        result <- testRunner $ do
            (P.entityKey -> uid, P.entityKey -> cid) <-
                runDB $ (,) <$> makeUser <*> makeBlogCategory "category"
            void . runDB $ makeBlogPost "The Title" "" uid cid Nothing
            pid <- createBlogPost
                uid
                NewBlogPost { nbpTitle       = "The Title"
                            , nbpSlug        = Nothing
                            , nbpDescription = Just "short text"
                            , nbpContent     = "Post's full content"
                            , nbpTags        = "t1, t2"
                            , nbpPublish     = True
                            , nbpCategoryId  = cid
                            }
            fmap fromJust . runDB $ P.get pid
        blogPostSlug <$> result @?= Right "the-title-1"
    ]


-- TODO: move these helpers to the Test.Utils module

-- | Assert a value passes some conditon.
satisfies :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
satisfies val predicate =
    let msg = show val <> " does not satisfy predicate."
    in  assertBool msg (predicate val)

-- | Assert two values are _not_ equal.
(@?/=) :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
(@?/=) actual expected = if actual /= expected
    then return ()
    else
        assertFailure
        $  "Expected different values, got the same:\n"
        <> show expected

makeUser :: MonadIO m => P.SqlPersistT m (P.Entity User)
makeUser = P.insertEntity $ User "me" "hunter2"

makeBlogCategory
    :: MonadIO m => Text -> P.SqlPersistT m (P.Entity BlogCategory)
makeBlogCategory title = do
    now <- liftIO getCurrentTime
    P.insertEntity $ BlogCategory title (slugify title) now now

-- | TODO: this one might be more useful if we filled in most fields with
-- most common args & rest as defaults but just returned a BlogPost so that
-- one can overwrite other fields & manually insert.
makeBlogPost
    :: MonadIO m
    => Text
    -> Text
    -> UserId
    -> BlogCategoryId
    -> Maybe UTCTime
    -> P.SqlPersistT m (P.Entity BlogPost)
makeBlogPost title content uid cid mbPublish = do
    now <- liftIO getCurrentTime
    P.insertEntity $ BlogPost title
                              (slugify title)
                              (T.take 20 content)
                              content
                              ""
                              uid
                              cid
                              now
                              now
                              mbPublish

mkBlogPost :: MonadIO m => Text -> UserId -> BlogCategoryId -> m BlogPost
mkBlogPost title uid cid = do
    now <- liftIO getCurrentTime
    return $ BlogPost title (slugify title) "" "" "" uid cid now now Nothing
