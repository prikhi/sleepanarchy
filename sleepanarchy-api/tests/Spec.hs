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
import           Servant                        ( Headers(..)
                                                , ServerError(..)
                                                , getHeaders
                                                )
import           System.Environment             ( setEnv )
import           System.Exit                    ( ExitCode(..) )

import           Hedgehog
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import           App
import           Handlers.Admin
import           Handlers.BlogPosts
import           Handlers.Links
import           Models.DB
import           Models.Utils                   ( slugify )
import           Test.Setup
import           Utils

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Database.Persist.Sql          as P
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range


main :: IO ()
main = do
    -- Run an blank test to initialize the database
    void . testRunner $ return ()
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
unitTests = testGroup "Unit Tests" [appUnitTests, adminUnitTests]

appUnitTests :: TestTree
appUnitTests = testGroup "App" [foldersToSubPathTests]

foldersToSubPathTests :: TestTree
foldersToSubPathTests = testGroup
    "foldersToSubPath"
    [ testCase "Treats empty folders as root path" $ do
        run [] @?= "."
    , testCase "Allows root path" $ do
        run ["/"] @?= "/"
    , testCase "Ignores empty paths" $ do
        run ["", "", ""] @?= "."
        run ["", "first", "", "second"] @?= "first/second"
    , testCase "Ignores parent segments" $ do
        run [".."] @?= "."
        run ["..", "nested"] @?= "nested"
        run ["nested", "..", "infix"] @?= "nested/infix"
        run ["nested/../inline"] @?= "nested/inline"
    , testCase "Allows folders as elements or in elements" $ do
        run ["nested/inline", "in-list"] @?= "nested/inline/in-list"
    ]
  where
    run :: [FilePath] -> FilePath
    run = fromMediaSubPath . foldersToSubPath

adminUnitTests :: TestTree
adminUnitTests = testGroup "Admin" [adminMediaUnitTests]

adminMediaUnitTests :: TestTree
adminMediaUnitTests = testGroup "Media" [listMediaDirectoryTests]

listMediaDirectoryTests :: TestTree
listMediaDirectoryTests = testGroup
    "listMediaDirectory"
    [ testCase "Throws a 404 when directory does not exist" $ do
        result <- testRunner
            $ listMediaDirectory (P.toSqlKey 9001) ["non-existent"]
        first errHTTPCode result @?= Left 404
    , testCase "Lists single directory" $ do
        let media = InMemoryDirectory $ HM.fromList
                [ ( "dir1"
                  , DirectoryNode . InMemoryDirectory $ HM.fromList
                      [("f1", FileNode "file one")]
                  )
                , ( "dir2"
                  , DirectoryNode . InMemoryDirectory $ HM.fromList
                      [ ("f1"    , FileNode "file one")
                      , ("f2.png", FileNode "file two")
                      , ("dir3"  , DirectoryNode $ InMemoryDirectory HM.empty)
                      ]
                  )
                ]
        (finalMedia, result) <- customTestRunner media
            $ listMediaDirectory (P.toSqlKey 9001) ["dir2"]
        finalMedia @?= media
        result @?= Right AdminMediaList
            { amlBasePath = "/dir2/"
            , amlContents = [ AdminMediaItem "dir3"   Directory
                            , AdminMediaItem "f1"     Other
                            , AdminMediaItem "f2.png" Image
                            ]
            }
    ]

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
dbTests = testGroup "DB Tests" [blogSidebarTests, linkTests, adminTests]

adminTests :: TestTree
adminTests = testGroup "Admin" [adminBlogTests, adminMediaTests]

adminMediaTests :: TestTree
adminMediaTests =
    testGroup "Media" [createMediaDirectoryTests, uploadMediaFileTests]

createMediaDirectoryTests :: TestTree
createMediaDirectoryTests = testGroup
    "createMediaDirectoryTests"
    [ testCase "Throws a 403 when user does not exist" $ do
        result <- testRunner $ createMediaDirectory (P.toSqlKey 9001) []
        first errHTTPCode result @?= Left 403
    , testCase "Throws a 422 if the directory path is a file" $ do
        let media = InMemoryDirectory $ HM.fromList [("test", FileNode "")]
        (finalMedia, result) <- customTestRunner media $ do
            uid <- P.entityKey <$> runDB makeUser
            createMediaDirectory uid ["test"]
        first errHTTPCode result @?= Left 422
        finalMedia @?= media
    , testCase "Does not error if the directory already exists" $ do
        let media = InMemoryDirectory $ HM.fromList
                [("testDir", DirectoryNode $ InMemoryDirectory HM.empty)]
        (finalMedia, result) <- customTestRunner media $ do
            uid <- P.entityKey <$> runDB makeUser
            createMediaDirectory uid ["testDir"]
        result `satisfies` isRight
        finalMedia @?= media
    , testCase "Creates directories with arbitrary nesting" $ do
        (finalMedia, result) <-
            customTestRunner (InMemoryDirectory HM.empty) $ do
                uid <- P.entityKey <$> runDB makeUser
                createMediaDirectory uid ["nested/in", "list-and-string"]
        result `satisfies` isRight
        finalMedia @?= InMemoryDirectory
            (HM.fromList
                [ ( "nested"
                  , DirectoryNode . InMemoryDirectory $ HM.fromList
                      [ ( "in"
                        , DirectoryNode $ InMemoryDirectory
                            (HM.fromList
                                [ ( "list-and-string"
                                  , DirectoryNode $ InMemoryDirectory HM.empty
                                  )
                                ]
                            )
                        )
                      ]
                  )
                ]
            )
    ]

uploadMediaFileTests :: TestTree
uploadMediaFileTests = testGroup
    "uploadMediaFile"
    [ testCase "Throws a 403 when user does not exist" $ do
        result <- testRunner $ uploadMediaFile
            (P.toSqlKey 9001)
            (MediaUpload { muPath = []
                         , muName = "filename.png"
                         , muData = Base64Text "utf8-bin-data"
                         }
            )
        first errHTTPCode result @?= Left 403
    , testCase "Persists the uploaded file" $ do
        let media = InMemoryDirectory $ HM.fromList
                [ ( "nested"
                  , DirectoryNode . InMemoryDirectory $ HM.fromList
                      [("path", DirectoryNode $ InMemoryDirectory HM.empty)]
                  )
                ]
        (finalMedia, result) <- customTestRunner media $ do
            uid <- runDB $ P.entityKey <$> makeUser
            uploadMediaFile uid $ MediaUpload
                { muPath = ["nested", "path"]
                , muName = "some-file.gif"
                , muData = Base64Text "some-binary-text"
                }
        result `satisfies` isRight
        finalMedia @?= InMemoryDirectory
            (HM.fromList
                [ ( "nested"
                  , DirectoryNode . InMemoryDirectory $ HM.fromList
                      [ ( "path"
                        , DirectoryNode $ InMemoryDirectory $ HM.fromList
                            [("some-file.gif", FileNode "some-binary-text")]
                        )
                      ]
                  )
                ]
            )
    , testCase "Does not override existing files" $ do
        let media = InMemoryDirectory
                $ HM.fromList [("test-file.txt", FileNode "contents")]
        (finalMedia, result) <- customTestRunner media $ do
            uid <- runDB $ P.entityKey <$> makeUser
            uploadMediaFile uid $ MediaUpload
                { muPath = []
                , muName = "test-file.txt"
                , muData = Base64Text "different stuff"
                }
        result `satisfies` isRight
        finalMedia @?= InMemoryDirectory
            (HM.fromList
                [ ("test-file.txt"    , FileNode "contents")
                , ("test-file-001.txt", FileNode "different stuff")
                ]
            )
    ]


adminBlogTests :: TestTree
adminBlogTests = testGroup
    "Blog"
    [ getBlogPostsAdminTests
    , getBlogPostAdminTests
    , updateBlogPostTests
    , createBlogPostTests
    , getBlogCategoriesAdminTests
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

getBlogCategoriesAdminTests :: TestTree
getBlogCategoriesAdminTests = testGroup
    "getBlogCategoriesAdmin"
    [ testCase "Returns empty list when no categories exist" $ do
        result <- testRunner $ do
            uid <- P.entityKey <$> runDB makeUser
            getBlogCategoriesAdmin uid
        result @?= Right []
    , testCase "Returns categories in alphabetical order" $ do
        result <- testRunner $ do
            (uid, firstId, secondId) <- runDB $ do
                uid      <- P.entityKey <$> makeUser
                secondId <- P.entityKey <$> makeBlogCategory "B"
                firstId  <- P.entityKey <$> makeBlogCategory "A"
                return (uid, firstId, secondId)
            (firstId, secondId, ) <$> getBlogCategoriesAdmin uid
        result `satisfies` isRight
        let (id1, id2, categories) = fromRight (error "checked") result
        categories @?= [AdminBlogCategory "A" id1, AdminBlogCategory "B" id2]
    ]


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

-- TODO: Move these to Handlers/LinksSpec.hs
linkTests :: TestTree
linkTests = testGroup
    "Links"
    [getAllLinksTests, getLinkCategoryTests, redirectToLinkTests]

getAllLinksTests :: TestTree
getAllLinksTests = testGroup
    "getAllLinks"
    [ testCase "Returns empty list when no categories exist" $ do
        result <- testRunner getAllLinks
        result @?= Right (RootLinkCategories [])
    , testCase "Drops empty categories recursively" $ do
        result <- testRunner $ do
            runDB $ do
                rootId <- P.entityKey <$> makeLinkCategory "root" Nothing
                void $ makeLinkCategory "child" $ Just rootId
            getAllLinks
        result @?= Right (RootLinkCategories [])
    , testCase "Returns categories with only Links" $ do
        result <- testRunner $ do
            rootId  <- runDB $ P.entityKey <$> makeLinkCategory "root" Nothing
            childId <- runDB $ P.entityKey <$> makeLinkCategory
                "child"
                (Just rootId)
            void . runDB $ makeLink "some link" childId
            getAllLinks
        result @?= Right
            (RootLinkCategories
                [ LinkCategoryMap
                      { lcmCategory    = "root"
                      , lcmSlug        = "root"
                      , lcmChildren    =
                          [ LinkCategoryMap
                                { lcmCategory    = "child"
                                , lcmSlug        = "child"
                                , lcmChildren    = []
                                , lcmLinks       = [ LinkDetails
                                                         { ldTitle = "some link"
                                                         , ldSlug = "some-link"
                                                         , ldDescription = ""
                                                         , ldViews = 0
                                                         }
                                                   ]
                                }
                          ]
                      , lcmLinks       = []
                      }
                ]
            )
    , testCase "Returns categories ordered by name" $ do
        result <- testRunner $ do
            runDB $ do
                root2Id <- P.entityKey <$> makeLinkCategory "ZZZ" Nothing
                root1Id <- P.entityKey <$> makeLinkCategory "AAA" Nothing
                void $ makeLink "l2" root2Id
                void $ makeLink "l1" root1Id
            getAllLinks
        result `satisfies` isRight
        let RootLinkCategories cats = fromRight (error "checked") result
        lcmCategory <$> cats @?= ["AAA", "ZZZ"]
    , testCase "Returns links ordered by name" $ do
        result <- testRunner $ do
            runDB $ do
                rootId <- P.entityKey <$> makeLinkCategory "root" Nothing
                mapM_ (`makeLink` rootId) ["ZZZ", "AAA", "HHH"]
            getAllLinks
        result `satisfies` isRight
        let RootLinkCategories (head -> rootCat) =
                fromRight (error "checked") result
        ldTitle <$> lcmLinks rootCat @?= ["AAA", "HHH", "ZZZ"]
    ]

getLinkCategoryTests :: TestTree
getLinkCategoryTests = testGroup
    "getLinkCategory"
    [ testCase "Throws a 404 if no categories exist" $ do
        result <- testRunner $ getLinkCategory "none"
        first errHTTPCode result @?= Left 404
    , testCase "Throws a 404 if the category does not exist" $ do
        result <- testRunner $ do
            runDB $ do
                root1Id <- P.entityKey <$> makeLinkCategory "root1" Nothing
                root2Id <- P.entityKey <$> makeLinkCategory "root2" Nothing
                void $ makeLinkCategory "root3" Nothing
                child1Id <- P.entityKey <$> makeLinkCategory "c1" (Just root1Id)
                child2Id <- P.entityKey <$> makeLinkCategory "c2" (Just root1Id)
                child3Id <- P.entityKey <$> makeLinkCategory "c3" (Just root2Id)
                void $ makeLink "l1" child1Id
                void $ makeLink "l2" child1Id
                void $ makeLink "l3" child2Id
                void $ makeLink "l4" child3Id
            getLinkCategory "doesnt-exist"
        first errHTTPCode result @?= Left 404
    , testCase "Throws a 404 if the category has no links" $ do
        result <- testRunner $ do
            runDB $ do
                root1Id <- P.entityKey <$> makeLinkCategory "root1" Nothing
                void $ makeLinkCategory "c1" (Just root1Id)
            getLinkCategory "c1"
        first errHTTPCode result @?= Left 404
    , testCase "Returns the correct root category" $ do
        result <- testRunner $ do
            runDB $ do
                root1Id <- P.entityKey <$> makeLinkCategory "root1" Nothing
                root2Id <- P.entityKey <$> makeLinkCategory "root2" Nothing
                c1Id    <- P.entityKey <$> makeLinkCategory "c1" (Just root1Id)
                void $ makeLink "c1l1" c1Id
                void $ makeLink "c1l2" c1Id
                void $ makeLink "r1l1" root1Id
                void $ makeLink "r2l1" root2Id
                void $ makeLink "r2l2" root2Id
            getLinkCategory "root1"
        result `satisfies` isRight
        let lcMap = fromRight (error "checked") result
        lcMap @?= LinkCategoryMap
            { lcmCategory    = "root1"
            , lcmSlug        = "root1"
            , lcmChildren    =
                [ LinkCategoryMap
                      { lcmCategory    = "c1"
                      , lcmSlug        = "c1"
                      , lcmChildren    = []
                      , lcmLinks       = [ LinkDetails { ldTitle       = "c1l1"
                                                       , ldSlug        = "c1l1"
                                                       , ldDescription = ""
                                                       , ldViews       = 0
                                                       }
                                         , LinkDetails { ldTitle       = "c1l2"
                                                       , ldSlug        = "c1l2"
                                                       , ldDescription = ""
                                                       , ldViews       = 0
                                                       }
                                         ]
                      }
                ]
            , lcmLinks       = [ LinkDetails { ldTitle       = "r1l1"
                                             , ldSlug        = "r1l1"
                                             , ldDescription = ""
                                             , ldViews       = 0
                                             }
                               ]
            }
    , testCase "Returns the correct sub-LinkCategoryMap" $ do
        result <- testRunner $ do
            runDB $ do
                root1Id <- P.entityKey <$> makeLinkCategory "root1" Nothing
                c1Id    <- P.entityKey <$> makeLinkCategory "c1" (Just root1Id)
                c1c1Id  <- P.entityKey <$> makeLinkCategory "c1c1" (Just c1Id)
                void $ makeLink "c1l1" c1Id
                void $ makeLink "c1l2" c1Id
                void $ makeLink "c1c1l1" c1c1Id
                c2Id <- P.entityKey <$> makeLinkCategory "c2" (Just root1Id)
                void $ makeLink "r1l1" root1Id
                void $ makeLink "c2l1" c2Id
            getLinkCategory "c1"
        result `satisfies` isRight
        let lcMap = fromRight (error "checked") result
        lcMap @?= LinkCategoryMap
            { lcmCategory    = "c1"
            , lcmSlug        = "c1"
            , lcmChildren    =
                [ LinkCategoryMap
                      { lcmCategory    = "c1c1"
                      , lcmSlug        = "c1c1"
                      , lcmChildren    = []
                      , lcmLinks       = [ LinkDetails { ldTitle = "c1c1l1"
                                                       , ldSlug = "c1c1l1"
                                                       , ldDescription = ""
                                                       , ldViews = 0
                                                       }
                                         ]
                      }
                ]
            , lcmLinks       = [ LinkDetails { ldTitle       = "c1l1"
                                             , ldSlug        = "c1l1"
                                             , ldDescription = ""
                                             , ldViews       = 0
                                             }
                               , LinkDetails { ldTitle       = "c1l2"
                                             , ldSlug        = "c1l2"
                                             , ldDescription = ""
                                             , ldViews       = 0
                                             }
                               ]
            }
    ]

redirectToLinkTests :: TestTree
redirectToLinkTests = testGroup
    "redirectToLink"
    [ testCase "Throws a 404 when Link doesn't exist" $ do
        result <- testRunner $ redirectToLink "doesnt-exist"
        bimap errHTTPCode getResponse result @?= Left 404
    , testCase "Returns expected Header & Body" $ do
        result <- testRunner $ do
            runDB $ do
                rootId <- P.entityKey <$> makeLinkCategory "root" Nothing
                linkId <- P.entityKey <$> makeLink "test link" rootId
                P.update linkId [LinkLink P.=. "https://http.cat/302"]
            redirectToLink "test-link"
        second getResponse result @?= Right (RedirectBody "Found")
        second (getHeaders . getHeadersHList) result
            @?= Right [("Location", "https://http.cat/302")]
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

makeLinkCategory
    :: MonadIO m
    => Text
    -> Maybe LinkCategoryId
    -> P.SqlPersistT m (P.Entity LinkCategory)
makeLinkCategory title parentId =
    P.insertEntity $ LinkCategory title (slugify title) parentId

makeLink
    :: MonadIO m => Text -> LinkCategoryId -> P.SqlPersistT m (P.Entity Link)
makeLink title parentId = do
    now <- liftIO getCurrentTime
    P.insertEntity $ Link { linkTitle       = title
                          , linkSlug        = slugify title
                          , linkDescription = ""
                          , linkLink        = "https://www.google.com"
                          , linkTags        = ""
                          , linkParentId    = parentId
                          , linkViews       = 0
                          , linkCreatedAt   = now
                          , linkUpdatedAt   = now
                          }
