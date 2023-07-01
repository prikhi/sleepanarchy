-- +migrate Up
ALTER TABLE "blog_post"
    ADD COLUMN "content_html" VARCHAR NOT NULL DEFAULT '',
    ADD COLUMN "description_html" VARCHAR NOT NULL DEFAULT ''
;

ALTER TABLE "blog_post"
    ALTER COLUMN "content_html" DROP DEFAULT,
    ALTER COLUMN "description_html" DROP DEFAULT
;

-- +migrate Down
ALTER TABLE "blog_post"
    DROP COLUMN "content_html",
    DROP COLUMN "description_html"
;
