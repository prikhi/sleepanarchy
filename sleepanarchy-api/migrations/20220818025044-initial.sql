-- +migrate Up
CREATE TABLE "user" (
    "id" SERIAL8 PRIMARY KEY UNIQUE,
    "name" VARCHAR NOT NULL,
    "password" VARCHAR NOT NULL
);
ALTER TABLE "user"
    ADD CONSTRAINT "unique_user" UNIQUE("name");

CREATE TABLE "blog_category" (
    "id" SERIAL8 PRIMARY KEY UNIQUE,
    "title" VARCHAR NOT NULL,
    "slug" VARCHAR NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);
ALTER TABLE "blog_category"
    ADD CONSTRAINT "unique_blog_category" UNIQUE("slug");


CREATE TABLE "blog_post" (
    "id" SERIAL8 PRIMARY KEY UNIQUE,
    "title" VARCHAR NOT NULL,
    "slug" VARCHAR NOT NULL,
    "description" VARCHAR NOT NULL,
    "content" VARCHAR NOT NULL,
    "tags" VARCHAR NOT NULL,
    "author_id" INT8 NOT NULL,
    "category_id" INT8 NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    "published_at" TIMESTAMP WITH TIME ZONE NULL
);
ALTER TABLE "blog_post"
    ADD CONSTRAINT "unique_blog_post" UNIQUE("slug"),
    ADD CONSTRAINT "blog_post_category_id_fkey" FOREIGN KEY("category_id")
        REFERENCES "blog_category"("id") ON DELETE RESTRICT ON UPDATE RESTRICT,
    ADD CONSTRAINT "blog_post_author_id_fkey" FOREIGN KEY("author_id")
        REFERENCES "user"("id") ON DELETE CASCADE ON UPDATE RESTRICT
;
CREATE INDEX "blog_post_category_id_idx" ON "blog_post"("category_id");


-- +migrate Down
DROP TABLE "blog_post";
DROP TABLE "blog_category";
DROP TABLE "user";
