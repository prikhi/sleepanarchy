#!/usr/bin/env bash

# Create old db
createdb -U sleepanarchy-blog -O sleepanarchy-blog sa-old
psql -U sleepanarchy-blog sa-old < /blog-dump.psql

# Dump old tables
pg_dump sa-old -U sleepanarchy-blog \
    -t blog_blogcategory -t blog_blogpost \
    -t linkdump_dump -t linkdump_dumpcategory > ~/table-dump.sql
dropdb -U sleepanarchy-blog sa-old

# Load old tables into new db
psql -U sleepanarchy-blog sleepanarchy-blog < ~/table-dump.sql
rm -f ~/table-dump.sql

# Migrate data & drop old tables
psql -U sleepanarchy-blog sleepanarchy-blog <<SQL
-- BLOG
INSERT INTO "blog_category"
    (id, title, slug, created_at, updated_at)
SELECT
    id, title, slug, NOW(), NOW()
FROM "blog_blogcategory"
;

INSERT INTO "blog_post"
    ( id, title, slug
    , description, content, tags
    , author_id, category_id, created_at
    , updated_at, published_at, content_html, description_html
    )
SELECT
    id, title, slug,
    description, content, REPLACE(REPLACE(keywords_string, ' ', ','), 'Package,Management', 'Package Management'),
    1, 1, created,
    updated, publish_date, '', ''
FROM "blog_blogpost"
;

-- LINKS
INSERT INTO "link_category"
    (id, title, slug, parent_id)
SELECT
    id, title, slug, parent_id
FROM "linkdump_dumpcategory"
;

INSERT INTO "link"
    (id, title, slug, description, link, tags, parent_id, views,
     created_at, updated_at
    )
SELECT
    id, title, slug, description, link, tags_string, category_id, views,
    created, updated
FROM "linkdump_dump";

-- CLEANUP
DROP TABLE "blog_blogpost";
DROP TABLE "blog_blogcategory";
DROP TABLE "linkdump_dump";
DROP TABLE "linkdump_dumpcategory";
SQL
