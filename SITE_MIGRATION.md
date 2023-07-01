# Old Site Database Migration

Some sketches of commands used during development that could be used to build
an old to new DB migration script.

## Setup

Create & migrate the new database:

```sh
createdb sleepanarchy-blog
cd sleepanarchy-api
sql-migrate up
```

Create & import the old database:

```sh
createdb sa-old -O $(whoami)
psql sa-old < blog-dump.sql
```


## Blog Post

Dump the old tables, load them into the new database:

```sh
pg_dump sa-old -h 127.0.0.1 -U postgres \
    -t blog_blogcategory -t blog_blogpost > blogpost.sql
psql sleepanarchy-blog -h 127.0.0.1 -U postgres < blogpost.sql
psql sleepanarchy-blog -h 127.0.0.1 -U postgres
```

Migrate rows from old tables to new tables:

```sql
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
```

Drop old tables:

```sql
DROP TABLE "blog_blogpost";
DROP TABLE "blog_blogcategory";
```

You will need to tweak the post markdown to render the code blocks with syntax
highlighting(old format used indented blocks with `:::language`, new format
should use fenced code blocks).

You will need to tweak the description fields to include any missing HTML - the
previous website generated the HTML from the post contents & only put plaintext
versions in the database..

Finally, run the `regnerate-html` command to pre-generate the HTML for every
post:

```sh
stack run sleepanarchy-api-management -- regenerate-html
```


## Links

Dump the old tables, load them into the new database:

```sh
pg_dump sa-old -h 127.0.0.1 -U postgres \
    -t linkdump_dump -t linkdump_dumpcategory > linkdump.sql
psql sleepanarchy-blog -h 127.0.0.1 -U postgres < linkdump.sql
psql sleepanarchy-blog -h 127.0.0.1 -U postgres
```

Migrate rows from old tables to new tables:

```sql
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
```

Drop old tables:

```sql
DROP TABLE "linkdump_dump";
DROP TABLE "linkdump_dumpcategory";
```
