# Old Site Database Migration

Some sketches of commands used during development that could be used to build
an old to new DB migration script.

Assumes new database is `sleepanarchy-blog` and old one is `sa-old`.

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
