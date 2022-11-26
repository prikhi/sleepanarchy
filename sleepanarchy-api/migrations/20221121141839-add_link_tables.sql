-- +migrate Up
CREATE TABLE "link_category" (
    "id" SERIAL8 PRIMARY KEY UNIQUE,
    "title" VARCHAR NOT NULL,
    "slug" VARCHAR NOT NULL,
    "parent_id" INT8 NULL
);

ALTER TABLE "link_category" ADD CONSTRAINT "unique_link_dump_category" UNIQUE ("slug");
ALTER TABLE "link_category" 
    ADD CONSTRAINT "link_category_parent_id_fkey" FOREIGN KEY ("parent_id") REFERENCES "link_category" ("id") 
        ON DELETE CASCADE ON UPDATE RESTRICT;

CREATE TABLE "link" (
    "id" SERIAL8 PRIMARY KEY UNIQUE,
    "title" VARCHAR NOT NULL,
    "slug" VARCHAR NOT NULL,
    "description" VARCHAR NOT NULL,
    "link" VARCHAR NOT NULL,
    "tags" VARCHAR NOT NULL,
    "parent_id" INT8 NOT NULL,
    "views" INT8 NOT NULL,
    "created_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now (),
    "updated_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now ()
);

ALTER TABLE "link" ADD CONSTRAINT "unique_link" UNIQUE ("slug");
ALTER TABLE "link" 
    ADD CONSTRAINT "link_parent_id_fkey" FOREIGN KEY ("parent_id") REFERENCES "link_category" ("id") 
        ON DELETE CASCADE ON UPDATE RESTRICT;

-- +migrate Down

DROP TABLE "link_category";
DROP TABLE "link";
