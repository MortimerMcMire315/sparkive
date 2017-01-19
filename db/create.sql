CREATE TABLE "attribute" (
	"id" serial NOT NULL,
	"attr_name" TEXT NOT NULL,
	CONSTRAINT attribute_pk PRIMARY KEY ("id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "item_attrs" (
	"id" serial NOT NULL,
	"item_id" bigint NOT NULL,
	"attr_value_id" bigint NOT NULL,
	CONSTRAINT item_attrs_pk PRIMARY KEY ("id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "item" (
	"id" serial NOT NULL,
	"title" TEXT NOT NULL,
	"data" TEXT NOT NULL,
	CONSTRAINT item_pk PRIMARY KEY ("id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "attr_values" (
	"id" serial NOT NULL,
	"attr_id" bigint NOT NULL,
	"attr_value" TEXT NOT NULL,
	CONSTRAINT attr_values_pk PRIMARY KEY ("id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "sparkive_user" (
	"id" serial NOT NULL,
	"username" TEXT NOT NULL UNIQUE,
	"pass" bytea NOT NULL,
	"salt" bytea NOT NULL,
	CONSTRAINT sparkive_user_pk PRIMARY KEY ("id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "sess_token" (
	"id" serial NOT NULL,
	"username" TEXT NOT NULL,
	"token" bytea NOT NULL,
	CONSTRAINT sess_token_pk PRIMARY KEY ("id")
) WITH (
  OIDS=FALSE
);




ALTER TABLE "item_attrs" ADD CONSTRAINT "item_attrs_fk0" FOREIGN KEY ("item_id") REFERENCES "item"("id");
ALTER TABLE "item_attrs" ADD CONSTRAINT "item_attrs_fk1" FOREIGN KEY ("attr_value_id") REFERENCES "attr_values"("id");


ALTER TABLE "attr_values" ADD CONSTRAINT "attr_values_fk0" FOREIGN KEY ("attr_id") REFERENCES "attribute"("id");


