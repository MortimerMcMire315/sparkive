SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';

SET search_path = public, pg_catalog;
SET default_tablespace = '';
SET default_with_oids = false;

CREATE TABLE attr_values (
    id integer NOT NULL,
    attr_id bigint NOT NULL,
    attr_value text NOT NULL
);

ALTER TABLE attr_values OWNER TO %user%;

CREATE SEQUENCE attr_values_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE attr_values_id_seq OWNER TO %user%;

ALTER SEQUENCE attr_values_id_seq OWNED BY attr_values.id;

CREATE TABLE attribute (
    id integer NOT NULL,
    attr_name text NOT NULL
);


ALTER TABLE attribute OWNER TO %user%;


CREATE SEQUENCE attribute_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attribute_id_seq OWNER TO %user%;


ALTER SEQUENCE attribute_id_seq OWNED BY attribute.id;



CREATE TABLE item (
    id integer NOT NULL,
    title text NOT NULL,
    data text NOT NULL
);


ALTER TABLE item OWNER TO %user%;


CREATE TABLE item_attrs (
    id integer NOT NULL,
    item_id bigint NOT NULL,
    attr_value_id bigint NOT NULL
);


ALTER TABLE item_attrs OWNER TO %user%;


CREATE SEQUENCE item_attrs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE item_attrs_id_seq OWNER TO %user%;


ALTER SEQUENCE item_attrs_id_seq OWNED BY item_attrs.id;



CREATE SEQUENCE item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE item_id_seq OWNER TO %user%;


ALTER SEQUENCE item_id_seq OWNED BY item.id;



CREATE TABLE sess_token (
    id integer NOT NULL,
    username integer NOT NULL,
    token integer NOT NULL
);


ALTER TABLE sess_token OWNER TO %user%;


CREATE SEQUENCE sess_token_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE sess_token_id_seq OWNER TO %user%;


ALTER SEQUENCE sess_token_id_seq OWNED BY sess_token.id;



CREATE SEQUENCE sess_token_token_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE sess_token_token_seq OWNER TO %user%;


ALTER SEQUENCE sess_token_token_seq OWNED BY sess_token.token;



CREATE SEQUENCE sess_token_username_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE sess_token_username_seq OWNER TO %user%;


ALTER SEQUENCE sess_token_username_seq OWNED BY sess_token.username;



CREATE TABLE sparkive_user (
    id integer NOT NULL,
    username text NOT NULL,
    pass bytea NOT NULL,
    salt bytea NOT NULL
);


ALTER TABLE sparkive_user OWNER TO %user%;


CREATE SEQUENCE sparkive_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE sparkive_user_id_seq OWNER TO %user%;


ALTER SEQUENCE sparkive_user_id_seq OWNED BY sparkive_user.id;



ALTER TABLE ONLY attr_values ALTER COLUMN id SET DEFAULT nextval('attr_values_id_seq'::regclass);



ALTER TABLE ONLY attribute ALTER COLUMN id SET DEFAULT nextval('attribute_id_seq'::regclass);



ALTER TABLE ONLY item ALTER COLUMN id SET DEFAULT nextval('item_id_seq'::regclass);



ALTER TABLE ONLY item_attrs ALTER COLUMN id SET DEFAULT nextval('item_attrs_id_seq'::regclass);



ALTER TABLE ONLY sess_token ALTER COLUMN id SET DEFAULT nextval('sess_token_id_seq'::regclass);



ALTER TABLE ONLY sess_token ALTER COLUMN username SET DEFAULT nextval('sess_token_username_seq'::regclass);



ALTER TABLE ONLY sess_token ALTER COLUMN token SET DEFAULT nextval('sess_token_token_seq'::regclass);



ALTER TABLE ONLY sparkive_user ALTER COLUMN id SET DEFAULT nextval('sparkive_user_id_seq'::regclass);






SELECT pg_catalog.setval('attr_values_id_seq', 1, false);






SELECT pg_catalog.setval('attribute_id_seq', 1, false);









SELECT pg_catalog.setval('item_attrs_id_seq', 1, false);



SELECT pg_catalog.setval('item_id_seq', 1, false);






SELECT pg_catalog.setval('sess_token_id_seq', 1, false);



SELECT pg_catalog.setval('sess_token_token_seq', 1, false);



SELECT pg_catalog.setval('sess_token_username_seq', 1, false);






SELECT pg_catalog.setval('sparkive_user_id_seq', 1, false);


INSERT INTO attr_values VALUES (1, 1, 'Leo Tolstoy');
INSERT INTO attr_values VALUES (2, 1, 'David Foster Wallace');
INSERT INTO attr_values VALUES (3, 1, 'Octavia Butler');
INSERT INTO attr_values VALUES (4, 2, 'Science Fiction');
INSERT INTO attr_values VALUES (5, 2, 'Historical Fiction');

INSERT INTO attribute VALUES (1, 'Author');
INSERT INTO attribute VALUES (2, 'Genre');

INSERT INTO item VALUES (1, 'Parable of the Sower', 'texttexttext');
INSERT INTO item VALUES (2, 'Parable of the Talents', 'texttexttext');
INSERT INTO item VALUES (3, 'War and Peace', 'texttexttext');
INSERT INTO item VALUES (4, 'Infinite Jest', 'texttexttext');

INSERT INTO item_attrs VALUES (1, 1, 3);
INSERT INTO item_attrs VALUES (2, 1, 4);
INSERT INTO item_attrs VALUES (3, 2, 3);
INSERT INTO item_attrs VALUES (4, 2, 4);
INSERT INTO item_attrs VALUES (5, 3, 1);
INSERT INTO item_attrs VALUES (6, 3, 5);
INSERT INTO item_attrs VALUES (7, 4, 2);
INSERT INTO item_attrs VALUES (8, 4, 5);

ALTER TABLE ONLY attr_values
    ADD CONSTRAINT attr_values_pk PRIMARY KEY (id);



ALTER TABLE ONLY attribute
    ADD CONSTRAINT attribute_pk PRIMARY KEY (id);



ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_pk PRIMARY KEY (id);



ALTER TABLE ONLY item
    ADD CONSTRAINT item_pk PRIMARY KEY (id);



ALTER TABLE ONLY sess_token
    ADD CONSTRAINT sess_token_pk PRIMARY KEY (id);



ALTER TABLE ONLY sparkive_user
    ADD CONSTRAINT sparkive_user_pk PRIMARY KEY (id);



ALTER TABLE ONLY sparkive_user
    ADD CONSTRAINT sparkive_user_username_key UNIQUE (username);



ALTER TABLE ONLY attr_values
    ADD CONSTRAINT attr_values_fk0 FOREIGN KEY (attr_id) REFERENCES attribute(id);



ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_fk0 FOREIGN KEY (item_id) REFERENCES item(id);



ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_fk1 FOREIGN KEY (attr_value_id) REFERENCES attr_values(id);



