--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.1
-- Dumped by pg_dump version 9.6.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: attr_values; Type: TABLE; Schema: public; Owner: %user%
--

CREATE TABLE attr_values (
    id integer NOT NULL,
    attr_id bigint NOT NULL,
    attr_value text NOT NULL
);


ALTER TABLE attr_values OWNER TO %user%;

--
-- Name: attr_values_id_seq; Type: SEQUENCE; Schema: public; Owner: %user%
--

CREATE SEQUENCE attr_values_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attr_values_id_seq OWNER TO %user%;

--
-- Name: attr_values_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: %user%
--

ALTER SEQUENCE attr_values_id_seq OWNED BY attr_values.id;


--
-- Name: attribute; Type: TABLE; Schema: public; Owner: %user%
--

CREATE TABLE attribute (
    id integer NOT NULL,
    attr_name text NOT NULL
);


ALTER TABLE attribute OWNER TO %user%;

--
-- Name: attribute_id_seq; Type: SEQUENCE; Schema: public; Owner: %user%
--

CREATE SEQUENCE attribute_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attribute_id_seq OWNER TO %user%;

--
-- Name: attribute_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: %user%
--

ALTER SEQUENCE attribute_id_seq OWNED BY attribute.id;


--
-- Name: item; Type: TABLE; Schema: public; Owner: %user%
--

CREATE TABLE item (
    id integer NOT NULL,
    title text NOT NULL,
    data text NOT NULL
);


ALTER TABLE item OWNER TO %user%;

--
-- Name: item_attrs; Type: TABLE; Schema: public; Owner: %user%
--

CREATE TABLE item_attrs (
    id integer NOT NULL,
    item_id bigint NOT NULL,
    attr_value_id bigint NOT NULL
);


ALTER TABLE item_attrs OWNER TO %user%;

--
-- Name: item_attrs_id_seq; Type: SEQUENCE; Schema: public; Owner: %user%
--

CREATE SEQUENCE item_attrs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE item_attrs_id_seq OWNER TO %user%;

--
-- Name: item_attrs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: %user%
--

ALTER SEQUENCE item_attrs_id_seq OWNED BY item_attrs.id;


--
-- Name: item_id_seq; Type: SEQUENCE; Schema: public; Owner: %user%
--

CREATE SEQUENCE item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE item_id_seq OWNER TO %user%;

--
-- Name: item_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: %user%
--

ALTER SEQUENCE item_id_seq OWNED BY item.id;


--
-- Name: attr_values id; Type: DEFAULT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY attr_values ALTER COLUMN id SET DEFAULT nextval('attr_values_id_seq'::regclass);


--
-- Name: attribute id; Type: DEFAULT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY attribute ALTER COLUMN id SET DEFAULT nextval('attribute_id_seq'::regclass);


--
-- Name: item id; Type: DEFAULT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY item ALTER COLUMN id SET DEFAULT nextval('item_id_seq'::regclass);


--
-- Name: item_attrs id; Type: DEFAULT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY item_attrs ALTER COLUMN id SET DEFAULT nextval('item_attrs_id_seq'::regclass);


--
-- Data for Name: attr_values; Type: TABLE DATA; Schema: public; Owner: %user%
--

INSERT INTO attr_values VALUES (1, 1, 'Leo Tolstoy');
INSERT INTO attr_values VALUES (2, 1, 'David Foster Wallace');
INSERT INTO attr_values VALUES (3, 1, 'Octavia Butler');
INSERT INTO attr_values VALUES (4, 2, 'Science Fiction');
INSERT INTO attr_values VALUES (5, 2, 'Historical Fiction');


--
-- Name: attr_values_id_seq; Type: SEQUENCE SET; Schema: public; Owner: %user%
--

SELECT pg_catalog.setval('attr_values_id_seq', 5, true);


--
-- Data for Name: attribute; Type: TABLE DATA; Schema: public; Owner: %user%
--

INSERT INTO attribute VALUES (1, 'Author');
INSERT INTO attribute VALUES (2, 'Genre');


--
-- Name: attribute_id_seq; Type: SEQUENCE SET; Schema: public; Owner: %user%
--

SELECT pg_catalog.setval('attribute_id_seq', 2, true);


--
-- Data for Name: item; Type: TABLE DATA; Schema: public; Owner: %user%
--

INSERT INTO item VALUES (1, 'Parable of the Sower', 'texttexttext');
INSERT INTO item VALUES (2, 'Parable of the Talents', 'texttexttext');
INSERT INTO item VALUES (3, 'War and Peace', 'texttexttext');
INSERT INTO item VALUES (4, 'Infinite Jest', 'texttexttext');


--
-- Data for Name: item_attrs; Type: TABLE DATA; Schema: public; Owner: %user%
--

INSERT INTO item_attrs VALUES (1, 1, 3);
INSERT INTO item_attrs VALUES (2, 1, 4);
INSERT INTO item_attrs VALUES (3, 2, 3);
INSERT INTO item_attrs VALUES (4, 2, 4);
INSERT INTO item_attrs VALUES (5, 3, 1);
INSERT INTO item_attrs VALUES (6, 3, 5);
INSERT INTO item_attrs VALUES (7, 4, 2);
INSERT INTO item_attrs VALUES (8, 4, 5);


--
-- Name: item_attrs_id_seq; Type: SEQUENCE SET; Schema: public; Owner: %user%
--

SELECT pg_catalog.setval('item_attrs_id_seq', 8, true);


--
-- Name: item_id_seq; Type: SEQUENCE SET; Schema: public; Owner: %user%
--

SELECT pg_catalog.setval('item_id_seq', 4, true);


--
-- Name: attr_values attr_values_pk; Type: CONSTRAINT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY attr_values
    ADD CONSTRAINT attr_values_pk PRIMARY KEY (id);


--
-- Name: attribute attribute_pk; Type: CONSTRAINT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY attribute
    ADD CONSTRAINT attribute_pk PRIMARY KEY (id);


--
-- Name: item_attrs item_attrs_pk; Type: CONSTRAINT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_pk PRIMARY KEY (id);


--
-- Name: item item_pk; Type: CONSTRAINT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY item
    ADD CONSTRAINT item_pk PRIMARY KEY (id);


--
-- Name: attr_values attr_values_fk0; Type: FK CONSTRAINT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY attr_values
    ADD CONSTRAINT attr_values_fk0 FOREIGN KEY (attr_id) REFERENCES attribute(id);


--
-- Name: item_attrs item_attrs_fk0; Type: FK CONSTRAINT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_fk0 FOREIGN KEY (item_id) REFERENCES item(id);


--
-- Name: item_attrs item_attrs_fk1; Type: FK CONSTRAINT; Schema: public; Owner: %user%
--

ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_fk1 FOREIGN KEY (attr_value_id) REFERENCES attr_values(id);


--
-- PostgreSQL database dump complete
--

