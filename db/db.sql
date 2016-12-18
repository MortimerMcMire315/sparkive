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
-- Name: attr_values; Type: TABLE; Schema: public; Owner: sayoder
--

CREATE TABLE attr_values (
    id integer NOT NULL,
    attr_id bigint NOT NULL,
    attr_value text NOT NULL
);


ALTER TABLE attr_values OWNER TO sayoder;

--
-- Name: attr_values_id_seq; Type: SEQUENCE; Schema: public; Owner: sayoder
--

CREATE SEQUENCE attr_values_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attr_values_id_seq OWNER TO sayoder;

--
-- Name: attr_values_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sayoder
--

ALTER SEQUENCE attr_values_id_seq OWNED BY attr_values.id;


--
-- Name: attribute; Type: TABLE; Schema: public; Owner: sayoder
--

CREATE TABLE attribute (
    id integer NOT NULL,
    attr_name text NOT NULL
);


ALTER TABLE attribute OWNER TO sayoder;

--
-- Name: attribute_id_seq; Type: SEQUENCE; Schema: public; Owner: sayoder
--

CREATE SEQUENCE attribute_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attribute_id_seq OWNER TO sayoder;

--
-- Name: attribute_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sayoder
--

ALTER SEQUENCE attribute_id_seq OWNED BY attribute.id;


--
-- Name: item; Type: TABLE; Schema: public; Owner: sayoder
--

CREATE TABLE item (
    id integer NOT NULL,
    title text NOT NULL,
    data text NOT NULL
);


ALTER TABLE item OWNER TO sayoder;

--
-- Name: item_attrs; Type: TABLE; Schema: public; Owner: sayoder
--

CREATE TABLE item_attrs (
    id integer NOT NULL,
    item_id bigint NOT NULL,
    attr_value_id bigint NOT NULL
);


ALTER TABLE item_attrs OWNER TO sayoder;

--
-- Name: item_attrs_id_seq; Type: SEQUENCE; Schema: public; Owner: sayoder
--

CREATE SEQUENCE item_attrs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE item_attrs_id_seq OWNER TO sayoder;

--
-- Name: item_attrs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sayoder
--

ALTER SEQUENCE item_attrs_id_seq OWNED BY item_attrs.id;


--
-- Name: item_id_seq; Type: SEQUENCE; Schema: public; Owner: sayoder
--

CREATE SEQUENCE item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE item_id_seq OWNER TO sayoder;

--
-- Name: item_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sayoder
--

ALTER SEQUENCE item_id_seq OWNED BY item.id;


--
-- Name: attr_values id; Type: DEFAULT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY attr_values ALTER COLUMN id SET DEFAULT nextval('attr_values_id_seq'::regclass);


--
-- Name: attribute id; Type: DEFAULT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY attribute ALTER COLUMN id SET DEFAULT nextval('attribute_id_seq'::regclass);


--
-- Name: item id; Type: DEFAULT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY item ALTER COLUMN id SET DEFAULT nextval('item_id_seq'::regclass);


--
-- Name: item_attrs id; Type: DEFAULT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY item_attrs ALTER COLUMN id SET DEFAULT nextval('item_attrs_id_seq'::regclass);


--
-- Data for Name: attr_values; Type: TABLE DATA; Schema: public; Owner: sayoder
--

COPY attr_values (id, attr_id, attr_value) FROM stdin;
1	1	Leo Tolstoy
2	1	David Foster Wallace
3	1	Octavia Butler
4	2	Science Fiction
5	2	Historical Fiction
\.


--
-- Name: attr_values_id_seq; Type: SEQUENCE SET; Schema: public; Owner: sayoder
--

SELECT pg_catalog.setval('attr_values_id_seq', 5, true);


--
-- Data for Name: attribute; Type: TABLE DATA; Schema: public; Owner: sayoder
--

COPY attribute (id, attr_name) FROM stdin;
1	Author
2	Genre
\.


--
-- Name: attribute_id_seq; Type: SEQUENCE SET; Schema: public; Owner: sayoder
--

SELECT pg_catalog.setval('attribute_id_seq', 2, true);


--
-- Data for Name: item; Type: TABLE DATA; Schema: public; Owner: sayoder
--

COPY item (id, title, data) FROM stdin;
1	Parable of the Sower	texttexttext
2	Parable of the Talents	texttexttext
3	War and Peace	texttexttext
4	Infinite Jest	texttexttext
\.


--
-- Data for Name: item_attrs; Type: TABLE DATA; Schema: public; Owner: sayoder
--

COPY item_attrs (id, item_id, attr_value_id) FROM stdin;
1	1	3
2	1	4
3	2	3
4	2	4
5	3	1
6	3	5
7	4	2
8	4	5
\.


--
-- Name: item_attrs_id_seq; Type: SEQUENCE SET; Schema: public; Owner: sayoder
--

SELECT pg_catalog.setval('item_attrs_id_seq', 8, true);


--
-- Name: item_id_seq; Type: SEQUENCE SET; Schema: public; Owner: sayoder
--

SELECT pg_catalog.setval('item_id_seq', 4, true);


--
-- Name: attr_values attr_values_pk; Type: CONSTRAINT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY attr_values
    ADD CONSTRAINT attr_values_pk PRIMARY KEY (id);


--
-- Name: attribute attribute_pk; Type: CONSTRAINT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY attribute
    ADD CONSTRAINT attribute_pk PRIMARY KEY (id);


--
-- Name: item_attrs item_attrs_pk; Type: CONSTRAINT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_pk PRIMARY KEY (id);


--
-- Name: item item_pk; Type: CONSTRAINT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY item
    ADD CONSTRAINT item_pk PRIMARY KEY (id);


--
-- Name: attr_values attr_values_fk0; Type: FK CONSTRAINT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY attr_values
    ADD CONSTRAINT attr_values_fk0 FOREIGN KEY (attr_id) REFERENCES attribute(id);


--
-- Name: item_attrs item_attrs_fk0; Type: FK CONSTRAINT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_fk0 FOREIGN KEY (item_id) REFERENCES item(id);


--
-- Name: item_attrs item_attrs_fk1; Type: FK CONSTRAINT; Schema: public; Owner: sayoder
--

ALTER TABLE ONLY item_attrs
    ADD CONSTRAINT item_attrs_fk1 FOREIGN KEY (attr_value_id) REFERENCES attr_values(id);


--
-- PostgreSQL database dump complete
--

