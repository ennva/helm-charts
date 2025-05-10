--
-- PostgreSQL database cluster dump
--

SET default_transaction_read_only = off;

SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;

--
-- Roles
--

CREATE ROLE dbmonitor;
ALTER ROLE dbmonitor WITH SUPERUSER INHERIT NOCREATEROLE NOCREATEDB LOGIN NOREPLICATION NOBYPASSRLS PASSWORD 'SCRAM-SHA-256$4096:w5tZaJU0PpgFFseF/E1vOQ==$AgHOurnESosCVylqygdXDQIXMEcrmQvWrw5tBxs+fLQ=:qfXbbSaAYAt3uOdHaGosYC9GjnYfH0l/QQga7AALMPI=';
CREATE ROLE hive;
ALTER ROLE hive WITH NOSUPERUSER INHERIT NOCREATEROLE NOCREATEDB LOGIN NOREPLICATION NOBYPASSRLS PASSWORD 'SCRAM-SHA-256$4096:xfKK3G3DzgCHiML13qcBLQ==$w42l3r8qXYATsWjvE0Iq3EQMoyg4YspcjdLGD2Kmwi8=:/L5kTgcQBT47JEMBNjBPfWUWMfTzd2k2t80nEhoT9m0=';
CREATE ROLE postgres;
ALTER ROLE postgres WITH SUPERUSER INHERIT CREATEROLE CREATEDB LOGIN REPLICATION BYPASSRLS PASSWORD 'SCRAM-SHA-256$4096:kzdvDUHhTQAjTq0r8Vv8bg==$DHfr2DpPJog48O6f3M08SIWxF8bs6xFYYkpgN2mC6xk=:o8ioHVqGCdkQK/54/Kdf2sB/IUBe7sFZ0+K3JJXSvO4=';

--
-- User Configurations
--


--
-- Role memberships
--

GRANT postgres TO dbmonitor WITH INHERIT TRUE GRANTED BY postgres;






--
-- Databases
--

--
-- Database "template1" dump
--

\connect template1

--
-- PostgreSQL database dump
--

-- Dumped from database version 16.9 (Debian 16.9-1.pgdg120+1)
-- Dumped by pg_dump version 16.9 (Debian 16.9-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- PostgreSQL database dump complete
--

--
-- Database "hive" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 16.9 (Debian 16.9-1.pgdg120+1)
-- Dumped by pg_dump version 16.9 (Debian 16.9-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: hive; Type: DATABASE; Schema: -; Owner: hive
--

CREATE DATABASE hive WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE_PROVIDER = libc LOCALE = 'en_US.utf8';


ALTER DATABASE hive OWNER TO hive;

\connect hive

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: DATABASE hive; Type: ACL; Schema: -; Owner: hive
--

REVOKE CONNECT,TEMPORARY ON DATABASE hive FROM PUBLIC;
GRANT TEMPORARY ON DATABASE hive TO PUBLIC;


--
-- PostgreSQL database dump complete
--

--
-- Database "postgres" dump
--

\connect postgres

--
-- PostgreSQL database dump
--

-- Dumped from database version 16.9 (Debian 16.9-1.pgdg120+1)
-- Dumped by pg_dump version 16.9 (Debian 16.9-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pg_stat_statements; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pg_stat_statements WITH SCHEMA public;


--
-- Name: EXTENSION pg_stat_statements; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pg_stat_statements IS 'track planning and execution statistics of all SQL statements executed';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: array_subtract(anyarray, anyarray); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.array_subtract(minuend anyarray, subtrahend anyarray, OUT difference anyarray) RETURNS anyarray
    LANGUAGE plpgsql STRICT
    AS $_$
begin
    execute 'select array(select unnest($1) except select unnest($2))'
      using minuend, subtrahend
       into difference;
end;
$_$;


ALTER FUNCTION public.array_subtract(minuend anyarray, subtrahend anyarray, OUT difference anyarray) OWNER TO postgres;

--
-- Name: clone_schema(text, text, boolean, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.clone_schema(source_schema text, dest_schema text, copy_data boolean DEFAULT false, _verbose boolean DEFAULT false) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
    sequence_owner_info jsonb[];
    table_objects jsonb[];
    fk_objects jsonb[];
    view_objects jsonb[];
    function_objects jsonb[];
    trigger_objects jsonb[];
    rule_objects jsonb[];
    comment_objects jsonb[];
    jobject jsonb;
    src_schema text;
    dst_schema text;
    source_obj text;
    dest_obj text;
    ix_stmt text;
BEGIN
    dst_schema = quote_ident(dest_schema);
    src_schema = quote_ident(source_schema);

    /* Check if source schema exists */
    PERFORM oid
       FROM pg_namespace
      WHERE nspname = source_schema;
    IF NOT FOUND
    THEN
        RAISE WARNING 'Source schema % does not exist.', src_schema;
        RETURN false;
    END IF;

    /* Check if dest schema exists */
    PERFORM oid
       FROM pg_namespace
      WHERE nspname = dest_schema;
    IF FOUND
    THEN
        RAISE INFO 'Destination schema % already exists.', dst_schema;
        RETURN false;
    END IF;

    SET LOCAL search_path = public;

    /*
     * Gather data for copy
     */

    /* Sequence owner info */
    IF _verbose THEN
        RAISE INFO 'Gathering sequence owner data from %...', source_schema;
    END IF;

    SELECT coalesce(array_agg(
               jsonb_build_object(
                   'sequence_name', s.relname::text,
                   'owner_object', o.relname::text,
                   'owner_column', a.attname::text
               )
           ), '{}'::jsonb[])
      INTO sequence_owner_info
      FROM pg_class s
      JOIN pg_sequence ps
        ON ps.seqrelid = s.oid
      JOIN pg_depend d
        ON d.objid = s.oid
      JOIN pg_attribute a
        ON a.attrelid = d.refobjid
       AND a.attnum = d.refobjsubid
      JOIN pg_class o
        ON o.oid = d.refobjid
     WHERE s.relkind = 'S'
       AND o.relnamespace = source_schema::regnamespace
       AND NOT o.relispartition;

    IF _verbose THEN
        RAISE INFO '    Got %s schema owner objects...', cardinality(sequence_owner_info);
    END IF;

    /* Table objects */
    IF _verbose THEN
        RAISE INFO 'Gathering table object data from %...', source_schema;
    END IF;

    SELECT coalesce(array_agg(
               jsonb_build_object(
                   'obj_id', t.oid,
                   'table_name', t.relname::text,
                   'table_kind', t.relkind::text,
                   'partition_type', CASE pt2.partstrat
                                          WHEN 'h' THEN 'HASH'
                                          WHEN 'l' THEN 'LIST'
                                          WHEN 'r' THEN 'RANGE'
                                          ELSE NULL
                                     END::text,
                   'partition_key', pk.attname::text,
                   'is_partition', t.relispartition,
                   'partitioned_table', p.relname,
                   'partition_expr', pg_get_expr(t.relpartbound, t.oid)
              )
              ORDER BY t.relkind, t.relispartition
           ), '{}'::jsonb[])
      INTO table_objects
      FROM pg_class t
      LEFT
      JOIN pg_inherits h
        ON h.inhrelid = t.oid
      LEFT
      JOIN pg_partitioned_table pt
        ON pt.partrelid = h.inhparent
      LEFT
      JOIN pg_class p
        ON p.oid = pt.partrelid
      LEFT
      JOIN pg_partitioned_table pt2
        ON pt2.partrelid = t.oid
      LEFT
      JOIN pg_attribute pk
        ON pk.attrelid = t.oid
       AND pk.attnum = pt2.partattrs::text::int2
     WHERE t.relnamespace = source_schema::regnamespace
       AND t.relkind in ('r', 'p');

    IF _verbose THEN
        RAISE INFO '    Got %s table objects...', cardinality(table_objects);
    END IF;

    /* Foreign Key objects */
    IF _verbose THEN
        RAISE INFO 'Gathering foreign key constraint data from %...', source_schema;
    END IF;

    SELECT coalesce(array_agg(
               jsonb_build_object(
                   'table_name', rn.relname,
                   'constraint_name', ct.conname,
                   'alter_stmt', 'ALTER TABLE ' || dst_schema || '.' || quote_ident(rn.relname) ||
                                     ' ADD CONSTRAINT ' || quote_ident(ct.conname) || ' ' ||
                                     replace(pg_get_constraintdef(ct.oid), source_schema || '.', dst_schema || '.') ||
                                     ' ;'
               )
           ), '{}'::jsonb[])
    INTO fk_objects
    FROM pg_constraint ct
    JOIN pg_class rn
        ON rn.oid = ct.conrelid
    WHERE connamespace = source_schema::regnamespace
    AND rn.relkind in ('r', 'p')
    AND NOT rn.relispartition
    AND ct.contype = 'f';

    IF _verbose THEN
        RAISE INFO '    Got %s foreign key objects...', cardinality(fk_objects);
    END IF;

    /* View objects */
    IF _verbose THEN
        RAISE INFO 'Gathering view object data from %...', source_schema;
    END IF;

    WITH RECURSIVE view_deps as (
    SELECT DISTINCT
           0 as depth,
           v.oid as view_oid,
           v.relname::text as view_name,
           v.relkind as view_kind,
           v.oid as dep_obj_id,
           v.relname::text as dep_obj_name,
           v.relkind as deb_obj_kind
      FROM pg_class v
     WHERE v.relnamespace = source_schema::regnamespace
       AND v.relkind IN ('v', 'm')
       AND NOT EXISTS (
                        SELECT 1 as x
                          FROM pg_depend d
                          JOIN pg_class c
                            ON c.oid = d.objid
                           AND c.relkind in ('m', 'v')
                         WHERE d.refobjid = v.oid
                      )
     UNION
    SELECT DISTINCT
           rv.depth + 1 as "depth",
           dv.oid as view_oid,
           dv.relname as view_name,
           dv.relkind as view_kind,
           rv.view_oid as ref_view_oid,
           rv.view_name as ref_view_name,
           rv.view_kind as ref_view_kind
      FROM pg_class dv
      JOIN pg_depend pd
        ON pd.objid = dv.oid
      JOIN view_deps as rv
        ON rv.view_oid = pd.refobjid
     WHERE dv.relnamespace = source_schema::regnamespace
       AND dv.relkind in ('m', 'v')
    ),
    base_view_def as (
    SELECT *,
           replace(pg_get_viewdef(view_oid), source_schema || '.', dst_schema || '.') as "view_def"
      FROM view_deps
    )
    SELECT coalesce(array_agg(
               jsonb_build_object(
                   'view_name', bvd.view_name,
                   'depth', bvd.depth,
                   'view_kind', CASE WHEN bvd.view_kind = 'm'
                                          THEN 'MATERIALIZED VIEW'
                                     ELSE 'VIEW'
                                END::text,
                   'view_def', CASE WHEN bvd.view_kind = 'm'
                                         THEN substr(bvd.view_def, 1, length(bvd.view_def) - 1) || ' WITH DATA;'
                                    ELSE bvd.view_def
                               END::text,
                   'view_indexes', COALESCE((SELECT to_jsonb(array_to_json(array_agg(replace(pg_get_indexdef(i.indexrelid),
                                                                                     source_schema || '.',
                                                                                     dst_schema || '.'))))
                                               FROM pg_index i
                                              WHERE i.indrelid = bvd.view_oid),
                                            jsonb_build_array())
               )
               order by bvd.depth
           ), '{}'::jsonb[])
      INTO view_objects
      FROM base_view_def bvd;

    IF _verbose THEN
        RAISE INFO '    Got %s view objects...', cardinality(view_objects);
    END IF;

    /* Function objects */
    IF _verbose THEN
        RAISE INFO 'Gathering function/procedure object data from %...', source_schema;
    END IF;

    SELECT coalesce(array_agg(
               jsonb_build_object(
                   'func_name', proname,
                   'func_type', CASE prokind
                                     WHEN 'p' THEN 'PROCEDURE'
                                     WHEN 'f' THEN 'FUNCTION'
                                     WHEN 'a' THEN 'AGGREGATE'
                                     WHEN 'w' THEN 'WINDOW'
                                     ELSE 'UNKNOWN'
                                END::text,
                   'func_stmt', replace(pg_get_functiondef(oid), source_schema || '.', dst_schema || '.')
               )
           ), '{}'::jsonb[])
    INTO function_objects
    FROM pg_proc
    WHERE pronamespace = source_schema::regnamespace;

    IF _verbose THEN
        RAISE INFO '    Got %s function/procedure objects...', cardinality(function_objects);
    END IF;

    /* Trigger objects */
    IF _verbose THEN
        RAISE INFO 'Gathering trigger object data from %...', source_schema;
    END IF;

    SELECT coalesce(array_agg(
               jsonb_build_object(
                   'trigger_id', t.oid,
                   'trigger_name', t.tgname::text,
                   'table_name', c.relname::text,
                   'trigger_def', replace(pg_get_triggerdef(t.oid), source_schema || '.', dst_schema || '.')
               )
           ), '{}'::jsonb[])
      INTO trigger_objects
      FROM pg_trigger t
      JOIN pg_class c
        ON c.oid = t.tgrelid
       AND NOT c.relispartition
     WHERE c.relnamespace = source_schema::regnamespace
       AND t.tgconstraint = 0;

    IF _verbose THEN
        RAISE INFO '    Got %s trigger objects...', cardinality(trigger_objects);
    END IF;

    /* Rule objects */
    IF _verbose THEN
        RAISE INFO 'Gathering rule object data from %...', source_schema;
    END IF;

    SELECT coalesce(array_agg(
               jsonb_build_object(
                   'tablename', tablename,
                   'rulename', rulename,
                   'rule_def', replace(definition, source_schema || '.', dst_schema || '.')
               )
           ), '{}'::jsonb[])
    INTO rule_objects
    FROM pg_rules
    WHERE schemaname = source_schema;

    IF _verbose THEN
        RAISE INFO '    Got %s rule objects...', cardinality(rule_objects);
    END IF;

    /* Comment objects */
    IF _verbose THEN
        RAISE INFO 'Gathering object comment data from %...', source_schema;
    END IF;

    select coalesce(array_agg(
               jsonb_build_object(
                   'oid', t.oid,
                   'attnum', coalesce(c.attnum, -1),
                   'relkind', t.relkind,
                   'table_name', quote_ident(t.relname::text),
                   'dot', case when c.attname is not null then '.' else '' end::text,
                   'column_name', case when c.attname is not null then quote_ident(c.attname) else '' end::text,
                   'comment_type', case when c.attname is null
                                             then case t.relkind
                                                       when 'm' then 'MATERIALIZED VIEW'
                                                       when 'v' then 'VIEW'
                                                       else 'TABLE'
                                                  end::text
                                        else 'COLUMN'
                                   end::text,
                   'description', d.description
               )
               order by t.oid, coalesce(c.attnum, -1)
           ), '{}'::jsonb[])
      into comment_objects
      from pg_description d
      join pg_class t
        on t.oid = d.objoid
      left
      join pg_attribute c
        on c.attrelid = t.oid
       and c.attnum = d.objsubid
     where t.relnamespace = source_schema::regnamespace
       and t.relkind = any('{r,p,v,m}'::text[]);

    IF _verbose THEN
        RAISE INFO '    Got %s comment objects...', cardinality(comment_objects);
    END IF;

    /*
     * ======================================================================
     */

    /*
     * Create the new schema
     */
    IF _verbose
    THEN
        RAISE INFO 'Creating schema %', dst_schema;
    END IF;
    EXECUTE 'CREATE SCHEMA ' || dst_schema || ' ;';

    /*
     * Create tables
     */
    IF cardinality(table_objects) > 0
    THEN
        IF _verbose
        THEN
            RAISE INFO 'Creating tables for %', dst_schema;
        END IF;
        FOREACH jobject IN ARRAY table_objects
        LOOP
            dest_obj = dst_schema || '.' || quote_ident(jobject->>'table_name'::text);
            source_obj = src_schema || '.' || quote_ident(jobject->>'table_name'::text);

            IF jobject->>'table_kind' = 'p'::text
            THEN
                IF _verbose
                THEN
                    RAISE INFO '    % (partitioned table)', dest_obj;
                END IF;
                EXECUTE FORMAT('CREATE TABLE IF NOT EXISTS %s (LIKE %s INCLUDING ALL) PARTITION BY %s ( %I ) ;',
                            dest_obj,
                            source_obj,
                            jobject->>'partition_type'::text,
                            jobject->>'partition_key'::text);
            ELSIF (jobject->>'is_partition'::text):: boolean
            THEN
                IF _verbose
                THEN
                    RAISE INFO '    % (table partition)', dest_obj;
                END IF;
                EXECUTE FORMAT('CREATE TABLE IF NOT EXISTS %s PARTITION OF %s.%I %s ;',
                            dest_obj,
                            dst_schema,
                            jobject->>'partitioned_table'::text,
                            jobject->>'partition_expr'::text);
            ELSE
                IF _verbose
                THEN
                    RAISE INFO '    % (table)', dest_obj;
                END IF;
                EXECUTE FORMAT('CREATE TABLE IF NOT EXISTS %s (LIKE %s INCLUDING ALL) ;',
                            dest_obj,
                            source_obj);
            END IF;

            IF (copy_data OR
                (jobject->>'table_name' ~ 'partitioned_tables'::text) OR
                (jobject->>'table_name' ~ 'django_migrations'::text)) AND
            (jobject->>'table_kind' = 'r'::text)
            THEN
                IF _verbose
                THEN
                    RAISE INFO '        Copying data...';
                END IF;
                EXECUTE FORMAT('INSERT INTO %s SELECT * FROM %s ;',
                            dest_obj,
                            source_obj);
            END IF;

            IF jobject->>'table_name' = 'partitioned_tables'::text
            THEN
                IF _verbose
                THEN
                    RAISE INFO '        Update partitioned_tables schema data';
                END IF;
                EXECUTE FORMAT('UPDATE %s SET schema_name = %L ;',
                            dest_obj,
                            dest_schema);
            END IF;
        END LOOP;
    ELSE
        IF _verbose
        THEN
            RAISE INFO 'No tables for %', dst_schema;
        END IF;
    END IF;

    /*
     * Set sequence value
     */
    IF cardinality(sequence_owner_info) > 0
    THEN
        IF _verbose
        THEN
            RAISE INFO 'Set current value for sequence objects in %', dst_schema;
        END IF;
        FOREACH jobject IN ARRAY sequence_owner_info
        LOOP
            IF _verbose
            THEN
                RAISE INFO '    Update sequence value for %.%', dst_schema, quote_ident(jobject->>'owner_object'::text);
            END IF;

            EXECUTE FORMAT('SELECT setval(''%s.%I'', (SELECT max(%I) FROM %s.%I));',
                        dst_schema,
                        jobject->>'sequence_name'::text,
                        jobject->>'owner_column'::text,
                        dst_schema,
                        jobject->>'owner_object'::text);
        END LOOP;
    ELSE
        IF _verbose
        THEN
            RAISE INFO 'No sequence owner data for %', dst_schema;
        END IF;
    END IF;

    /*
     * Create Foreign Key Constraints
     */
    IF cardinality(fk_objects) > 0
    THEN
        IF _verbose
        THEN
            RAISE INFO 'Create foriegn key constraints for tables in "%"', dst_schema;
        END IF;
        FOREACH jobject IN ARRAY fk_objects
        LOOP
            IF _verbose
            THEN
                RAISE INFO '    %.%', jobject->>'table_name', jobject->>'constraint_name'::text;
            END IF;
            EXECUTE jobject->>'alter_stmt'::text;
        END LOOP;
    ELSE
        IF _verbose
        THEN
            RAISE INFO 'No foreign key constraints for %', dst_schema;
        END IF;
    END IF;

    /*
     * Create Views
     */
    IF cardinality(view_objects) > 0
    THEN
        IF _verbose
        THEN
            RAISE INFO 'Creating views for %', dst_schema;
        END IF;
        FOREACH jobject IN ARRAY view_objects
        LOOP
            IF _verbose
            THEN
                RAISE INFO '    %: "%"', jobject->>'view_kind', jobject->>'view_name'::text;
            END IF;
            EXECUTE FORMAT('CREATE %s %s.%I AS %s',
                        jobject->>'view_kind'::text,
                        dst_schema,
                        jobject->>'view_name'::text,
                        jobject->>'view_def'::text);

            IF jsonb_array_length(jobject->'view_indexes') > 0
            THEN
                IF _verbose
                THEN
                    RAISE INFO '        Create indexes';
                END IF;
                FOR ix_stmt IN select jsonb_array_elements_text(jobject->'view_indexes')
                LOOP
                    EXECUTE ix_stmt;
                END LOOP;
            END IF;
        END LOOP;
    ELSE
        IF _verbose
        THEN
            RAISE INFO 'No view objects for %', dst_schema;
        END IF;
    END IF;

    /*
     * Create functions
     */
    IF cardinality(function_objects) > 0
    THEN
        IF _verbose
        THEN
            RAISE INFO 'Create functions, procedures for "%"', dst_schema;
        END IF;
        FOREACH jobject IN ARRAY function_objects
        LOOP
            IF _verbose
            THEN
                RAISE INFO '    "%" "%"', jobject->>'func_type', jobject->>'func_name'::text;
            END IF;
            EXECUTE jobject->>'func_stmt'::text;
        END LOOP;
    ELSE
        IF _verbose
        THEN
            RAISE INFO 'No function/procedure objects for %', dst_schema;
        END IF;
    END IF;

    /*
     * Create triggers
     */
    IF cardinality(trigger_objects) > 0
    THEN
        IF _verbose
        THEN
            RAISE INFO 'Create triggers on objects in "%"', dst_schema;
        END IF;
        FOREACH jobject IN ARRAY trigger_objects
        LOOP
            IF _verbose
            THEN
                RAISE INFO '    "%"."%"', jobject->>'table_name', jobject->>'trigger_name'::text;
            END IF;
            EXECUTE jobject->>'trigger_def'::text;
        END LOOP;
    ELSE
        IF _verbose
        THEN
            RAISE INFO 'No trigger objects for %', dst_schema;
        END IF;
    END IF;

    /*
     *  Create rules
     */
    IF cardinality(rule_objects) > 0
    THEN
        IF _verbose
        THEN
            RAISE INFO 'Creating rules on objects in %', dst_schema;
        END IF;
        FOREACH jobject IN ARRAY rule_objects
        LOOP
            IF _verbose
            THEN
                RAISE INFO '    RULE "%" on "%"', jobject->>'rulename', jobject->>'tablename'::text;
            END IF;
            EXECUTE jobject->>'rule_def'::text;
        END LOOP;
    ELSE
        IF _verbose
        THEN
            RAISE INFO 'No rule objects for %', dst_schema;
        END IF;
    END IF;

    /*
     * Create comments
     */
    IF cardinality(comment_objects) > 0
    THEN
        IF _verbose
        THEN
            RAISE INFO 'Creating comments on objects in %', dst_schema;
        END IF;
        FOREACH jobject IN ARRAY comment_objects
        LOOP
            IF _verbose AND ((jobject->>'attnum')::int = -1)
            THEN
                RAISE INFO '    % % %', jobject->>'comment_type', jobject->>'table_name', jobject->>'column_name';
            END IF;
            EXECUTE FORMAT('COMMENT ON %s %s.%s%s%s IS %L ;',
                        jobject->>'comment_type'::text,
                        dst_schema,
                        jobject->>'table_name',
                        jobject->>'dot',
                        jobject->>'column_name'::text,
                        jobject->>'description'::text);
        END LOOP;
    ELSE
        IF _verbose
        THEN
            RAISE INFO 'No comments on objects for %', dst_schema;
        END IF;
    END IF;

    RETURN true;
END;
$$;


ALTER FUNCTION public.clone_schema(source_schema text, dest_schema text, copy_data boolean, _verbose boolean) OWNER TO postgres;

--
-- Name: create_date_partitions(text, text, text, text, boolean); Type: PROCEDURE; Schema: public; Owner: postgres
--

CREATE PROCEDURE public.create_date_partitions(IN check_table text, IN check_col text, IN schema text, IN partitioned_table text, IN _commit boolean DEFAULT false)
    LANGUAGE plpgsql
    AS $$
DECLARE
    rec record;
    table_parts text[];
    check_table_name text;
    partition_name text  = '';
    check_stmt text = '';
BEGIN
    table_parts = string_to_array(check_table, '.');
    IF ( cardinality(table_parts) > 1 )
    THEN
        check_table_name = quote_ident(table_parts[1]) || '.'::text || quote_ident(table_parts[2]);
    ELSE
        check_table_name = quote_ident(table_parts[1]);
    END IF;

    check_stmt = 'WITH distinct_date_key as (' ||
                    'SELECT DISTINCT ' ||
                    '       to_char(' || quote_ident(check_col) || ', ''YYYY-MM-01'')::text as date_key' ||
                    '  FROM ' || check_table_name || ' ' ||
                    ') ' ||
                    'SELECT ddk.date_key::date' ||
                    '  FROM distinct_date_key as ddk ' ||
                    ' WHERE NOT EXISTS (SELECT 1 ' ||
                    '                     FROM ' ||
                                            quote_ident(schema) || '."partitioned_tables" ' ||
                    '                    WHERE schema_name = ' || quote_literal(schema) ||
                    '                      AND partition_of_table_name = ' || quote_literal(partitioned_table) ||
                    '                      AND partition_type = ''range'' ' ||
                    '                      AND ddk.date_key = (partition_parameters->>''from'') ) ;';
    FOR rec IN EXECUTE check_stmt
    LOOP
        -- Create the new partition
        partition_name = partitioned_table || '_' || to_char(rec.date_key, 'YYYY_MM');
        CALL public.create_table_date_range_partition(
            schema,
            partition_name,
            partitioned_table,
            rec.date_key::date,
            (rec.date_key::date + '1 month'::interval)::date
        );
        END LOOP;

    IF (_commit = true) AND (partition_name != '')
    THEN
        COMMIT;
    END IF;
END;
$$;


ALTER PROCEDURE public.create_date_partitions(IN check_table text, IN check_col text, IN schema text, IN partitioned_table text, IN _commit boolean) OWNER TO postgres;

--
-- Name: create_table_date_range_partition(text, text, text, date, date, boolean, boolean); Type: PROCEDURE; Schema: public; Owner: postgres
--

CREATE PROCEDURE public.create_table_date_range_partition(IN schema text, IN table_partition text, IN partitioned_table text, IN date_from date, IN date_to date, IN _default boolean DEFAULT false, IN _commit boolean DEFAULT false)
    LANGUAGE plpgsql
    AS $$
DECLARE
    action_stmt text = '';
    end_date date = null::date;
BEGIN
    IF ( schema IS NULL OR schema = '' )
    THEN
        RAISE null_value_not_allowed
              USING MESSAGE = 'schema parameter cannot be null or empty string',
                    HINT = 'Use a valid schema name.';
    END IF;
    IF ( table_partition IS NULL OR table_partition = '' )
    THEN
        RAISE null_value_not_allowed
              USING MESSAGE = 'table_partition parameter cannot be null or empty string',
                    HINT = 'This must be a unique table name within the specified schema.';
    END IF;
    IF ( partitioned_table IS NULL OR partitioned_table = '' )
    THEN
        RAISE null_value_not_allowed
              USING MESSAGE = 'partitioned_table parameter cannot be null or empty string',
                    HINT = 'This must be the name of a table within the specified schema that is a partitioned table.';
    END IF;
    if ( NOT _default )
    THEN
        IF ( date_from IS NULL )
        THEN
            RAISE null_value_not_allowed
                USING MESSAGE = 'date_from parameter cannot be null',
                        HINT = 'This should be a valid date.';
        END IF;
        IF ( date_to IS NULL )
        THEN
            end_date = (date_from::date + '1 month'::interval)::date;
        ELSE
            end_date = date_to;
        END IF;
    END IF;

    action_stmt = 'CREATE TABLE IF NOT EXISTS ' ||
                    quote_ident(schema) || '.' || quote_ident(table_partition) ||
                    ' PARTITION OF ' ||
                    quote_ident(schema) || '.' || quote_ident(partitioned_table);
    IF ( _default )
    THEN
        action_stmt = action_stmt ||
                    ' DEFAULT ; ';
    ELSE
        action_stmt = action_stmt ||
                    ' FOR VALUES FROM (' ||
                    quote_literal(date_from) ||
                    '::date) TO (' ||
                    quote_literal(end_date) ||
                    '::date); ';
    END IF;
    EXECUTE action_stmt;

    -- log the new partition
    action_stmt = 'INSERT INTO ' || quote_ident(schema) || '."partitioned_tables" ( ' ||
                          '"schema_name", "table_name", "partition_of_table_name", ' ||
                          '"partition_type", "partition_col", "partition_parameters" ' ||
                  ') ' ||
                  'SELECT ' || quote_literal(schema) || ', ' ||
                               quote_literal(table_partition) || ', ' ||
                               quote_literal(partitioned_table) || ', ' ||
                               '''range'', ' ||
                               'string_agg(a.attname, '',''), ' ||
                               'jsonb_build_object( ' ||
                                   '''default'', false, ' ||
                                   '''from'', ' || quote_literal(date_from::text) || ', ' ||
                                   '''to'', ' || quote_literal(end_date::text) ||
                               ') ' ||
                    'FROM pg_partitioned_table p ' ||
                    'JOIN pg_attribute a ' ||
                      'ON a.attrelid = p.partrelid ' ||
                     'AND a.attnum = any(string_to_array(p.partattrs::text, '' '')::smallint[]) ' ||
                   'WHERE p.partrelid = ' ||
                          quote_literal(schema || '.'::text || partitioned_table) || '::regclass ' ||
                      'ON CONFLICT (schema_name, table_name) DO NOTHING ;';
    EXECUTE action_stmt;

    IF ( _commit = true )
    THEN
        COMMIT;
    END IF;
END;
$$;


ALTER PROCEDURE public.create_table_date_range_partition(IN schema text, IN table_partition text, IN partitioned_table text, IN date_from date, IN date_to date, IN _default boolean, IN _commit boolean) OWNER TO postgres;

--
-- Name: jsonb_sha256_text(jsonb); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.jsonb_sha256_text(j_param jsonb, OUT hash_val text) RETURNS text
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $$
begin
    select encode(sha256(decode(string_agg(key || ':' || value, '|'), 'escape')), 'hex')
      from (
             select *
               from jsonb_each_text(j_param)
              order by key, value
           ) as ordered_jsonb
      into hash_val;
end;
$$;


ALTER FUNCTION public.jsonb_sha256_text(j_param jsonb, OUT hash_val text) OWNER TO postgres;

--
-- Name: migrations_complete(jsonb, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.migrations_complete(leaf_migrations jsonb, _verbose boolean DEFAULT false) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
    schema_rec record;
    leaf_app_key text;
    leaf_app_keys text[];
    latest_migrations jsonb;
    required_tables int := 0;
    completed_migrations boolean := true;
    exists_rec record;
    chk_res boolean;
BEGIN
    /*
     * Verify that the necessary tables are present
     */
    SELECT count(*)
      INTO required_tables
      FROM pg_class c
      JOIN pg_namespace n
        ON n.oid = c.relnamespace
     WHERE n.nspname = 'public'
       AND c.relname in ('api_tenant', 'django_migrations');

    /*
     * If migrations have been run against public, then we expect to find both tables
     */
    IF required_tables != 2
    THEN
        IF _verbose
        THEN
            RAISE WARNING 'Schema "public" not initialized';
        END IF;
        RETURN false;
    END IF;

    /*
     * setup app keys for processing
     */
    SELECT array_agg(k)
      INTO leaf_app_keys
      FROM jsonb_object_keys(leaf_migrations) k;

    FOR schema_rec IN
        SELECT nspname::text as schema_name
        FROM pg_catalog.pg_namespace
        WHERE nspname = 'public'
            OR nspname = 'template0'
            OR nspname LIKE 'acct%'
         ORDER
            BY case when nspname::text = 'public'
                         then '0public'
                    else nspname::text
               END::text
    LOOP
        /* Get the latest recorded migrations by app for this tenant schema */
        IF _verbose
        THEN
            RAISE INFO 'Checking migration state in schema %', schema_rec.schema_name;
        END IF;

        /* Check for race condition if someone deletes a source, etc during processing */
        EXECUTE 'SELECT EXISTS ( ' ||
                            'SELECT c.oid ' ||
                              'FROM pg_class c ' ||
                              'JOIN pg_namespace n ' ||
                                'ON n.oid = c.relnamespace ' ||
                             'WHERE c.relname = ''django_migrations'' ' ||
                               'AND n.nspname = ' || quote_literal(schema_rec.schema_name) || ' ' ||
                        ')::boolean as "objects_exist", ' ||
                        'EXISTS ( ' ||
                            'SELECT n.oid ' ||
                              'FROM pg_namespace n ' ||
                             'WHERE nspname = ' || quote_literal(schema_rec.schema_name) || ' ' ||
                        ')::boolean as "schema_exists" '
          INTO exists_rec;

        CONTINUE WHEN (NOT exists_rec.schema_exists);

        IF NOT exists_rec.objects_exist
        THEN
            RAISE WARNING '    %.django_migrations does not exist', schema_rec.schema_name;
            completed_migrations = false;
        ELSE
            EXECUTE 'SELECT jsonb_object_agg(app, migration) ' ||
                    'FROM ( ' ||
                            'SELECT app, ' ||
                                    'max(name) as "migration" ' ||
                            'FROM ' || quote_ident(schema_rec.schema_name) || '.django_migrations ' ||
                            'GROUP BY app ' ||
                        ') AS x ;'
            INTO latest_migrations;

            /* Loop through leaf apps */
            FOREACH leaf_app_key IN ARRAY leaf_app_keys
            LOOP
                /* test that app exists or not */
                IF latest_migrations ? leaf_app_key
                THEN
                    /* App exists! Test if the leaf migration name is greater than the last recorded migration for the app */
                    chk_res = (leaf_migrations->>leaf_app_key > latest_migrations->>leaf_app_key)::boolean;
                    IF _verbose
                    THEN
                        RAISE INFO '    checking %.% > %.% (%)',
                                    leaf_app_key,
                                    leaf_migrations->>leaf_app_key,
                                    leaf_app_key,
                                    latest_migrations->>leaf_app_key,
                                    chk_res::text;
                    END IF;
                    IF chk_res
                    THEN
                        /* leaf ahead of last recorded. run migrations! */
                        completed_migrations = false;
                        IF _verbose
                        THEN
                            RAISE INFO '        Will run migrations.';
                        END IF;
                    END IF;
                ELSE
                    /* App does not exist, run migrations! */
                    IF _verbose
                    THEN
                        RAISE INFO '    app % missing from schema', leaf_app_key;
                    END IF;
                    completed_migrations = false;
                END IF;

                EXIT WHEN not completed_migrations;
            END LOOP;
        END IF;

        /* Stop processing if we are going to run migrations */
        EXIT WHEN not completed_migrations;
    END LOOP;

    IF _verbose
    THEN
        RAISE INFO 'Migration Check: App should%execute migrations.', case when completed_migrations then ' not ' else ' ' end::text;
    END IF;

    RETURN completed_migrations;
END;
$$;


ALTER FUNCTION public.migrations_complete(leaf_migrations jsonb, _verbose boolean) OWNER TO postgres;

--
-- Name: scan_for_date_partitions(text, text, text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.scan_for_date_partitions(check_table text, check_col text, schema text, partitioned_table text) RETURNS TABLE(partition_start date)
    LANGUAGE plpgsql
    AS $$
DECLARE
    rec record;
    table_parts text[];
    check_table_name text;
    partition_name text  = '';
    check_stmt text = '';
BEGIN
    table_parts = string_to_array(check_table, '.');
    IF ( cardinality(table_parts) > 1 )
    THEN
        check_table_name = quote_ident(table_parts[1]) || '.'::text || quote_ident(table_parts[2]);
    ELSE
        check_table_name = quote_ident(table_parts[1]);
    END IF;

    check_stmt = 'WITH distinct_date_key as (' ||
                    'SELECT DISTINCT ' ||
                    '       date_trunc(''month'', ' || quote_ident(check_col) || ')::date as date_val,' ||
                    '       to_char(' || quote_ident(check_col) || ', ''YYYY-MM-01'')::text as date_key' ||
                    '  FROM ' || check_table_name || ' ' ||
                    ') ' ||
                    'SELECT ddk.date_val as partition_start ' ||
                    '  FROM distinct_date_key as ddk ' ||
                    ' WHERE NOT EXISTS (SELECT 1 ' ||
                    '                     FROM ' ||
                                            quote_ident(schema) || '."partitioned_tables" ' ||
                    '                    WHERE schema_name = ' || quote_literal(schema) ||
                    '                      AND partition_of_table_name = ' || quote_literal(partitioned_table) ||
                    '                      AND partition_type = ''range'' ' ||
                    '                      AND ddk.date_key = (partition_parameters->>''from'') ) ;';
    RETURN QUERY EXECUTE check_stmt;
END;
$$;


ALTER FUNCTION public.scan_for_date_partitions(check_table text, check_col text, schema text, partitioned_table text) OWNER TO postgres;

--
-- Name: tr_presto_delete_wrapper_log_action(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.tr_presto_delete_wrapper_log_action() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
    if NEW.result_rows is null
    then
        execute 'delete from ' || quote_ident(TG_TABLE_SCHEMA) || '.' || quote_ident(NEW.table_name) || ' ' ||
                NEW.where_clause;
        get diagnostics NEW.result_rows = row_count;
    end if;

    return NEW;
end;
$$;


ALTER FUNCTION public.tr_presto_delete_wrapper_log_action() OWNER TO postgres;

--
-- Name: trfn_attach_date_range_partition(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.trfn_attach_date_range_partition() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    alter_stmt text = '';
    msg text = '';
BEGIN
    IF NEW.active != OLD.active
    THEN
        IF NEW.active = false
        THEN
            alter_stmt = 'ALTER TABLE ' ||
                        quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.partition_of_table_name) ||
                        ' DETACH PARTITION ' ||
                        quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name)
                        || ' ;';
            msg = 'DETACH PARTITION ' ||
                quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name);
        ELSE
            alter_stmt = 'ALTER TABLE ' ||
                        quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.partition_of_table_name) ||
                        ' ATTACH PARTITION ' ||
                        quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name) || ' ';
            IF ( (NEW.partition_parameters->>'default')::boolean )
            THEN
                alter_stmt = alter_stmt || 'DEFAULT ;';
                msg = 'DEFAULT';
            ELSE
                alter_stmt = alter_stmt || 'FOR VALUES FROM ( ' ||
                                quote_literal(NEW.partition_parameters->>'from') || '::date ) TO (' ||
                                quote_literal(NEW.partition_parameters->>'to') || '::date ) ;';
                msg = 'FOR VALUES FROM ( ' ||
                    quote_literal(NEW.partition_parameters->>'from') || '::date ) TO (' ||
                    quote_literal(NEW.partition_parameters->>'to') || '::date )';
            END IF;
            msg = 'ATTACH PARTITION ' ||
                quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name) ||
                ' ' || msg;
        END IF;

        RAISE NOTICE 'ALTER TABLE %.% : %',
                     quote_ident(NEW.schema_name), quote_ident(NEW.partition_of_table_name), msg;
        EXECUTE alter_stmt;
    END IF;

    RETURN NULL;
END;
$$;


ALTER FUNCTION public.trfn_attach_date_range_partition() OWNER TO postgres;

--
-- Name: trfn_manage_date_range_partition(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.trfn_manage_date_range_partition() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    alter_stmt text = '';
    action_stmt text = '';
    alter_msg text = '';
    action_msg text = '';
    table_name text = '';
BEGIN
    IF ( TG_OP = 'DELETE' )
    THEN
        IF ( OLD.active )
        THEN
            alter_stmt = 'ALTER TABLE ' ||
                        quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.partition_of_table_name) ||
                        ' DETACH PARTITION ' ||
                        quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name) ||
                        ' ;';
        END IF;
        action_stmt = 'DROP TABLE IF EXISTS ' || quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name) || ' ;';
        table_name = quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.partition_of_table_name);
        alter_msg = 'DROP PARTITION ' || quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name);
    ELSIF ( TG_OP = 'UPDATE' )
    THEN
        /* If the partition was active, then detach it */
        if ( OLD.active )
        THEN
            alter_stmt = 'ALTER TABLE ' ||
                        quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.partition_of_table_name) ||
                        ' DETACH PARTITION ' ||
                        quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name)
                        || ' ;';
        END IF;

        /* If we are going to active or are still active, then attach the partition */
        if ( NEW.active )
        THEN
            action_stmt = 'ALTER TABLE ' ||
                            quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.partition_of_table_name) ||
                            ' ATTACH PARTITION ' ||
                            quote_ident(OLD.schema_name) || '.' || quote_ident(OLD.table_name) || ' ';
            IF ( (NEW.partition_parameters->>'default') = 'true' )
            THEN
                action_stmt = action_stmt || 'DEFAULT ;';
                action_msg = 'DEFAULT';
            ELSE
                action_stmt = action_stmt || 'FOR VALUES FROM ( ' ||
                            quote_literal(NEW.partition_parameters->>'from') || '::date ) TO (' ||
                            quote_literal(NEW.partition_parameters->>'to') || '::date ) ;';
                action_msg = 'FOR VALUES FROM ( ' ||
                            quote_literal(NEW.partition_parameters->>'from') || '::date ) TO (' ||
                            quote_literal(NEW.partition_parameters->>'to') || '::date )';
            END IF;
        END IF;

        table_name = quote_ident(NEW.schema_name) || '.' || quote_ident(NEW.partition_of_table_name);
        action_msg = 'ALTER PARTITION ' || quote_ident(NEW.schema_name) || '.' || quote_ident(NEW.table_name) ||
                     ' ' || action_msg;
    ELSIF ( TG_OP = 'INSERT' )
    THEN
        action_stmt = 'CREATE TABLE IF NOT EXISTS ' ||
                      quote_ident(NEW.schema_name) || '.' || quote_ident(NEW.table_name) || ' ' ||
                      'PARTITION OF ' ||
                      quote_ident(NEW.schema_name) || '.' || quote_ident(NEW.partition_of_table_name) || ' ';
        IF ( (NEW.partition_parameters->>'default')::boolean )
        THEN
            action_stmt = action_stmt || 'DEFAULT ;';
            action_msg = 'DEFAULT';
        ELSE
            action_stmt = action_stmt || 'FOR VALUES FROM ( ' ||
                          quote_literal(NEW.partition_parameters->>'from') || '::date ) TO (' ||
                          quote_literal(NEW.partition_parameters->>'to') || '::date ) ;';
            action_msg = 'FOR VALUES FROM ( ' ||
                         quote_literal(NEW.partition_parameters->>'from') || '::date ) TO (' ||
                         quote_literal(NEW.partition_parameters->>'to') || '::date )';
        END IF;
        action_msg = 'CREATE PARTITION ' || quote_ident(NEW.schema_name) || '.' || quote_ident(NEW.table_name) ||
                     ' ' || action_msg;
        table_name = quote_ident(NEW.schema_name) || '.' || quote_ident(NEW.partition_of_table_name);
    ELSE
        RAISE EXCEPTION 'Unhandled trigger operation %', TG_OP;
    END IF;

    IF ( alter_stmt != '' )
    THEN
        IF ( alter_msg != '' )
        THEN
            RAISE NOTICE 'ALTER TABLE % : %', table_name, alter_msg;
        END IF;

        EXECUTE alter_stmt;
    END IF;

    IF ( action_stmt != '' )
    THEN
        IF ( action_msg != '' )
        THEN
            RAISE NOTICE 'ALTER TABLE % : %', table_name, action_msg;
        END IF;

        EXECUTE action_stmt;
    END IF;

    RETURN NULL;
END;
$$;


ALTER FUNCTION public.trfn_manage_date_range_partition() OWNER TO postgres;

--
-- Name: trfn_partition_manager(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.trfn_partition_manager() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    item text = '';
    n_partition_type text = null;
    action_stmt text = '';
    action_stmt_2 text = '';
    action_items text[] = '{}'::text[];
    action_ix integer = 1;
    total_actions integer = 0;
    action_stmts text[] = '{}'::text[];
    messages text[] = '{}'::text[];
    message_text text = '';
    col_type_name text = null;
    partition_attached boolean = null;
BEGIN
    /* Force any pending constraint work during the current transaction to fire immediately */
    SET CONSTRAINTS ALL IMMEDIATE;

    IF ( TG_OP = 'DELETE' )
    THEN
        EXECUTE format(
'
select c.oid
  from pg_class c
  join pg_namespace n
    on n.oid = c.relnamespace
 where n.nspname = %L
   and c.relname = %L ;
',
                    OLD.schema_name,
                    OLD.table_name
                )
           INTO item;
        IF item IS NOT NULL
        THEN
            IF ( OLD.active )
            THEN
                action_stmts = array_append(
                    action_stmts,
                    format(
                        'ALTER TABLE %I.%I DETACH PARTITION %I.%I ;',
                        OLD.schema_name,
                        OLD.partition_of_table_name,
                        OLD.schema_name,
                        OLD.table_name
                    )
                );
                messages = array_append(
                    messages,
                    format(
                        'DETACH PARTITION %I.%I FROM %I.%I',
                        OLD.schema_name,
                        OLD.table_name,
                        OLD.schema_name,
                        OLD.partition_of_table_name
                    )
                );
            END IF;

            action_stmts = array_append(
                action_stmts,
                format(
                    'TRUNCATE TABLE %I.%I ;', OLD.schema_name, OLD.table_name
                )
            );
            messages = array_append(messages, format('TRUNCATE TABLE %I.%I', OLD.schema_name, OLD.table_name));

            action_stmts = array_append(
                action_stmts,
                format(
                    'DROP TABLE IF EXISTS %I.%I ;', OLD.schema_name, OLD.table_name
                )
            );
            messages = array_append(messages, format('DROP TABLE IF EXISTS %I.%I', OLD.schema_name, OLD.table_name));
        ELSE
            RAISE NOTICE 'Table %.% does not exist. No partition actions taken', OLD.schema_name, OLD.table_name;
        END IF;
    ELSIF ( TG_OP = 'UPDATE' )
    THEN
        EXECUTE
         format(
'
SELECT relispartition
  FROM pg_class
 WHERE oid = ''%I.%I''::regclass ;
',
            OLD.schema_name,
            OLD.table_name
         )
           INTO partition_attached;

        IF OLD.partition_of_table_name != NEW.partition_of_table_name
        THEN
            action_stmts = array_append(
                action_stmts,
                format(
                    'ALTER TABLE IF EXISTS %I.%I RENAME TO %I ;',
                    OLD.schema_name,
                    OLD.partition_of_table_name,
                    NEW.partition_of_table_name
                )
            );
            messages = array_append(
                messages,
                format(
                    'RENAME TABLE %I.%I to %I',
                    OLD.schema_name,
                    OLD.partition_of_table_name,
                    NEW.partition_of_table_name
                )
            );
        END IF;

        IF OLD.table_name != NEW.table_name
        THEN
            action_stmts = array_append(
                action_stmts,
                format(
                    'ALTER TABLE IF EXISTS %I.%I RENAME TO %I ;',
                    OLD.schema_name,
                    OLD.table_name,
                    NEW.table_name
                )
            );
            messages = array_append(
                messages,
                format(
                    'RENAME TABLE %I.%I to %I',
                    OLD.schema_name,
                    OLD.table_name,
                    NEW.table_name
                )
            );
        END IF;

        IF ((OLD.active AND NOT NEW.active) OR
            (OLD.partition_parameters != NEW.partition_parameters)) AND
           partition_attached IS DISTINCT FROM false
        THEN
            partition_attached = false;

            action_stmts = array_append(
                action_stmts,
                format(
                    'ALTER TABLE IF EXISTS %I.%I DETACH PARTITION %I.%I ;',
                    OLD.schema_name,
                    NEW.partition_of_table_name,
                    OLD.schema_name,
                    NEW.table_name
                )
            );
            messages = array_append(
                messages,
                format('DETACH PARTITION %I.%I FROM %I.%I',
                    OLD.schema_name,
                    NEW.table_name,
                    OLD.schema_name,
                    NEW.partition_of_table_name
                )
            );
        END IF;

        IF ((NEW.active AND NOT OLD.active) OR
            (OLD.partition_parameters != NEW.partition_parameters)) AND
           partition_attached IS DISTINCT FROM true
        THEN
            partition_attached = true;

            action_stmt = format(
                'ALTER TABLE IF EXISTS %I.%I ATTACH PARTITION %I.%I ',
                OLD.schema_name,
                NEW.partition_of_table_name,
                OLD.schema_name,
                NEW.table_name
            );
            message_text = format(
                'ATTACH PARITITION %I.%I TO %I.%I ',
                OLD.schema_name,
                NEW.table_name,
                OLD.schema_name,
                NEW.partition_of_table_name
            );

            IF ( (NEW.partition_parameters->>'default') = 'true' )
            THEN
                action_stmts = array_append(
                    action_stmts,
                    action_stmt || 'DEFAULT ;'
                );
                messages = array_append(
                    messages,
                    message_text || 'AS DEFAULT PARITION'
                );
            ELSE
                EXECUTE format(
                    '
select format_type(
    (
        select atttypid
          from pg_attribute
         where attrelid = %L::regclass
           and attname = %L
    ),
    null
);
',
                    quote_ident(NEW.schema_name) || '.' || quote_ident(NEW.partition_of_table_name),
                    NEW.partition_col
                )
                   INTO col_type_name;
                IF col_type_name != 'char' AND col_type_name ~ '^char'
                THEN
                    col_type_name = 'text';
                END IF;

                n_partition_type = lower(NEW.partition_type);
                IF n_partition_type = 'range'
                THEN
                    action_stmt_2 = format(
                        'FOR VALUES FROM ( %L::%I ) TO ( %L::%I ) ',
                        NEW.partition_parameters->>'from',
                        col_type_name,
                        NEW.partition_parameters->>'to',
                        col_type_name
                    );
                ELSIF n_partition_type = 'list'
                THEN
                    FOREACH item IN ARRAY (string_to_array(NEW.partition_parameters->>'in', ',')::text[])
                    LOOP
                        action_items = array_append(
                            action_items,
                            format('%L::%I', item, col_type_name)
                        );
                    END LOOP;
                    action_stmt_2 = format(
                        'FOR VALUES IN ( %s ) ',
                        array_to_string(action_items, ', ')
                    );
                ELSE
                    RAISE EXCEPTION 'Only ''range'' and ''list'' partition types are currently supported';
                END IF;
                action_stmts = array_append(action_stmts, action_stmt || action_stmt_2);
                messages = array_append(messages, message_text || action_stmt_2);
            END IF;
        END IF;
    ELSIF ( TG_OP = 'INSERT' )
    THEN
        action_stmt = format(
            'CREATE TABLE IF NOT EXISTS %I.%I PARTITION OF %I.%I ',
            NEW.schema_name,
            NEW.table_name,
            NEW.schema_name,
            NEW.partition_of_table_name
        );
        message_text = format(
            'CREATING NEW PARTITION %I.%I FOR %I.%I ',
            NEW.schema_name,
            NEW.table_name,
            NEW.schema_name,
            NEW.partition_of_table_name
        );
        IF ( (NEW.partition_parameters->>'default')::boolean )
        THEN
            action_stmts = array_append(
                action_stmts,
                action_stmt || 'DEFAULT '
            );
            messages = array_append(
                messages,
                message_text || 'AS DEFAULT PARITION'
            );
        ELSE
            EXECUTE format(
                        '
select format_type(atttypid, null)
  from pg_attribute
 where attrelid = %L::regclass
   and attname = %L ;
',
                quote_ident(NEW.schema_name) || '.' || quote_ident(NEW.partition_of_table_name),
                NEW.partition_col
            )
               INTO col_type_name;
            IF col_type_name != 'char' AND col_type_name ~ '^char'
            THEN
                col_type_name = 'text';
            END IF;

            n_partition_type = lower(NEW.partition_type);
            IF n_partition_type = 'range'
            THEN
                action_stmt_2 = format(
                    'FOR VALUES FROM ( %L::%I ) TO ( %L::%I ) ',
                    NEW.partition_parameters->>'from',
                    col_type_name,
                    NEW.partition_parameters->>'to',
                    col_type_name
                );
            ELSIF n_partition_type = 'list'
            THEN
                FOREACH item IN ARRAY (string_to_array(NEW.partition_parameters->>'in', ',')::text[])
                LOOP
                    action_items = array_append(
                        action_items,
                        format('%L::%I', item, col_type_name)
                    );
                END LOOP;
                action_stmt_2 = format(
                    'FOR VALUES IN ( %s ) ',
                    array_to_string(action_items, ', ')
                );
            ELSE
                RAISE EXCEPTION 'Only ''range'' and ''list'' partition types are currently supported';
            END IF;

            IF nullif(NEW.subpartition_col, '') IS NOT NULL AND
               nullif(NEW.subpartition_type, '') IS NOT NULL
            THEN
                action_stmt_2 = action_stmt_2 ||
                                format(
                                    'PARTITION BY %s ( %I ) ',
                                    NEW.subpartition_type,
                                    NEW.subpartition_col
                                );
            END IF;

            action_stmts = array_append(action_stmts, action_stmt || action_stmt_2 || ' ;');
            messages = array_append(messages, message_text || action_stmt_2);
        END IF;
    ELSE
        RAISE EXCEPTION 'Unhandled trigger operation %', TG_OP;
    END IF;

    /* Execute the action statements we've queued */
    total_actions = cardinality(action_stmts);
    LOOP
        EXIT WHEN action_ix > total_actions;

        RAISE INFO '%', messages[action_ix];
        EXECUTE action_stmts[action_ix];

        action_ix = action_ix + 1;
    END LOOP;

    RETURN NULL;
END;
$$;


ALTER FUNCTION public.trfn_partition_manager() OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: BUCKETING_COLS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."BUCKETING_COLS" (
    "SD_ID" bigint NOT NULL,
    "BUCKET_COL_NAME" character varying(256) DEFAULT NULL::character varying,
    "INTEGER_IDX" bigint NOT NULL
);


ALTER TABLE public."BUCKETING_COLS" OWNER TO postgres;

--
-- Name: CDS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."CDS" (
    "CD_ID" bigint NOT NULL
);


ALTER TABLE public."CDS" OWNER TO postgres;

--
-- Name: COLUMNS_V2; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."COLUMNS_V2" (
    "CD_ID" bigint NOT NULL,
    "COMMENT" character varying(4000),
    "COLUMN_NAME" character varying(767) NOT NULL,
    "TYPE_NAME" text,
    "INTEGER_IDX" integer NOT NULL
);


ALTER TABLE public."COLUMNS_V2" OWNER TO postgres;

--
-- Name: CTLGS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."CTLGS" (
    "CTLG_ID" bigint NOT NULL,
    "NAME" character varying(256),
    "DESC" character varying(4000),
    "LOCATION_URI" character varying(4000) NOT NULL
);


ALTER TABLE public."CTLGS" OWNER TO postgres;

--
-- Name: DATABASE_PARAMS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."DATABASE_PARAMS" (
    "DB_ID" bigint NOT NULL,
    "PARAM_KEY" character varying(180) NOT NULL,
    "PARAM_VALUE" character varying(4000) DEFAULT NULL::character varying
);


ALTER TABLE public."DATABASE_PARAMS" OWNER TO postgres;

--
-- Name: DBS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."DBS" (
    "DB_ID" bigint NOT NULL,
    "DESC" character varying(4000) DEFAULT NULL::character varying,
    "DB_LOCATION_URI" character varying(4000) NOT NULL,
    "NAME" character varying(128) DEFAULT NULL::character varying,
    "OWNER_NAME" character varying(128) DEFAULT NULL::character varying,
    "OWNER_TYPE" character varying(10) DEFAULT NULL::character varying,
    "CTLG_NAME" character varying(256) DEFAULT 'hive'::character varying
);


ALTER TABLE public."DBS" OWNER TO postgres;

--
-- Name: DB_PRIVS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."DB_PRIVS" (
    "DB_GRANT_ID" bigint NOT NULL,
    "CREATE_TIME" bigint NOT NULL,
    "DB_ID" bigint,
    "GRANT_OPTION" smallint NOT NULL,
    "GRANTOR" character varying(128) DEFAULT NULL::character varying,
    "GRANTOR_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_NAME" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_TYPE" character varying(128) DEFAULT NULL::character varying,
    "DB_PRIV" character varying(128) DEFAULT NULL::character varying,
    "AUTHORIZER" character varying(128) DEFAULT NULL::character varying
);


ALTER TABLE public."DB_PRIVS" OWNER TO postgres;

--
-- Name: DELEGATION_TOKENS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."DELEGATION_TOKENS" (
    "TOKEN_IDENT" character varying(767) NOT NULL,
    "TOKEN" character varying(767)
);


ALTER TABLE public."DELEGATION_TOKENS" OWNER TO postgres;

--
-- Name: FUNCS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."FUNCS" (
    "FUNC_ID" bigint NOT NULL,
    "CLASS_NAME" character varying(4000),
    "CREATE_TIME" integer NOT NULL,
    "DB_ID" bigint,
    "FUNC_NAME" character varying(128),
    "FUNC_TYPE" integer NOT NULL,
    "OWNER_NAME" character varying(128),
    "OWNER_TYPE" character varying(10)
);


ALTER TABLE public."FUNCS" OWNER TO postgres;

--
-- Name: FUNC_RU; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."FUNC_RU" (
    "FUNC_ID" bigint NOT NULL,
    "RESOURCE_TYPE" integer NOT NULL,
    "RESOURCE_URI" character varying(4000),
    "INTEGER_IDX" integer NOT NULL
);


ALTER TABLE public."FUNC_RU" OWNER TO postgres;

--
-- Name: GLOBAL_PRIVS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."GLOBAL_PRIVS" (
    "USER_GRANT_ID" bigint NOT NULL,
    "CREATE_TIME" bigint NOT NULL,
    "GRANT_OPTION" smallint NOT NULL,
    "GRANTOR" character varying(128) DEFAULT NULL::character varying,
    "GRANTOR_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_NAME" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_TYPE" character varying(128) DEFAULT NULL::character varying,
    "USER_PRIV" character varying(128) DEFAULT NULL::character varying,
    "AUTHORIZER" character varying(128) DEFAULT NULL::character varying
);


ALTER TABLE public."GLOBAL_PRIVS" OWNER TO postgres;

--
-- Name: IDXS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."IDXS" (
    "INDEX_ID" bigint NOT NULL,
    "CREATE_TIME" bigint NOT NULL,
    "DEFERRED_REBUILD" boolean NOT NULL,
    "INDEX_HANDLER_CLASS" character varying(4000) DEFAULT NULL::character varying,
    "INDEX_NAME" character varying(128) DEFAULT NULL::character varying,
    "INDEX_TBL_ID" bigint,
    "LAST_ACCESS_TIME" bigint NOT NULL,
    "ORIG_TBL_ID" bigint,
    "SD_ID" bigint
);


ALTER TABLE public."IDXS" OWNER TO postgres;

--
-- Name: INDEX_PARAMS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."INDEX_PARAMS" (
    "INDEX_ID" bigint NOT NULL,
    "PARAM_KEY" character varying(256) NOT NULL,
    "PARAM_VALUE" character varying(4000) DEFAULT NULL::character varying
);


ALTER TABLE public."INDEX_PARAMS" OWNER TO postgres;

--
-- Name: I_SCHEMA; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."I_SCHEMA" (
    "SCHEMA_ID" bigint NOT NULL,
    "SCHEMA_TYPE" integer NOT NULL,
    "NAME" character varying(256),
    "DB_ID" bigint,
    "COMPATIBILITY" integer NOT NULL,
    "VALIDATION_LEVEL" integer NOT NULL,
    "CAN_EVOLVE" boolean NOT NULL,
    "SCHEMA_GROUP" character varying(256),
    "DESCRIPTION" character varying(4000)
);


ALTER TABLE public."I_SCHEMA" OWNER TO postgres;

--
-- Name: KEY_CONSTRAINTS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."KEY_CONSTRAINTS" (
    "CHILD_CD_ID" bigint,
    "CHILD_INTEGER_IDX" bigint,
    "CHILD_TBL_ID" bigint,
    "PARENT_CD_ID" bigint,
    "PARENT_INTEGER_IDX" bigint NOT NULL,
    "PARENT_TBL_ID" bigint NOT NULL,
    "POSITION" bigint NOT NULL,
    "CONSTRAINT_NAME" character varying(400) NOT NULL,
    "CONSTRAINT_TYPE" smallint NOT NULL,
    "UPDATE_RULE" smallint,
    "DELETE_RULE" smallint,
    "ENABLE_VALIDATE_RELY" smallint NOT NULL,
    "DEFAULT_VALUE" character varying(400)
);


ALTER TABLE public."KEY_CONSTRAINTS" OWNER TO postgres;

--
-- Name: MASTER_KEYS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."MASTER_KEYS" (
    "KEY_ID" integer NOT NULL,
    "MASTER_KEY" character varying(767)
);


ALTER TABLE public."MASTER_KEYS" OWNER TO postgres;

--
-- Name: MASTER_KEYS_KEY_ID_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public."MASTER_KEYS_KEY_ID_seq"
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public."MASTER_KEYS_KEY_ID_seq" OWNER TO postgres;

--
-- Name: MASTER_KEYS_KEY_ID_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public."MASTER_KEYS_KEY_ID_seq" OWNED BY public."MASTER_KEYS"."KEY_ID";


--
-- Name: METASTORE_DB_PROPERTIES; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."METASTORE_DB_PROPERTIES" (
    "PROPERTY_KEY" character varying(255) NOT NULL,
    "PROPERTY_VALUE" character varying(1000) NOT NULL,
    "DESCRIPTION" character varying(1000)
);


ALTER TABLE public."METASTORE_DB_PROPERTIES" OWNER TO postgres;

--
-- Name: MV_CREATION_METADATA; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."MV_CREATION_METADATA" (
    "MV_CREATION_METADATA_ID" bigint NOT NULL,
    "CAT_NAME" character varying(256) NOT NULL,
    "DB_NAME" character varying(128) NOT NULL,
    "TBL_NAME" character varying(256) NOT NULL,
    "TXN_LIST" text,
    "MATERIALIZATION_TIME" bigint NOT NULL
);


ALTER TABLE public."MV_CREATION_METADATA" OWNER TO postgres;

--
-- Name: MV_TABLES_USED; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."MV_TABLES_USED" (
    "MV_CREATION_METADATA_ID" bigint NOT NULL,
    "TBL_ID" bigint NOT NULL
);


ALTER TABLE public."MV_TABLES_USED" OWNER TO postgres;

--
-- Name: NOTIFICATION_LOG; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."NOTIFICATION_LOG" (
    "NL_ID" bigint NOT NULL,
    "EVENT_ID" bigint NOT NULL,
    "EVENT_TIME" integer NOT NULL,
    "EVENT_TYPE" character varying(32) NOT NULL,
    "CAT_NAME" character varying(256),
    "DB_NAME" character varying(128),
    "TBL_NAME" character varying(256),
    "MESSAGE" text,
    "MESSAGE_FORMAT" character varying(16)
);


ALTER TABLE public."NOTIFICATION_LOG" OWNER TO postgres;

--
-- Name: NOTIFICATION_SEQUENCE; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."NOTIFICATION_SEQUENCE" (
    "NNI_ID" bigint NOT NULL,
    "NEXT_EVENT_ID" bigint NOT NULL
);


ALTER TABLE public."NOTIFICATION_SEQUENCE" OWNER TO postgres;

--
-- Name: NUCLEUS_TABLES; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."NUCLEUS_TABLES" (
    "CLASS_NAME" character varying(128) NOT NULL,
    "TABLE_NAME" character varying(128) NOT NULL,
    "TYPE" character varying(4) NOT NULL,
    "OWNER" character varying(2) NOT NULL,
    "VERSION" character varying(20) NOT NULL,
    "INTERFACE_NAME" character varying(255) DEFAULT NULL::character varying
);


ALTER TABLE public."NUCLEUS_TABLES" OWNER TO postgres;

--
-- Name: PARTITIONS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."PARTITIONS" (
    "PART_ID" bigint NOT NULL,
    "CREATE_TIME" bigint NOT NULL,
    "LAST_ACCESS_TIME" bigint NOT NULL,
    "PART_NAME" character varying(767) DEFAULT NULL::character varying,
    "SD_ID" bigint,
    "TBL_ID" bigint
);


ALTER TABLE public."PARTITIONS" OWNER TO postgres;

--
-- Name: PARTITION_EVENTS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."PARTITION_EVENTS" (
    "PART_NAME_ID" bigint NOT NULL,
    "CAT_NAME" character varying(256),
    "DB_NAME" character varying(128),
    "EVENT_TIME" bigint NOT NULL,
    "EVENT_TYPE" integer NOT NULL,
    "PARTITION_NAME" character varying(767),
    "TBL_NAME" character varying(256)
);


ALTER TABLE public."PARTITION_EVENTS" OWNER TO postgres;

--
-- Name: PARTITION_KEYS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."PARTITION_KEYS" (
    "TBL_ID" bigint NOT NULL,
    "PKEY_COMMENT" character varying(4000) DEFAULT NULL::character varying,
    "PKEY_NAME" character varying(128) NOT NULL,
    "PKEY_TYPE" character varying(767) NOT NULL,
    "INTEGER_IDX" bigint NOT NULL
);


ALTER TABLE public."PARTITION_KEYS" OWNER TO postgres;

--
-- Name: PARTITION_KEY_VALS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."PARTITION_KEY_VALS" (
    "PART_ID" bigint NOT NULL,
    "PART_KEY_VAL" character varying(256) DEFAULT NULL::character varying,
    "INTEGER_IDX" bigint NOT NULL
);


ALTER TABLE public."PARTITION_KEY_VALS" OWNER TO postgres;

--
-- Name: PARTITION_PARAMS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."PARTITION_PARAMS" (
    "PART_ID" bigint NOT NULL,
    "PARAM_KEY" character varying(256) NOT NULL,
    "PARAM_VALUE" character varying(4000) DEFAULT NULL::character varying
);


ALTER TABLE public."PARTITION_PARAMS" OWNER TO postgres;

--
-- Name: PART_COL_PRIVS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."PART_COL_PRIVS" (
    "PART_COLUMN_GRANT_ID" bigint NOT NULL,
    "COLUMN_NAME" character varying(767) DEFAULT NULL::character varying,
    "CREATE_TIME" bigint NOT NULL,
    "GRANT_OPTION" smallint NOT NULL,
    "GRANTOR" character varying(128) DEFAULT NULL::character varying,
    "GRANTOR_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PART_ID" bigint,
    "PRINCIPAL_NAME" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PART_COL_PRIV" character varying(128) DEFAULT NULL::character varying,
    "AUTHORIZER" character varying(128) DEFAULT NULL::character varying
);


ALTER TABLE public."PART_COL_PRIVS" OWNER TO postgres;

--
-- Name: PART_COL_STATS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."PART_COL_STATS" (
    "CS_ID" bigint NOT NULL,
    "CAT_NAME" character varying(256) DEFAULT NULL::character varying,
    "DB_NAME" character varying(128) DEFAULT NULL::character varying,
    "TABLE_NAME" character varying(256) DEFAULT NULL::character varying,
    "PARTITION_NAME" character varying(767) DEFAULT NULL::character varying,
    "COLUMN_NAME" character varying(767) DEFAULT NULL::character varying,
    "COLUMN_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PART_ID" bigint NOT NULL,
    "LONG_LOW_VALUE" bigint,
    "LONG_HIGH_VALUE" bigint,
    "DOUBLE_LOW_VALUE" double precision,
    "DOUBLE_HIGH_VALUE" double precision,
    "BIG_DECIMAL_LOW_VALUE" character varying(4000) DEFAULT NULL::character varying,
    "BIG_DECIMAL_HIGH_VALUE" character varying(4000) DEFAULT NULL::character varying,
    "NUM_NULLS" bigint NOT NULL,
    "NUM_DISTINCTS" bigint,
    "BIT_VECTOR" bytea,
    "AVG_COL_LEN" double precision,
    "MAX_COL_LEN" bigint,
    "NUM_TRUES" bigint,
    "NUM_FALSES" bigint,
    "LAST_ANALYZED" bigint NOT NULL
);


ALTER TABLE public."PART_COL_STATS" OWNER TO postgres;

--
-- Name: PART_PRIVS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."PART_PRIVS" (
    "PART_GRANT_ID" bigint NOT NULL,
    "CREATE_TIME" bigint NOT NULL,
    "GRANT_OPTION" smallint NOT NULL,
    "GRANTOR" character varying(128) DEFAULT NULL::character varying,
    "GRANTOR_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PART_ID" bigint,
    "PRINCIPAL_NAME" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PART_PRIV" character varying(128) DEFAULT NULL::character varying,
    "AUTHORIZER" character varying(128) DEFAULT NULL::character varying
);


ALTER TABLE public."PART_PRIVS" OWNER TO postgres;

--
-- Name: ROLES; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."ROLES" (
    "ROLE_ID" bigint NOT NULL,
    "CREATE_TIME" bigint NOT NULL,
    "OWNER_NAME" character varying(128) DEFAULT NULL::character varying,
    "ROLE_NAME" character varying(128) DEFAULT NULL::character varying
);


ALTER TABLE public."ROLES" OWNER TO postgres;

--
-- Name: ROLE_MAP; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."ROLE_MAP" (
    "ROLE_GRANT_ID" bigint NOT NULL,
    "ADD_TIME" bigint NOT NULL,
    "GRANT_OPTION" smallint NOT NULL,
    "GRANTOR" character varying(128) DEFAULT NULL::character varying,
    "GRANTOR_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_NAME" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_TYPE" character varying(128) DEFAULT NULL::character varying,
    "ROLE_ID" bigint
);


ALTER TABLE public."ROLE_MAP" OWNER TO postgres;

--
-- Name: SCHEMA_VERSION; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SCHEMA_VERSION" (
    "SCHEMA_VERSION_ID" bigint NOT NULL,
    "SCHEMA_ID" bigint,
    "VERSION" integer NOT NULL,
    "CREATED_AT" bigint NOT NULL,
    "CD_ID" bigint,
    "STATE" integer NOT NULL,
    "DESCRIPTION" character varying(4000),
    "SCHEMA_TEXT" text,
    "FINGERPRINT" character varying(256),
    "SCHEMA_VERSION_NAME" character varying(256),
    "SERDE_ID" bigint
);


ALTER TABLE public."SCHEMA_VERSION" OWNER TO postgres;

--
-- Name: SDS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SDS" (
    "SD_ID" bigint NOT NULL,
    "INPUT_FORMAT" character varying(4000) DEFAULT NULL::character varying,
    "IS_COMPRESSED" boolean NOT NULL,
    "LOCATION" character varying(4000) DEFAULT NULL::character varying,
    "NUM_BUCKETS" bigint NOT NULL,
    "OUTPUT_FORMAT" character varying(4000) DEFAULT NULL::character varying,
    "SERDE_ID" bigint,
    "CD_ID" bigint,
    "IS_STOREDASSUBDIRECTORIES" boolean NOT NULL
);


ALTER TABLE public."SDS" OWNER TO postgres;

--
-- Name: SD_PARAMS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SD_PARAMS" (
    "SD_ID" bigint NOT NULL,
    "PARAM_KEY" character varying(256) NOT NULL,
    "PARAM_VALUE" text
);


ALTER TABLE public."SD_PARAMS" OWNER TO postgres;

--
-- Name: SEQUENCE_TABLE; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SEQUENCE_TABLE" (
    "SEQUENCE_NAME" character varying(255) NOT NULL,
    "NEXT_VAL" bigint NOT NULL
);


ALTER TABLE public."SEQUENCE_TABLE" OWNER TO postgres;

--
-- Name: SERDES; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SERDES" (
    "SERDE_ID" bigint NOT NULL,
    "NAME" character varying(128) DEFAULT NULL::character varying,
    "SLIB" character varying(4000) DEFAULT NULL::character varying,
    "DESCRIPTION" character varying(4000),
    "SERIALIZER_CLASS" character varying(4000),
    "DESERIALIZER_CLASS" character varying(4000),
    "SERDE_TYPE" integer
);


ALTER TABLE public."SERDES" OWNER TO postgres;

--
-- Name: SERDE_PARAMS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SERDE_PARAMS" (
    "SERDE_ID" bigint NOT NULL,
    "PARAM_KEY" character varying(256) NOT NULL,
    "PARAM_VALUE" text
);


ALTER TABLE public."SERDE_PARAMS" OWNER TO postgres;

--
-- Name: SKEWED_COL_NAMES; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SKEWED_COL_NAMES" (
    "SD_ID" bigint NOT NULL,
    "SKEWED_COL_NAME" character varying(256) DEFAULT NULL::character varying,
    "INTEGER_IDX" bigint NOT NULL
);


ALTER TABLE public."SKEWED_COL_NAMES" OWNER TO postgres;

--
-- Name: SKEWED_COL_VALUE_LOC_MAP; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SKEWED_COL_VALUE_LOC_MAP" (
    "SD_ID" bigint NOT NULL,
    "STRING_LIST_ID_KID" bigint NOT NULL,
    "LOCATION" character varying(4000) DEFAULT NULL::character varying
);


ALTER TABLE public."SKEWED_COL_VALUE_LOC_MAP" OWNER TO postgres;

--
-- Name: SKEWED_STRING_LIST; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SKEWED_STRING_LIST" (
    "STRING_LIST_ID" bigint NOT NULL
);


ALTER TABLE public."SKEWED_STRING_LIST" OWNER TO postgres;

--
-- Name: SKEWED_STRING_LIST_VALUES; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SKEWED_STRING_LIST_VALUES" (
    "STRING_LIST_ID" bigint NOT NULL,
    "STRING_LIST_VALUE" character varying(256) DEFAULT NULL::character varying,
    "INTEGER_IDX" bigint NOT NULL
);


ALTER TABLE public."SKEWED_STRING_LIST_VALUES" OWNER TO postgres;

--
-- Name: SKEWED_VALUES; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SKEWED_VALUES" (
    "SD_ID_OID" bigint NOT NULL,
    "STRING_LIST_ID_EID" bigint NOT NULL,
    "INTEGER_IDX" bigint NOT NULL
);


ALTER TABLE public."SKEWED_VALUES" OWNER TO postgres;

--
-- Name: SORT_COLS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."SORT_COLS" (
    "SD_ID" bigint NOT NULL,
    "COLUMN_NAME" character varying(767) DEFAULT NULL::character varying,
    "ORDER" bigint NOT NULL,
    "INTEGER_IDX" bigint NOT NULL
);


ALTER TABLE public."SORT_COLS" OWNER TO postgres;

--
-- Name: TABLE_PARAMS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."TABLE_PARAMS" (
    "TBL_ID" bigint NOT NULL,
    "PARAM_KEY" character varying(256) NOT NULL,
    "PARAM_VALUE" text
);


ALTER TABLE public."TABLE_PARAMS" OWNER TO postgres;

--
-- Name: TAB_COL_STATS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."TAB_COL_STATS" (
    "CS_ID" bigint NOT NULL,
    "CAT_NAME" character varying(256) DEFAULT NULL::character varying,
    "DB_NAME" character varying(128) DEFAULT NULL::character varying,
    "TABLE_NAME" character varying(256) DEFAULT NULL::character varying,
    "COLUMN_NAME" character varying(767) DEFAULT NULL::character varying,
    "COLUMN_TYPE" character varying(128) DEFAULT NULL::character varying,
    "TBL_ID" bigint NOT NULL,
    "LONG_LOW_VALUE" bigint,
    "LONG_HIGH_VALUE" bigint,
    "DOUBLE_LOW_VALUE" double precision,
    "DOUBLE_HIGH_VALUE" double precision,
    "BIG_DECIMAL_LOW_VALUE" character varying(4000) DEFAULT NULL::character varying,
    "BIG_DECIMAL_HIGH_VALUE" character varying(4000) DEFAULT NULL::character varying,
    "NUM_NULLS" bigint NOT NULL,
    "NUM_DISTINCTS" bigint,
    "BIT_VECTOR" bytea,
    "AVG_COL_LEN" double precision,
    "MAX_COL_LEN" bigint,
    "NUM_TRUES" bigint,
    "NUM_FALSES" bigint,
    "LAST_ANALYZED" bigint NOT NULL
);


ALTER TABLE public."TAB_COL_STATS" OWNER TO postgres;

--
-- Name: TBLS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."TBLS" (
    "TBL_ID" bigint NOT NULL,
    "CREATE_TIME" bigint NOT NULL,
    "DB_ID" bigint,
    "LAST_ACCESS_TIME" bigint NOT NULL,
    "OWNER" character varying(767) DEFAULT NULL::character varying,
    "OWNER_TYPE" character varying(10) DEFAULT NULL::character varying,
    "RETENTION" bigint NOT NULL,
    "SD_ID" bigint,
    "TBL_NAME" character varying(256) DEFAULT NULL::character varying,
    "TBL_TYPE" character varying(128) DEFAULT NULL::character varying,
    "VIEW_EXPANDED_TEXT" text,
    "VIEW_ORIGINAL_TEXT" text,
    "IS_REWRITE_ENABLED" boolean DEFAULT false NOT NULL
);


ALTER TABLE public."TBLS" OWNER TO postgres;

--
-- Name: TBL_COL_PRIVS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."TBL_COL_PRIVS" (
    "TBL_COLUMN_GRANT_ID" bigint NOT NULL,
    "COLUMN_NAME" character varying(767) DEFAULT NULL::character varying,
    "CREATE_TIME" bigint NOT NULL,
    "GRANT_OPTION" smallint NOT NULL,
    "GRANTOR" character varying(128) DEFAULT NULL::character varying,
    "GRANTOR_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_NAME" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_TYPE" character varying(128) DEFAULT NULL::character varying,
    "TBL_COL_PRIV" character varying(128) DEFAULT NULL::character varying,
    "TBL_ID" bigint,
    "AUTHORIZER" character varying(128) DEFAULT NULL::character varying
);


ALTER TABLE public."TBL_COL_PRIVS" OWNER TO postgres;

--
-- Name: TBL_PRIVS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."TBL_PRIVS" (
    "TBL_GRANT_ID" bigint NOT NULL,
    "CREATE_TIME" bigint NOT NULL,
    "GRANT_OPTION" smallint NOT NULL,
    "GRANTOR" character varying(128) DEFAULT NULL::character varying,
    "GRANTOR_TYPE" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_NAME" character varying(128) DEFAULT NULL::character varying,
    "PRINCIPAL_TYPE" character varying(128) DEFAULT NULL::character varying,
    "TBL_PRIV" character varying(128) DEFAULT NULL::character varying,
    "TBL_ID" bigint,
    "AUTHORIZER" character varying(128) DEFAULT NULL::character varying
);


ALTER TABLE public."TBL_PRIVS" OWNER TO postgres;

--
-- Name: TYPES; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."TYPES" (
    "TYPES_ID" bigint NOT NULL,
    "TYPE_NAME" character varying(128) DEFAULT NULL::character varying,
    "TYPE1" character varying(767) DEFAULT NULL::character varying,
    "TYPE2" character varying(767) DEFAULT NULL::character varying
);


ALTER TABLE public."TYPES" OWNER TO postgres;

--
-- Name: TYPE_FIELDS; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."TYPE_FIELDS" (
    "TYPE_NAME" bigint NOT NULL,
    "COMMENT" character varying(256) DEFAULT NULL::character varying,
    "FIELD_NAME" character varying(128) NOT NULL,
    "FIELD_TYPE" character varying(767) NOT NULL,
    "INTEGER_IDX" bigint NOT NULL
);


ALTER TABLE public."TYPE_FIELDS" OWNER TO postgres;

--
-- Name: VERSION; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."VERSION" (
    "VER_ID" bigint NOT NULL,
    "SCHEMA_VERSION" character varying(127) NOT NULL,
    "VERSION_COMMENT" character varying(255) NOT NULL
);


ALTER TABLE public."VERSION" OWNER TO postgres;

--
-- Name: WM_MAPPING; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."WM_MAPPING" (
    "MAPPING_ID" bigint NOT NULL,
    "RP_ID" bigint NOT NULL,
    "ENTITY_TYPE" character varying(128) NOT NULL,
    "ENTITY_NAME" character varying(128) NOT NULL,
    "POOL_ID" bigint,
    "ORDERING" integer
);


ALTER TABLE public."WM_MAPPING" OWNER TO postgres;

--
-- Name: WM_POOL; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."WM_POOL" (
    "POOL_ID" bigint NOT NULL,
    "RP_ID" bigint NOT NULL,
    "PATH" character varying(1024) NOT NULL,
    "ALLOC_FRACTION" double precision,
    "QUERY_PARALLELISM" integer,
    "SCHEDULING_POLICY" character varying(1024)
);


ALTER TABLE public."WM_POOL" OWNER TO postgres;

--
-- Name: WM_POOL_TO_TRIGGER; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."WM_POOL_TO_TRIGGER" (
    "POOL_ID" bigint NOT NULL,
    "TRIGGER_ID" bigint NOT NULL
);


ALTER TABLE public."WM_POOL_TO_TRIGGER" OWNER TO postgres;

--
-- Name: WM_RESOURCEPLAN; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."WM_RESOURCEPLAN" (
    "RP_ID" bigint NOT NULL,
    "NAME" character varying(128) NOT NULL,
    "QUERY_PARALLELISM" integer,
    "STATUS" character varying(20) NOT NULL,
    "DEFAULT_POOL_ID" bigint
);


ALTER TABLE public."WM_RESOURCEPLAN" OWNER TO postgres;

--
-- Name: WM_TRIGGER; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."WM_TRIGGER" (
    "TRIGGER_ID" bigint NOT NULL,
    "RP_ID" bigint NOT NULL,
    "NAME" character varying(128) NOT NULL,
    "TRIGGER_EXPRESSION" character varying(1024) DEFAULT NULL::character varying,
    "ACTION_EXPRESSION" character varying(1024) DEFAULT NULL::character varying,
    "IS_IN_UNMANAGED" smallint DEFAULT 0 NOT NULL
);


ALTER TABLE public."WM_TRIGGER" OWNER TO postgres;

--
-- Name: __customer_total_data; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.__customer_total_data (
    id bigint NOT NULL,
    date date NOT NULL,
    total_aws_unblended_cost numeric(33,2),
    total_aws_calculated_amortized_cost numeric(33,2),
    total_azure_pretax_cost numeric(33,2),
    total_gcp_unblended_cost numeric(33,2),
    total_gcp_total numeric(33,2),
    total_oci_total numeric(33,2),
    total_total_infrastructure_raw_cost numeric(33,2),
    total_total_cost_model_costs numeric(33,2),
    total_infra_total_cost_model numeric(33,2),
    total_infra_cost_model_cpu_cost numeric(33,2),
    total_infra_cost_model_memory_cost numeric(33,2),
    total_infra_cost_model_volume_cost numeric(33,2),
    total_sup_total_cost_model numeric(33,2),
    total_sup_cost_model_cpu_cost numeric(33,2),
    total_sup_cost_model_memory_cost numeric(33,2),
    total_sup_cost_model_volume_cost numeric(33,2),
    total_cluster_count integer,
    total_node_count integer,
    total_infra_node_count integer,
    total_control_plane_node_count integer,
    total_worker_node_count integer,
    total_infra_node_cpu_cores numeric(33,2),
    total_control_plane_node_cpu_cores numeric(33,2),
    total_worker_node_cpu_cores numeric(33,2),
    total_infra_node_mem_gb numeric(33,2),
    total_control_plane_node_mem_gb numeric(33,2),
    total_worker_node_mem_gb numeric(33,2),
    total_pvc_count integer,
    total_cluster_capacity_cores numeric(33,2),
    total_cluster_capacity_core_hours numeric(33,2),
    total_cluster_capacity_memory_gb numeric(33,2),
    total_cluster_capacity_memory_gb_hours numeric(33,2),
    total_volume_request_gb numeric(33,2),
    total_volume_request_gb_mo numeric(33,2),
    total_pvc_capacity_gb numeric(33,2),
    total_pvc_capacity_gb_mo numeric(33,2)
);


ALTER TABLE public.__customer_total_data OWNER TO postgres;

--
-- Name: __customer_total_data_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.__customer_total_data ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.__customer_total_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_customer; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_customer (
    id integer NOT NULL,
    date_created timestamp with time zone NOT NULL,
    date_updated timestamp with time zone NOT NULL,
    uuid uuid NOT NULL,
    account_id character varying(150),
    schema_name text NOT NULL,
    org_id character varying(36)
);


ALTER TABLE public.api_customer OWNER TO postgres;

--
-- Name: api_customer_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_customer ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_customer_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_domain; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_domain (
    id integer NOT NULL,
    domain character varying(253) NOT NULL,
    is_primary boolean NOT NULL,
    tenant_id integer NOT NULL
);


ALTER TABLE public.api_domain OWNER TO postgres;

--
-- Name: api_domain_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_domain ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_domain_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_exchangeratedictionary; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_exchangeratedictionary (
    id integer NOT NULL,
    currency_exchange_dictionary jsonb
);


ALTER TABLE public.api_exchangeratedictionary OWNER TO postgres;

--
-- Name: api_exchangeratedictionary_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_exchangeratedictionary ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_exchangeratedictionary_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_exchangerates; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_exchangerates (
    id integer NOT NULL,
    currency_type character varying(5) NOT NULL,
    exchange_rate double precision NOT NULL
);


ALTER TABLE public.api_exchangerates OWNER TO postgres;

--
-- Name: api_exchangerates_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_exchangerates ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_exchangerates_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_provider; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_provider (
    uuid uuid NOT NULL,
    name character varying(256) NOT NULL,
    type character varying(50) NOT NULL,
    setup_complete boolean NOT NULL,
    created_timestamp timestamp with time zone,
    data_updated_timestamp timestamp with time zone,
    active boolean NOT NULL,
    authentication_id integer,
    billing_source_id integer,
    created_by_id integer,
    customer_id integer,
    infrastructure_id integer,
    paused boolean NOT NULL,
    additional_context jsonb,
    polling_timestamp timestamp with time zone
);


ALTER TABLE public.api_provider OWNER TO postgres;

--
-- Name: api_providerauthentication; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_providerauthentication (
    id integer NOT NULL,
    uuid uuid NOT NULL,
    credentials jsonb NOT NULL
);


ALTER TABLE public.api_providerauthentication OWNER TO postgres;

--
-- Name: api_providerauthentication_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_providerauthentication ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_providerauthentication_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_providerbillingsource; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_providerbillingsource (
    id integer NOT NULL,
    uuid uuid NOT NULL,
    data_source jsonb NOT NULL
);


ALTER TABLE public.api_providerbillingsource OWNER TO postgres;

--
-- Name: api_providerbillingsource_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_providerbillingsource ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_providerbillingsource_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_providerinfrastructuremap; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_providerinfrastructuremap (
    id integer NOT NULL,
    infrastructure_type character varying(50) NOT NULL,
    infrastructure_provider_id uuid NOT NULL
);


ALTER TABLE public.api_providerinfrastructuremap OWNER TO postgres;

--
-- Name: api_providerinfrastructuremap_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_providerinfrastructuremap ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_providerinfrastructuremap_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_sources; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_sources (
    source_id integer NOT NULL,
    source_uuid uuid,
    name text,
    auth_header text,
    "offset" integer NOT NULL,
    account_id text,
    source_type text NOT NULL,
    authentication jsonb NOT NULL,
    billing_source jsonb,
    koku_uuid text,
    pending_delete boolean NOT NULL,
    pending_update boolean NOT NULL,
    out_of_order_delete boolean NOT NULL,
    status jsonb,
    paused boolean NOT NULL,
    provider_id uuid,
    additional_context jsonb,
    org_id text
);


ALTER TABLE public.api_sources OWNER TO postgres;

--
-- Name: api_tenant; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_tenant (
    id integer NOT NULL,
    schema_name character varying(63) NOT NULL
);


ALTER TABLE public.api_tenant OWNER TO postgres;

--
-- Name: api_tenant_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_tenant ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_tenant_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: api_user; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_user (
    id integer NOT NULL,
    uuid uuid NOT NULL,
    username character varying(150) NOT NULL,
    email character varying(254) NOT NULL,
    date_created timestamp with time zone NOT NULL,
    is_active boolean,
    customer_id integer
);


ALTER TABLE public.api_user OWNER TO postgres;

--
-- Name: api_user_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.api_user ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.api_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: auth_group; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_group (
    id integer NOT NULL,
    name character varying(150) NOT NULL
);


ALTER TABLE public.auth_group OWNER TO postgres;

--
-- Name: auth_group_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.auth_group ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.auth_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: auth_group_permissions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_group_permissions (
    id integer NOT NULL,
    group_id integer NOT NULL,
    permission_id integer NOT NULL
);


ALTER TABLE public.auth_group_permissions OWNER TO postgres;

--
-- Name: auth_group_permissions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.auth_group_permissions ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.auth_group_permissions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: auth_permission; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_permission (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    content_type_id integer NOT NULL,
    codename character varying(100) NOT NULL
);


ALTER TABLE public.auth_permission OWNER TO postgres;

--
-- Name: auth_permission_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.auth_permission ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.auth_permission_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: auth_user; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_user (
    id integer NOT NULL,
    password character varying(128) NOT NULL,
    last_login timestamp with time zone,
    is_superuser boolean NOT NULL,
    username character varying(150) NOT NULL,
    first_name character varying(150) NOT NULL,
    last_name character varying(150) NOT NULL,
    email character varying(254) NOT NULL,
    is_staff boolean NOT NULL,
    is_active boolean NOT NULL,
    date_joined timestamp with time zone NOT NULL
);


ALTER TABLE public.auth_user OWNER TO postgres;

--
-- Name: auth_user_groups; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_user_groups (
    id integer NOT NULL,
    user_id integer NOT NULL,
    group_id integer NOT NULL
);


ALTER TABLE public.auth_user_groups OWNER TO postgres;

--
-- Name: auth_user_groups_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.auth_user_groups ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.auth_user_groups_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: auth_user_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.auth_user ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.auth_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: auth_user_user_permissions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_user_user_permissions (
    id integer NOT NULL,
    user_id integer NOT NULL,
    permission_id integer NOT NULL
);


ALTER TABLE public.auth_user_user_permissions OWNER TO postgres;

--
-- Name: auth_user_user_permissions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.auth_user_user_permissions ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.auth_user_user_permissions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: aux_table; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.aux_table (
    mt_key1 character varying(128) NOT NULL,
    mt_key2 bigint NOT NULL,
    mt_comment character varying(255)
);


ALTER TABLE public.aux_table OWNER TO postgres;

--
-- Name: compaction_queue; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.compaction_queue (
    cq_id bigint NOT NULL,
    cq_database character varying(128) NOT NULL,
    cq_table character varying(128) NOT NULL,
    cq_partition character varying(767),
    cq_state character(1) NOT NULL,
    cq_type character(1) NOT NULL,
    cq_tblproperties character varying(2048),
    cq_worker_id character varying(128),
    cq_start bigint,
    cq_run_as character varying(128),
    cq_highest_write_id bigint,
    cq_meta_info bytea,
    cq_hadoop_job_id character varying(32)
);


ALTER TABLE public.compaction_queue OWNER TO postgres;

--
-- Name: completed_compactions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.completed_compactions (
    cc_id bigint NOT NULL,
    cc_database character varying(128) NOT NULL,
    cc_table character varying(128) NOT NULL,
    cc_partition character varying(767),
    cc_state character(1) NOT NULL,
    cc_type character(1) NOT NULL,
    cc_tblproperties character varying(2048),
    cc_worker_id character varying(128),
    cc_start bigint,
    cc_end bigint,
    cc_run_as character varying(128),
    cc_highest_write_id bigint,
    cc_meta_info bytea,
    cc_hadoop_job_id character varying(32)
);


ALTER TABLE public.completed_compactions OWNER TO postgres;

--
-- Name: completed_txn_components; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.completed_txn_components (
    ctc_txnid bigint NOT NULL,
    ctc_database character varying(128) NOT NULL,
    ctc_table character varying(256),
    ctc_partition character varying(767),
    ctc_timestamp timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ctc_writeid bigint,
    ctc_update_delete character(1) NOT NULL
);


ALTER TABLE public.completed_txn_components OWNER TO postgres;

--
-- Name: delayed_celery_tasks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.delayed_celery_tasks (
    id integer NOT NULL,
    task_name character varying(255) NOT NULL,
    task_args jsonb NOT NULL,
    task_kwargs jsonb NOT NULL,
    timeout_timestamp timestamp with time zone NOT NULL,
    provider_uuid uuid NOT NULL,
    queue_name character varying(255) NOT NULL,
    metadata jsonb
);


ALTER TABLE public.delayed_celery_tasks OWNER TO postgres;

--
-- Name: delayed_celery_tasks_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.delayed_celery_tasks ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.delayed_celery_tasks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: django_content_type; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.django_content_type (
    id integer NOT NULL,
    app_label character varying(100) NOT NULL,
    model character varying(100) NOT NULL
);


ALTER TABLE public.django_content_type OWNER TO postgres;

--
-- Name: django_content_type_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.django_content_type ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.django_content_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: django_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.django_migrations (
    id integer NOT NULL,
    app character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    applied timestamp with time zone NOT NULL
);


ALTER TABLE public.django_migrations OWNER TO postgres;

--
-- Name: django_migrations_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.django_migrations ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.django_migrations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: django_session; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.django_session (
    session_key character varying(40) NOT NULL,
    session_data text NOT NULL,
    expire_date timestamp with time zone NOT NULL
);


ALTER TABLE public.django_session OWNER TO postgres;

--
-- Name: hive_locks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.hive_locks (
    hl_lock_ext_id bigint NOT NULL,
    hl_lock_int_id bigint NOT NULL,
    hl_txnid bigint NOT NULL,
    hl_db character varying(128) NOT NULL,
    hl_table character varying(128),
    hl_partition character varying(767) DEFAULT NULL::character varying,
    hl_lock_state character(1) NOT NULL,
    hl_lock_type character(1) NOT NULL,
    hl_last_heartbeat bigint NOT NULL,
    hl_acquired_at bigint,
    hl_user character varying(128) NOT NULL,
    hl_host character varying(128) NOT NULL,
    hl_heartbeat_count integer,
    hl_agent_info character varying(128),
    hl_blockedby_ext_id bigint,
    hl_blockedby_int_id bigint
);


ALTER TABLE public.hive_locks OWNER TO postgres;

--
-- Name: materialization_rebuild_locks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.materialization_rebuild_locks (
    mrl_txn_id bigint NOT NULL,
    mrl_db_name character varying(128) NOT NULL,
    mrl_tbl_name character varying(256) NOT NULL,
    mrl_last_heartbeat bigint NOT NULL
);


ALTER TABLE public.materialization_rebuild_locks OWNER TO postgres;

--
-- Name: min_history_level; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.min_history_level (
    mhl_txnid bigint NOT NULL,
    mhl_min_open_txnid bigint NOT NULL
);


ALTER TABLE public.min_history_level OWNER TO postgres;

--
-- Name: next_compaction_queue_id; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.next_compaction_queue_id (
    ncq_next bigint NOT NULL
);


ALTER TABLE public.next_compaction_queue_id OWNER TO postgres;

--
-- Name: next_lock_id; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.next_lock_id (
    nl_next bigint NOT NULL
);


ALTER TABLE public.next_lock_id OWNER TO postgres;

--
-- Name: next_txn_id; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.next_txn_id (
    ntxn_next bigint NOT NULL
);


ALTER TABLE public.next_txn_id OWNER TO postgres;

--
-- Name: next_write_id; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.next_write_id (
    nwi_database character varying(128) NOT NULL,
    nwi_table character varying(256) NOT NULL,
    nwi_next bigint NOT NULL
);


ALTER TABLE public.next_write_id OWNER TO postgres;

--
-- Name: region_mapping; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.region_mapping (
    id integer NOT NULL,
    region character varying(32) NOT NULL,
    region_name character varying(64) NOT NULL
);


ALTER TABLE public.region_mapping OWNER TO postgres;

--
-- Name: region_mapping_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.region_mapping ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.region_mapping_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: repl_txn_map; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.repl_txn_map (
    rtm_repl_policy character varying(256) NOT NULL,
    rtm_src_txn_id bigint NOT NULL,
    rtm_target_txn_id bigint NOT NULL
);


ALTER TABLE public.repl_txn_map OWNER TO postgres;

--
-- Name: reporting_common_costusagereportmanifest; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.reporting_common_costusagereportmanifest (
    id integer NOT NULL,
    assembly_id text NOT NULL,
    billing_period_start_datetime timestamp with time zone NOT NULL,
    num_total_files integer NOT NULL,
    provider_id uuid NOT NULL,
    s3_csv_cleared boolean,
    s3_parquet_cleared boolean,
    operator_version text,
    cluster_channel text,
    operator_airgapped boolean,
    operator_certified boolean,
    operator_errors jsonb,
    cluster_id text,
    report_tracker jsonb,
    operator_daily_reports boolean,
    s3_parquet_cleared_tracker jsonb,
    daily_archive_start_date timestamp with time zone,
    creation_datetime timestamp with time zone,
    export_datetime timestamp with time zone,
    completed_datetime timestamp with time zone,
    state jsonb
);


ALTER TABLE public.reporting_common_costusagereportmanifest OWNER TO postgres;

--
-- Name: reporting_common_costusagereportmanifest_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.reporting_common_costusagereportmanifest ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.reporting_common_costusagereportmanifest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: reporting_common_costusagereportstatus; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.reporting_common_costusagereportstatus (
    id integer NOT NULL,
    report_name character varying(128) NOT NULL,
    etag character varying(64),
    manifest_id integer,
    started_datetime timestamp with time zone,
    completed_datetime timestamp with time zone,
    celery_task_id uuid,
    failed_status integer,
    status integer NOT NULL
);


ALTER TABLE public.reporting_common_costusagereportstatus OWNER TO postgres;

--
-- Name: reporting_common_costusagereportstatus_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.reporting_common_costusagereportstatus ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.reporting_common_costusagereportstatus_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: reporting_common_diskcapacity; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.reporting_common_diskcapacity (
    product_substring character varying(20) NOT NULL,
    capacity integer NOT NULL,
    provider_type character varying(50) NOT NULL
);


ALTER TABLE public.reporting_common_diskcapacity OWNER TO postgres;

--
-- Name: runtime_stats; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.runtime_stats (
    rs_id bigint NOT NULL,
    create_time bigint NOT NULL,
    weight bigint NOT NULL,
    payload bytea
);


ALTER TABLE public.runtime_stats OWNER TO postgres;

--
-- Name: txn_components; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.txn_components (
    tc_txnid bigint NOT NULL,
    tc_database character varying(128) NOT NULL,
    tc_table character varying(128),
    tc_partition character varying(767) DEFAULT NULL::character varying,
    tc_operation_type character(1) NOT NULL,
    tc_writeid bigint
);


ALTER TABLE public.txn_components OWNER TO postgres;

--
-- Name: txn_to_write_id; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.txn_to_write_id (
    t2w_txnid bigint NOT NULL,
    t2w_database character varying(128) NOT NULL,
    t2w_table character varying(256) NOT NULL,
    t2w_writeid bigint NOT NULL
);


ALTER TABLE public.txn_to_write_id OWNER TO postgres;

--
-- Name: txns; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.txns (
    txn_id bigint NOT NULL,
    txn_state character(1) NOT NULL,
    txn_started bigint NOT NULL,
    txn_last_heartbeat bigint NOT NULL,
    txn_user character varying(128) NOT NULL,
    txn_host character varying(128) NOT NULL,
    txn_agent_info character varying(128),
    txn_meta_info character varying(128),
    txn_heartbeat_count integer,
    txn_type integer
);


ALTER TABLE public.txns OWNER TO postgres;

--
-- Name: worker_cache_table; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.worker_cache_table (
    cache_key character varying(255) NOT NULL,
    value text NOT NULL,
    expires timestamp with time zone NOT NULL
);


ALTER TABLE public.worker_cache_table OWNER TO postgres;

--
-- Name: write_set; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.write_set (
    ws_database character varying(128) NOT NULL,
    ws_table character varying(128) NOT NULL,
    ws_partition character varying(767),
    ws_txnid bigint NOT NULL,
    ws_commit_id bigint NOT NULL,
    ws_operation_type character(1) NOT NULL
);


ALTER TABLE public.write_set OWNER TO postgres;

--
-- Name: MASTER_KEYS KEY_ID; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."MASTER_KEYS" ALTER COLUMN "KEY_ID" SET DEFAULT nextval('public."MASTER_KEYS_KEY_ID_seq"'::regclass);


--
-- Data for Name: BUCKETING_COLS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."BUCKETING_COLS" ("SD_ID", "BUCKET_COL_NAME", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: CDS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."CDS" ("CD_ID") FROM stdin;
\.


--
-- Data for Name: COLUMNS_V2; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."COLUMNS_V2" ("CD_ID", "COMMENT", "COLUMN_NAME", "TYPE_NAME", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: CTLGS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."CTLGS" ("CTLG_ID", "NAME", "DESC", "LOCATION_URI") FROM stdin;
1	hive	Default catalog for Hive	s3a://koku-bucket/data
\.


--
-- Data for Name: DATABASE_PARAMS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."DATABASE_PARAMS" ("DB_ID", "PARAM_KEY", "PARAM_VALUE") FROM stdin;
\.


--
-- Data for Name: DBS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."DBS" ("DB_ID", "DESC", "DB_LOCATION_URI", "NAME", "OWNER_NAME", "OWNER_TYPE", "CTLG_NAME") FROM stdin;
1	Default Hive database	s3a://koku-bucket/data	default	public	ROLE	hive
\.


--
-- Data for Name: DB_PRIVS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."DB_PRIVS" ("DB_GRANT_ID", "CREATE_TIME", "DB_ID", "GRANT_OPTION", "GRANTOR", "GRANTOR_TYPE", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "DB_PRIV", "AUTHORIZER") FROM stdin;
\.


--
-- Data for Name: DELEGATION_TOKENS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."DELEGATION_TOKENS" ("TOKEN_IDENT", "TOKEN") FROM stdin;
\.


--
-- Data for Name: FUNCS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."FUNCS" ("FUNC_ID", "CLASS_NAME", "CREATE_TIME", "DB_ID", "FUNC_NAME", "FUNC_TYPE", "OWNER_NAME", "OWNER_TYPE") FROM stdin;
\.


--
-- Data for Name: FUNC_RU; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."FUNC_RU" ("FUNC_ID", "RESOURCE_TYPE", "RESOURCE_URI", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: GLOBAL_PRIVS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."GLOBAL_PRIVS" ("USER_GRANT_ID", "CREATE_TIME", "GRANT_OPTION", "GRANTOR", "GRANTOR_TYPE", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "USER_PRIV", "AUTHORIZER") FROM stdin;
1	1746826961	1	admin	ROLE	admin	ROLE	All	SQL
\.


--
-- Data for Name: IDXS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."IDXS" ("INDEX_ID", "CREATE_TIME", "DEFERRED_REBUILD", "INDEX_HANDLER_CLASS", "INDEX_NAME", "INDEX_TBL_ID", "LAST_ACCESS_TIME", "ORIG_TBL_ID", "SD_ID") FROM stdin;
\.


--
-- Data for Name: INDEX_PARAMS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."INDEX_PARAMS" ("INDEX_ID", "PARAM_KEY", "PARAM_VALUE") FROM stdin;
\.


--
-- Data for Name: I_SCHEMA; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."I_SCHEMA" ("SCHEMA_ID", "SCHEMA_TYPE", "NAME", "DB_ID", "COMPATIBILITY", "VALIDATION_LEVEL", "CAN_EVOLVE", "SCHEMA_GROUP", "DESCRIPTION") FROM stdin;
\.


--
-- Data for Name: KEY_CONSTRAINTS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."KEY_CONSTRAINTS" ("CHILD_CD_ID", "CHILD_INTEGER_IDX", "CHILD_TBL_ID", "PARENT_CD_ID", "PARENT_INTEGER_IDX", "PARENT_TBL_ID", "POSITION", "CONSTRAINT_NAME", "CONSTRAINT_TYPE", "UPDATE_RULE", "DELETE_RULE", "ENABLE_VALIDATE_RELY", "DEFAULT_VALUE") FROM stdin;
\.


--
-- Data for Name: MASTER_KEYS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."MASTER_KEYS" ("KEY_ID", "MASTER_KEY") FROM stdin;
\.


--
-- Data for Name: METASTORE_DB_PROPERTIES; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."METASTORE_DB_PROPERTIES" ("PROPERTY_KEY", "PROPERTY_VALUE", "DESCRIPTION") FROM stdin;
\.


--
-- Data for Name: MV_CREATION_METADATA; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."MV_CREATION_METADATA" ("MV_CREATION_METADATA_ID", "CAT_NAME", "DB_NAME", "TBL_NAME", "TXN_LIST", "MATERIALIZATION_TIME") FROM stdin;
\.


--
-- Data for Name: MV_TABLES_USED; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."MV_TABLES_USED" ("MV_CREATION_METADATA_ID", "TBL_ID") FROM stdin;
\.


--
-- Data for Name: NOTIFICATION_LOG; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."NOTIFICATION_LOG" ("NL_ID", "EVENT_ID", "EVENT_TIME", "EVENT_TYPE", "CAT_NAME", "DB_NAME", "TBL_NAME", "MESSAGE", "MESSAGE_FORMAT") FROM stdin;
\.


--
-- Data for Name: NOTIFICATION_SEQUENCE; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."NOTIFICATION_SEQUENCE" ("NNI_ID", "NEXT_EVENT_ID") FROM stdin;
1	1
\.


--
-- Data for Name: NUCLEUS_TABLES; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."NUCLEUS_TABLES" ("CLASS_NAME", "TABLE_NAME", "TYPE", "OWNER", "VERSION", "INTERFACE_NAME") FROM stdin;
\.


--
-- Data for Name: PARTITIONS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."PARTITIONS" ("PART_ID", "CREATE_TIME", "LAST_ACCESS_TIME", "PART_NAME", "SD_ID", "TBL_ID") FROM stdin;
\.


--
-- Data for Name: PARTITION_EVENTS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."PARTITION_EVENTS" ("PART_NAME_ID", "CAT_NAME", "DB_NAME", "EVENT_TIME", "EVENT_TYPE", "PARTITION_NAME", "TBL_NAME") FROM stdin;
\.


--
-- Data for Name: PARTITION_KEYS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."PARTITION_KEYS" ("TBL_ID", "PKEY_COMMENT", "PKEY_NAME", "PKEY_TYPE", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: PARTITION_KEY_VALS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."PARTITION_KEY_VALS" ("PART_ID", "PART_KEY_VAL", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: PARTITION_PARAMS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."PARTITION_PARAMS" ("PART_ID", "PARAM_KEY", "PARAM_VALUE") FROM stdin;
\.


--
-- Data for Name: PART_COL_PRIVS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."PART_COL_PRIVS" ("PART_COLUMN_GRANT_ID", "COLUMN_NAME", "CREATE_TIME", "GRANT_OPTION", "GRANTOR", "GRANTOR_TYPE", "PART_ID", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "PART_COL_PRIV", "AUTHORIZER") FROM stdin;
\.


--
-- Data for Name: PART_COL_STATS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."PART_COL_STATS" ("CS_ID", "CAT_NAME", "DB_NAME", "TABLE_NAME", "PARTITION_NAME", "COLUMN_NAME", "COLUMN_TYPE", "PART_ID", "LONG_LOW_VALUE", "LONG_HIGH_VALUE", "DOUBLE_LOW_VALUE", "DOUBLE_HIGH_VALUE", "BIG_DECIMAL_LOW_VALUE", "BIG_DECIMAL_HIGH_VALUE", "NUM_NULLS", "NUM_DISTINCTS", "BIT_VECTOR", "AVG_COL_LEN", "MAX_COL_LEN", "NUM_TRUES", "NUM_FALSES", "LAST_ANALYZED") FROM stdin;
\.


--
-- Data for Name: PART_PRIVS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."PART_PRIVS" ("PART_GRANT_ID", "CREATE_TIME", "GRANT_OPTION", "GRANTOR", "GRANTOR_TYPE", "PART_ID", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "PART_PRIV", "AUTHORIZER") FROM stdin;
\.


--
-- Data for Name: ROLES; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."ROLES" ("ROLE_ID", "CREATE_TIME", "OWNER_NAME", "ROLE_NAME") FROM stdin;
1	1746826961	admin	admin
2	1746826961	public	public
\.


--
-- Data for Name: ROLE_MAP; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."ROLE_MAP" ("ROLE_GRANT_ID", "ADD_TIME", "GRANT_OPTION", "GRANTOR", "GRANTOR_TYPE", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "ROLE_ID") FROM stdin;
\.


--
-- Data for Name: SCHEMA_VERSION; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SCHEMA_VERSION" ("SCHEMA_VERSION_ID", "SCHEMA_ID", "VERSION", "CREATED_AT", "CD_ID", "STATE", "DESCRIPTION", "SCHEMA_TEXT", "FINGERPRINT", "SCHEMA_VERSION_NAME", "SERDE_ID") FROM stdin;
\.


--
-- Data for Name: SDS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SDS" ("SD_ID", "INPUT_FORMAT", "IS_COMPRESSED", "LOCATION", "NUM_BUCKETS", "OUTPUT_FORMAT", "SERDE_ID", "CD_ID", "IS_STOREDASSUBDIRECTORIES") FROM stdin;
\.


--
-- Data for Name: SD_PARAMS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SD_PARAMS" ("SD_ID", "PARAM_KEY", "PARAM_VALUE") FROM stdin;
\.


--
-- Data for Name: SEQUENCE_TABLE; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SEQUENCE_TABLE" ("SEQUENCE_NAME", "NEXT_VAL") FROM stdin;
org.apache.hadoop.hive.metastore.model.MNotificationLog	1
org.apache.hadoop.hive.metastore.model.MDatabase	6
org.apache.hadoop.hive.metastore.model.MRole	6
org.apache.hadoop.hive.metastore.model.MGlobalPrivilege	6
\.


--
-- Data for Name: SERDES; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SERDES" ("SERDE_ID", "NAME", "SLIB", "DESCRIPTION", "SERIALIZER_CLASS", "DESERIALIZER_CLASS", "SERDE_TYPE") FROM stdin;
\.


--
-- Data for Name: SERDE_PARAMS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SERDE_PARAMS" ("SERDE_ID", "PARAM_KEY", "PARAM_VALUE") FROM stdin;
\.


--
-- Data for Name: SKEWED_COL_NAMES; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SKEWED_COL_NAMES" ("SD_ID", "SKEWED_COL_NAME", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: SKEWED_COL_VALUE_LOC_MAP; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SKEWED_COL_VALUE_LOC_MAP" ("SD_ID", "STRING_LIST_ID_KID", "LOCATION") FROM stdin;
\.


--
-- Data for Name: SKEWED_STRING_LIST; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SKEWED_STRING_LIST" ("STRING_LIST_ID") FROM stdin;
\.


--
-- Data for Name: SKEWED_STRING_LIST_VALUES; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SKEWED_STRING_LIST_VALUES" ("STRING_LIST_ID", "STRING_LIST_VALUE", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: SKEWED_VALUES; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SKEWED_VALUES" ("SD_ID_OID", "STRING_LIST_ID_EID", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: SORT_COLS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."SORT_COLS" ("SD_ID", "COLUMN_NAME", "ORDER", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: TABLE_PARAMS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."TABLE_PARAMS" ("TBL_ID", "PARAM_KEY", "PARAM_VALUE") FROM stdin;
\.


--
-- Data for Name: TAB_COL_STATS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."TAB_COL_STATS" ("CS_ID", "CAT_NAME", "DB_NAME", "TABLE_NAME", "COLUMN_NAME", "COLUMN_TYPE", "TBL_ID", "LONG_LOW_VALUE", "LONG_HIGH_VALUE", "DOUBLE_LOW_VALUE", "DOUBLE_HIGH_VALUE", "BIG_DECIMAL_LOW_VALUE", "BIG_DECIMAL_HIGH_VALUE", "NUM_NULLS", "NUM_DISTINCTS", "BIT_VECTOR", "AVG_COL_LEN", "MAX_COL_LEN", "NUM_TRUES", "NUM_FALSES", "LAST_ANALYZED") FROM stdin;
\.


--
-- Data for Name: TBLS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."TBLS" ("TBL_ID", "CREATE_TIME", "DB_ID", "LAST_ACCESS_TIME", "OWNER", "OWNER_TYPE", "RETENTION", "SD_ID", "TBL_NAME", "TBL_TYPE", "VIEW_EXPANDED_TEXT", "VIEW_ORIGINAL_TEXT", "IS_REWRITE_ENABLED") FROM stdin;
\.


--
-- Data for Name: TBL_COL_PRIVS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."TBL_COL_PRIVS" ("TBL_COLUMN_GRANT_ID", "COLUMN_NAME", "CREATE_TIME", "GRANT_OPTION", "GRANTOR", "GRANTOR_TYPE", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "TBL_COL_PRIV", "TBL_ID", "AUTHORIZER") FROM stdin;
\.


--
-- Data for Name: TBL_PRIVS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."TBL_PRIVS" ("TBL_GRANT_ID", "CREATE_TIME", "GRANT_OPTION", "GRANTOR", "GRANTOR_TYPE", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "TBL_PRIV", "TBL_ID", "AUTHORIZER") FROM stdin;
\.


--
-- Data for Name: TYPES; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."TYPES" ("TYPES_ID", "TYPE_NAME", "TYPE1", "TYPE2") FROM stdin;
\.


--
-- Data for Name: TYPE_FIELDS; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."TYPE_FIELDS" ("TYPE_NAME", "COMMENT", "FIELD_NAME", "FIELD_TYPE", "INTEGER_IDX") FROM stdin;
\.


--
-- Data for Name: VERSION; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."VERSION" ("VER_ID", "SCHEMA_VERSION", "VERSION_COMMENT") FROM stdin;
1	3.1.0	Hive release version 3.1.0
\.


--
-- Data for Name: WM_MAPPING; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."WM_MAPPING" ("MAPPING_ID", "RP_ID", "ENTITY_TYPE", "ENTITY_NAME", "POOL_ID", "ORDERING") FROM stdin;
\.


--
-- Data for Name: WM_POOL; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."WM_POOL" ("POOL_ID", "RP_ID", "PATH", "ALLOC_FRACTION", "QUERY_PARALLELISM", "SCHEDULING_POLICY") FROM stdin;
\.


--
-- Data for Name: WM_POOL_TO_TRIGGER; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."WM_POOL_TO_TRIGGER" ("POOL_ID", "TRIGGER_ID") FROM stdin;
\.


--
-- Data for Name: WM_RESOURCEPLAN; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."WM_RESOURCEPLAN" ("RP_ID", "NAME", "QUERY_PARALLELISM", "STATUS", "DEFAULT_POOL_ID") FROM stdin;
\.


--
-- Data for Name: WM_TRIGGER; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."WM_TRIGGER" ("TRIGGER_ID", "RP_ID", "NAME", "TRIGGER_EXPRESSION", "ACTION_EXPRESSION", "IS_IN_UNMANAGED") FROM stdin;
\.


--
-- Data for Name: __customer_total_data; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.__customer_total_data (id, date, total_aws_unblended_cost, total_aws_calculated_amortized_cost, total_azure_pretax_cost, total_gcp_unblended_cost, total_gcp_total, total_oci_total, total_total_infrastructure_raw_cost, total_total_cost_model_costs, total_infra_total_cost_model, total_infra_cost_model_cpu_cost, total_infra_cost_model_memory_cost, total_infra_cost_model_volume_cost, total_sup_total_cost_model, total_sup_cost_model_cpu_cost, total_sup_cost_model_memory_cost, total_sup_cost_model_volume_cost, total_cluster_count, total_node_count, total_infra_node_count, total_control_plane_node_count, total_worker_node_count, total_infra_node_cpu_cores, total_control_plane_node_cpu_cores, total_worker_node_cpu_cores, total_infra_node_mem_gb, total_control_plane_node_mem_gb, total_worker_node_mem_gb, total_pvc_count, total_cluster_capacity_cores, total_cluster_capacity_core_hours, total_cluster_capacity_memory_gb, total_cluster_capacity_memory_gb_hours, total_volume_request_gb, total_volume_request_gb_mo, total_pvc_capacity_gb, total_pvc_capacity_gb_mo) FROM stdin;
\.


--
-- Data for Name: api_customer; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_customer (id, date_created, date_updated, uuid, account_id, schema_name, org_id) FROM stdin;
\.


--
-- Data for Name: api_domain; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_domain (id, domain, is_primary, tenant_id) FROM stdin;
\.


--
-- Data for Name: api_exchangeratedictionary; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_exchangeratedictionary (id, currency_exchange_dictionary) FROM stdin;
\.


--
-- Data for Name: api_exchangerates; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_exchangerates (id, currency_type, exchange_rate) FROM stdin;
\.


--
-- Data for Name: api_provider; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_provider (uuid, name, type, setup_complete, created_timestamp, data_updated_timestamp, active, authentication_id, billing_source_id, created_by_id, customer_id, infrastructure_id, paused, additional_context, polling_timestamp) FROM stdin;
\.


--
-- Data for Name: api_providerauthentication; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_providerauthentication (id, uuid, credentials) FROM stdin;
\.


--
-- Data for Name: api_providerbillingsource; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_providerbillingsource (id, uuid, data_source) FROM stdin;
\.


--
-- Data for Name: api_providerinfrastructuremap; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_providerinfrastructuremap (id, infrastructure_type, infrastructure_provider_id) FROM stdin;
\.


--
-- Data for Name: api_sources; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_sources (source_id, source_uuid, name, auth_header, "offset", account_id, source_type, authentication, billing_source, koku_uuid, pending_delete, pending_update, out_of_order_delete, status, paused, provider_id, additional_context, org_id) FROM stdin;
\.


--
-- Data for Name: api_tenant; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_tenant (id, schema_name) FROM stdin;
1	public
\.


--
-- Data for Name: api_user; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_user (id, uuid, username, email, date_created, is_active, customer_id) FROM stdin;
\.


--
-- Data for Name: auth_group; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_group (id, name) FROM stdin;
\.


--
-- Data for Name: auth_group_permissions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_group_permissions (id, group_id, permission_id) FROM stdin;
\.


--
-- Data for Name: auth_permission; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_permission (id, name, content_type_id, codename) FROM stdin;
1	Can add permission	1	add_permission
2	Can change permission	1	change_permission
3	Can delete permission	1	delete_permission
4	Can view permission	1	view_permission
5	Can add group	2	add_group
6	Can change group	2	change_group
7	Can delete group	2	delete_group
8	Can view group	2	view_group
9	Can add user	3	add_user
10	Can change user	3	change_user
11	Can delete user	3	delete_user
12	Can view user	3	view_user
13	Can add content type	4	add_contenttype
14	Can change content type	4	change_contenttype
15	Can delete content type	4	delete_contenttype
16	Can view content type	4	view_contenttype
17	Can add session	5	add_session
18	Can change session	5	change_session
19	Can delete session	5	delete_session
20	Can view session	5	view_session
21	Can add customer	6	add_customer
22	Can change customer	6	change_customer
23	Can delete customer	6	delete_customer
24	Can view customer	6	view_customer
25	Can add provider	7	add_provider
26	Can change provider	7	change_provider
27	Can delete provider	7	delete_provider
28	Can view provider	7	view_provider
29	Can add provider authentication	8	add_providerauthentication
30	Can change provider authentication	8	change_providerauthentication
31	Can delete provider authentication	8	delete_providerauthentication
32	Can view provider authentication	8	view_providerauthentication
33	Can add provider billing source	9	add_providerbillingsource
34	Can change provider billing source	9	change_providerbillingsource
35	Can delete provider billing source	9	delete_providerbillingsource
36	Can view provider billing source	9	view_providerbillingsource
37	Can add sources	10	add_sources
38	Can change sources	10	change_sources
39	Can delete sources	10	delete_sources
40	Can view sources	10	view_sources
41	Can add tenant	11	add_tenant
42	Can change tenant	11	change_tenant
43	Can delete tenant	11	delete_tenant
44	Can view tenant	11	view_tenant
45	Can add user	12	add_user
46	Can change user	12	change_user
47	Can delete user	12	delete_user
48	Can view user	12	view_user
49	Can add provider infrastructure map	13	add_providerinfrastructuremap
50	Can change provider infrastructure map	13	change_providerinfrastructuremap
51	Can delete provider infrastructure map	13	delete_providerinfrastructuremap
52	Can view provider infrastructure map	13	view_providerinfrastructuremap
53	Can add exchange rates	14	add_exchangerates
54	Can change exchange rates	14	change_exchangerates
55	Can delete exchange rates	14	delete_exchangerates
56	Can view exchange rates	14	view_exchangerates
57	Can add exchange rate dictionary	15	add_exchangeratedictionary
58	Can change exchange rate dictionary	15	change_exchangeratedictionary
59	Can delete exchange rate dictionary	15	delete_exchangeratedictionary
60	Can view exchange rate dictionary	15	view_exchangeratedictionary
61	Can add domain	16	add_domain
62	Can change domain	16	change_domain
63	Can delete domain	16	delete_domain
64	Can view domain	16	view_domain
65	Can add customer total data	17	add_customertotaldata
66	Can change customer total data	17	change_customertotaldata
67	Can delete customer total data	17	delete_customertotaldata
68	Can view customer total data	17	view_customertotaldata
69	Can add tenant api provider	18	add_tenantapiprovider
70	Can change tenant api provider	18	change_tenantapiprovider
71	Can delete tenant api provider	18	delete_tenantapiprovider
72	Can view tenant api provider	18	view_tenantapiprovider
73	Can add openshift cost category	19	add_openshiftcostcategory
74	Can change openshift cost category	19	change_openshiftcostcategory
75	Can delete openshift cost category	19	delete_openshiftcostcategory
76	Can view openshift cost category	19	view_openshiftcostcategory
77	Can add ingress reports	20	add_ingressreports
78	Can change ingress reports	20	change_ingressreports
79	Can delete ingress reports	20	delete_ingressreports
80	Can view ingress reports	20	view_ingressreports
81	Can add ocp all cost line item daily summary	21	add_ocpallcostlineitemdailysummary
82	Can change ocp all cost line item daily summary	21	change_ocpallcostlineitemdailysummary
83	Can delete ocp all cost line item daily summary	21	delete_ocpallcostlineitemdailysummary
84	Can view ocp all cost line item daily summary	21	view_ocpallcostlineitemdailysummary
85	Can add ocp all cost line item project daily summary	22	add_ocpallcostlineitemprojectdailysummary
86	Can change ocp all cost line item project daily summary	22	change_ocpallcostlineitemprojectdailysummary
87	Can delete ocp all cost line item project daily summary	22	delete_ocpallcostlineitemprojectdailysummary
88	Can view ocp all cost line item project daily summary	22	view_ocpallcostlineitemprojectdailysummary
89	Can add aws account alias	23	add_awsaccountalias
90	Can change aws account alias	23	change_awsaccountalias
91	Can delete aws account alias	23	delete_awsaccountalias
92	Can view aws account alias	23	view_awsaccountalias
93	Can add aws organizational unit	24	add_awsorganizationalunit
94	Can change aws organizational unit	24	change_awsorganizationalunit
95	Can delete aws organizational unit	24	delete_awsorganizationalunit
96	Can view aws organizational unit	24	view_awsorganizationalunit
97	Can add aws compute summary by account p	25	add_awscomputesummarybyaccountp
98	Can change aws compute summary by account p	25	change_awscomputesummarybyaccountp
99	Can delete aws compute summary by account p	25	delete_awscomputesummarybyaccountp
100	Can view aws compute summary by account p	25	view_awscomputesummarybyaccountp
101	Can add aws compute summary p	26	add_awscomputesummaryp
102	Can change aws compute summary p	26	change_awscomputesummaryp
103	Can delete aws compute summary p	26	delete_awscomputesummaryp
104	Can view aws compute summary p	26	view_awscomputesummaryp
105	Can add aws cost entry bill	27	add_awscostentrybill
106	Can change aws cost entry bill	27	change_awscostentrybill
107	Can delete aws cost entry bill	27	delete_awscostentrybill
108	Can view aws cost entry bill	27	view_awscostentrybill
109	Can add aws cost entry line item daily summary	28	add_awscostentrylineitemdailysummary
110	Can change aws cost entry line item daily summary	28	change_awscostentrylineitemdailysummary
111	Can delete aws cost entry line item daily summary	28	delete_awscostentrylineitemdailysummary
112	Can view aws cost entry line item daily summary	28	view_awscostentrylineitemdailysummary
113	Can add aws cost summary by account p	29	add_awscostsummarybyaccountp
114	Can change aws cost summary by account p	29	change_awscostsummarybyaccountp
115	Can delete aws cost summary by account p	29	delete_awscostsummarybyaccountp
116	Can view aws cost summary by account p	29	view_awscostsummarybyaccountp
117	Can add aws cost summary by region p	30	add_awscostsummarybyregionp
118	Can change aws cost summary by region p	30	change_awscostsummarybyregionp
119	Can delete aws cost summary by region p	30	delete_awscostsummarybyregionp
120	Can view aws cost summary by region p	30	view_awscostsummarybyregionp
121	Can add aws cost summary by service p	31	add_awscostsummarybyservicep
122	Can change aws cost summary by service p	31	change_awscostsummarybyservicep
123	Can delete aws cost summary by service p	31	delete_awscostsummarybyservicep
124	Can view aws cost summary by service p	31	view_awscostsummarybyservicep
125	Can add aws cost summary p	32	add_awscostsummaryp
126	Can change aws cost summary p	32	change_awscostsummaryp
127	Can delete aws cost summary p	32	delete_awscostsummaryp
128	Can view aws cost summary p	32	view_awscostsummaryp
129	Can add aws database summary p	33	add_awsdatabasesummaryp
130	Can change aws database summary p	33	change_awsdatabasesummaryp
131	Can delete aws database summary p	33	delete_awsdatabasesummaryp
132	Can view aws database summary p	33	view_awsdatabasesummaryp
133	Can add aws network summary p	34	add_awsnetworksummaryp
134	Can change aws network summary p	34	change_awsnetworksummaryp
135	Can delete aws network summary p	34	delete_awsnetworksummaryp
136	Can view aws network summary p	34	view_awsnetworksummaryp
137	Can add aws storage summary by account p	35	add_awsstoragesummarybyaccountp
138	Can change aws storage summary by account p	35	change_awsstoragesummarybyaccountp
139	Can delete aws storage summary by account p	35	delete_awsstoragesummarybyaccountp
140	Can view aws storage summary by account p	35	view_awsstoragesummarybyaccountp
141	Can add aws storage summary p	36	add_awsstoragesummaryp
142	Can change aws storage summary p	36	change_awsstoragesummaryp
143	Can delete aws storage summary p	36	delete_awsstoragesummaryp
144	Can view aws storage summary p	36	view_awsstoragesummaryp
145	Can add aws tags summary	37	add_awstagssummary
146	Can change aws tags summary	37	change_awstagssummary
147	Can delete aws tags summary	37	delete_awstagssummary
148	Can view aws tags summary	37	view_awstagssummary
149	Can add aws tags values	38	add_awstagsvalues
150	Can change aws tags values	38	change_awstagsvalues
151	Can delete aws tags values	38	delete_awstagsvalues
152	Can view aws tags values	38	view_awstagsvalues
153	Can add aws category summary	39	add_awscategorysummary
154	Can change aws category summary	39	change_awscategorysummary
155	Can delete aws category summary	39	delete_awscategorysummary
156	Can view aws category summary	39	view_awscategorysummary
157	Can add aws enabled category keys	40	add_awsenabledcategorykeys
158	Can change aws enabled category keys	40	change_awsenabledcategorykeys
159	Can delete aws enabled category keys	40	delete_awsenabledcategorykeys
160	Can view aws enabled category keys	40	view_awsenabledcategorykeys
161	Can add azure compute summary p	41	add_azurecomputesummaryp
162	Can change azure compute summary p	41	change_azurecomputesummaryp
163	Can delete azure compute summary p	41	delete_azurecomputesummaryp
164	Can view azure compute summary p	41	view_azurecomputesummaryp
165	Can add azure cost entry bill	42	add_azurecostentrybill
166	Can change azure cost entry bill	42	change_azurecostentrybill
167	Can delete azure cost entry bill	42	delete_azurecostentrybill
168	Can view azure cost entry bill	42	view_azurecostentrybill
169	Can add azure cost entry line item daily summary	43	add_azurecostentrylineitemdailysummary
170	Can change azure cost entry line item daily summary	43	change_azurecostentrylineitemdailysummary
171	Can delete azure cost entry line item daily summary	43	delete_azurecostentrylineitemdailysummary
172	Can view azure cost entry line item daily summary	43	view_azurecostentrylineitemdailysummary
173	Can add azure cost summary by account p	44	add_azurecostsummarybyaccountp
174	Can change azure cost summary by account p	44	change_azurecostsummarybyaccountp
175	Can delete azure cost summary by account p	44	delete_azurecostsummarybyaccountp
176	Can view azure cost summary by account p	44	view_azurecostsummarybyaccountp
177	Can add azure cost summary by location p	45	add_azurecostsummarybylocationp
178	Can change azure cost summary by location p	45	change_azurecostsummarybylocationp
179	Can delete azure cost summary by location p	45	delete_azurecostsummarybylocationp
180	Can view azure cost summary by location p	45	view_azurecostsummarybylocationp
181	Can add azure cost summary by service p	46	add_azurecostsummarybyservicep
182	Can change azure cost summary by service p	46	change_azurecostsummarybyservicep
183	Can delete azure cost summary by service p	46	delete_azurecostsummarybyservicep
184	Can view azure cost summary by service p	46	view_azurecostsummarybyservicep
185	Can add azure cost summary p	47	add_azurecostsummaryp
186	Can change azure cost summary p	47	change_azurecostsummaryp
187	Can delete azure cost summary p	47	delete_azurecostsummaryp
188	Can view azure cost summary p	47	view_azurecostsummaryp
189	Can add azure database summary p	48	add_azuredatabasesummaryp
190	Can change azure database summary p	48	change_azuredatabasesummaryp
191	Can delete azure database summary p	48	delete_azuredatabasesummaryp
192	Can view azure database summary p	48	view_azuredatabasesummaryp
193	Can add azure network summary p	49	add_azurenetworksummaryp
194	Can change azure network summary p	49	change_azurenetworksummaryp
195	Can delete azure network summary p	49	delete_azurenetworksummaryp
196	Can view azure network summary p	49	view_azurenetworksummaryp
197	Can add azure storage summary p	50	add_azurestoragesummaryp
198	Can change azure storage summary p	50	change_azurestoragesummaryp
199	Can delete azure storage summary p	50	delete_azurestoragesummaryp
200	Can view azure storage summary p	50	view_azurestoragesummaryp
201	Can add azure tags summary	51	add_azuretagssummary
202	Can change azure tags summary	51	change_azuretagssummary
203	Can delete azure tags summary	51	delete_azuretagssummary
204	Can view azure tags summary	51	view_azuretagssummary
205	Can add azure tags values	52	add_azuretagsvalues
206	Can change azure tags values	52	change_azuretagsvalues
207	Can delete azure tags values	52	delete_azuretagsvalues
208	Can view azure tags values	52	view_azuretagsvalues
209	Can add ocp usage report period	53	add_ocpusagereportperiod
210	Can change ocp usage report period	53	change_ocpusagereportperiod
211	Can delete ocp usage report period	53	delete_ocpusagereportperiod
212	Can view ocp usage report period	53	view_ocpusagereportperiod
213	Can add cost summary	54	add_costsummary
214	Can change cost summary	54	change_costsummary
215	Can delete cost summary	54	delete_costsummary
216	Can view cost summary	54	view_costsummary
217	Can add currency settings	55	add_currencysettings
218	Can change currency settings	55	change_currencysettings
219	Can delete currency settings	55	delete_currencysettings
220	Can view currency settings	55	view_currencysettings
221	Can add gcp compute summary by account p	56	add_gcpcomputesummarybyaccountp
222	Can change gcp compute summary by account p	56	change_gcpcomputesummarybyaccountp
223	Can delete gcp compute summary by account p	56	delete_gcpcomputesummarybyaccountp
224	Can view gcp compute summary by account p	56	view_gcpcomputesummarybyaccountp
225	Can add gcp compute summary p	57	add_gcpcomputesummaryp
226	Can change gcp compute summary p	57	change_gcpcomputesummaryp
227	Can delete gcp compute summary p	57	delete_gcpcomputesummaryp
228	Can view gcp compute summary p	57	view_gcpcomputesummaryp
229	Can add gcp cost entry bill	58	add_gcpcostentrybill
230	Can change gcp cost entry bill	58	change_gcpcostentrybill
231	Can delete gcp cost entry bill	58	delete_gcpcostentrybill
232	Can view gcp cost entry bill	58	view_gcpcostentrybill
233	Can add gcp cost entry line item daily summary	59	add_gcpcostentrylineitemdailysummary
234	Can change gcp cost entry line item daily summary	59	change_gcpcostentrylineitemdailysummary
235	Can delete gcp cost entry line item daily summary	59	delete_gcpcostentrylineitemdailysummary
236	Can view gcp cost entry line item daily summary	59	view_gcpcostentrylineitemdailysummary
237	Can add gcp cost summary by account p	60	add_gcpcostsummarybyaccountp
238	Can change gcp cost summary by account p	60	change_gcpcostsummarybyaccountp
239	Can delete gcp cost summary by account p	60	delete_gcpcostsummarybyaccountp
240	Can view gcp cost summary by account p	60	view_gcpcostsummarybyaccountp
241	Can add gcp cost summary by project p	61	add_gcpcostsummarybyprojectp
242	Can change gcp cost summary by project p	61	change_gcpcostsummarybyprojectp
243	Can delete gcp cost summary by project p	61	delete_gcpcostsummarybyprojectp
244	Can view gcp cost summary by project p	61	view_gcpcostsummarybyprojectp
245	Can add gcp cost summary by region p	62	add_gcpcostsummarybyregionp
246	Can change gcp cost summary by region p	62	change_gcpcostsummarybyregionp
247	Can delete gcp cost summary by region p	62	delete_gcpcostsummarybyregionp
248	Can view gcp cost summary by region p	62	view_gcpcostsummarybyregionp
249	Can add gcp cost summary by service p	63	add_gcpcostsummarybyservicep
250	Can change gcp cost summary by service p	63	change_gcpcostsummarybyservicep
251	Can delete gcp cost summary by service p	63	delete_gcpcostsummarybyservicep
252	Can view gcp cost summary by service p	63	view_gcpcostsummarybyservicep
253	Can add gcp cost summary p	64	add_gcpcostsummaryp
254	Can change gcp cost summary p	64	change_gcpcostsummaryp
255	Can delete gcp cost summary p	64	delete_gcpcostsummaryp
256	Can view gcp cost summary p	64	view_gcpcostsummaryp
257	Can add gcp database summary p	65	add_gcpdatabasesummaryp
258	Can change gcp database summary p	65	change_gcpdatabasesummaryp
259	Can delete gcp database summary p	65	delete_gcpdatabasesummaryp
260	Can view gcp database summary p	65	view_gcpdatabasesummaryp
261	Can add gcp network summary p	66	add_gcpnetworksummaryp
262	Can change gcp network summary p	66	change_gcpnetworksummaryp
263	Can delete gcp network summary p	66	delete_gcpnetworksummaryp
264	Can view gcp network summary p	66	view_gcpnetworksummaryp
265	Can add gcp storage summary by account p	67	add_gcpstoragesummarybyaccountp
266	Can change gcp storage summary by account p	67	change_gcpstoragesummarybyaccountp
267	Can delete gcp storage summary by account p	67	delete_gcpstoragesummarybyaccountp
268	Can view gcp storage summary by account p	67	view_gcpstoragesummarybyaccountp
269	Can add gcp storage summary by project p	68	add_gcpstoragesummarybyprojectp
270	Can change gcp storage summary by project p	68	change_gcpstoragesummarybyprojectp
271	Can delete gcp storage summary by project p	68	delete_gcpstoragesummarybyprojectp
272	Can view gcp storage summary by project p	68	view_gcpstoragesummarybyprojectp
273	Can add gcp storage summary by region p	69	add_gcpstoragesummarybyregionp
274	Can change gcp storage summary by region p	69	change_gcpstoragesummarybyregionp
275	Can delete gcp storage summary by region p	69	delete_gcpstoragesummarybyregionp
276	Can view gcp storage summary by region p	69	view_gcpstoragesummarybyregionp
277	Can add gcp storage summary by service p	70	add_gcpstoragesummarybyservicep
278	Can change gcp storage summary by service p	70	change_gcpstoragesummarybyservicep
279	Can delete gcp storage summary by service p	70	delete_gcpstoragesummarybyservicep
280	Can view gcp storage summary by service p	70	view_gcpstoragesummarybyservicep
281	Can add gcp storage summary p	71	add_gcpstoragesummaryp
282	Can change gcp storage summary p	71	change_gcpstoragesummaryp
283	Can delete gcp storage summary p	71	delete_gcpstoragesummaryp
284	Can view gcp storage summary p	71	view_gcpstoragesummaryp
285	Can add gcp tags summary	72	add_gcptagssummary
286	Can change gcp tags summary	72	change_gcptagssummary
287	Can delete gcp tags summary	72	delete_gcptagssummary
288	Can view gcp tags summary	72	view_gcptagssummary
289	Can add gcp tags values	73	add_gcptagsvalues
290	Can change gcp tags values	73	change_gcptagsvalues
291	Can delete gcp tags values	73	delete_gcptagsvalues
292	Can view gcp tags values	73	view_gcptagsvalues
293	Can add gcp topology	74	add_gcptopology
294	Can change gcp topology	74	change_gcptopology
295	Can delete gcp topology	74	delete_gcptopology
296	Can view gcp topology	74	view_gcptopology
297	Can add ocp all compute summary pt	75	add_ocpallcomputesummarypt
298	Can change ocp all compute summary pt	75	change_ocpallcomputesummarypt
299	Can delete ocp all compute summary pt	75	delete_ocpallcomputesummarypt
300	Can view ocp all compute summary pt	75	view_ocpallcomputesummarypt
301	Can add ocp all cost line item daily summary p	76	add_ocpallcostlineitemdailysummaryp
302	Can change ocp all cost line item daily summary p	76	change_ocpallcostlineitemdailysummaryp
303	Can delete ocp all cost line item daily summary p	76	delete_ocpallcostlineitemdailysummaryp
304	Can view ocp all cost line item daily summary p	76	view_ocpallcostlineitemdailysummaryp
305	Can add ocp all cost line item project daily summary p	77	add_ocpallcostlineitemprojectdailysummaryp
306	Can change ocp all cost line item project daily summary p	77	change_ocpallcostlineitemprojectdailysummaryp
307	Can delete ocp all cost line item project daily summary p	77	delete_ocpallcostlineitemprojectdailysummaryp
308	Can view ocp all cost line item project daily summary p	77	view_ocpallcostlineitemprojectdailysummaryp
309	Can add ocp all cost summary by account pt	78	add_ocpallcostsummarybyaccountpt
310	Can change ocp all cost summary by account pt	78	change_ocpallcostsummarybyaccountpt
311	Can delete ocp all cost summary by account pt	78	delete_ocpallcostsummarybyaccountpt
312	Can view ocp all cost summary by account pt	78	view_ocpallcostsummarybyaccountpt
313	Can add ocp all cost summary by region pt	79	add_ocpallcostsummarybyregionpt
314	Can change ocp all cost summary by region pt	79	change_ocpallcostsummarybyregionpt
315	Can delete ocp all cost summary by region pt	79	delete_ocpallcostsummarybyregionpt
316	Can view ocp all cost summary by region pt	79	view_ocpallcostsummarybyregionpt
317	Can add ocp all cost summary by service pt	80	add_ocpallcostsummarybyservicept
318	Can change ocp all cost summary by service pt	80	change_ocpallcostsummarybyservicept
319	Can delete ocp all cost summary by service pt	80	delete_ocpallcostsummarybyservicept
320	Can view ocp all cost summary by service pt	80	view_ocpallcostsummarybyservicept
321	Can add ocp all cost summary pt	81	add_ocpallcostsummarypt
322	Can change ocp all cost summary pt	81	change_ocpallcostsummarypt
323	Can delete ocp all cost summary pt	81	delete_ocpallcostsummarypt
324	Can view ocp all cost summary pt	81	view_ocpallcostsummarypt
325	Can add ocp all database summary pt	82	add_ocpalldatabasesummarypt
326	Can change ocp all database summary pt	82	change_ocpalldatabasesummarypt
327	Can delete ocp all database summary pt	82	delete_ocpalldatabasesummarypt
328	Can view ocp all database summary pt	82	view_ocpalldatabasesummarypt
329	Can add ocp all network summary pt	83	add_ocpallnetworksummarypt
330	Can change ocp all network summary pt	83	change_ocpallnetworksummarypt
331	Can delete ocp all network summary pt	83	delete_ocpallnetworksummarypt
332	Can view ocp all network summary pt	83	view_ocpallnetworksummarypt
333	Can add ocp all storage summary pt	84	add_ocpallstoragesummarypt
334	Can change ocp all storage summary pt	84	change_ocpallstoragesummarypt
335	Can delete ocp all storage summary pt	84	delete_ocpallstoragesummarypt
336	Can view ocp all storage summary pt	84	view_ocpallstoragesummarypt
337	Can add ocpaws compute summary p	85	add_ocpawscomputesummaryp
338	Can change ocpaws compute summary p	85	change_ocpawscomputesummaryp
339	Can delete ocpaws compute summary p	85	delete_ocpawscomputesummaryp
340	Can view ocpaws compute summary p	85	view_ocpawscomputesummaryp
341	Can add ocpaws cost line item daily summary p	86	add_ocpawscostlineitemdailysummaryp
342	Can change ocpaws cost line item daily summary p	86	change_ocpawscostlineitemdailysummaryp
343	Can delete ocpaws cost line item daily summary p	86	delete_ocpawscostlineitemdailysummaryp
344	Can view ocpaws cost line item daily summary p	86	view_ocpawscostlineitemdailysummaryp
345	Can add ocpaws cost line item project daily summary p	87	add_ocpawscostlineitemprojectdailysummaryp
346	Can change ocpaws cost line item project daily summary p	87	change_ocpawscostlineitemprojectdailysummaryp
347	Can delete ocpaws cost line item project daily summary p	87	delete_ocpawscostlineitemprojectdailysummaryp
348	Can view ocpaws cost line item project daily summary p	87	view_ocpawscostlineitemprojectdailysummaryp
349	Can add ocpaws cost summary by account p	88	add_ocpawscostsummarybyaccountp
350	Can change ocpaws cost summary by account p	88	change_ocpawscostsummarybyaccountp
351	Can delete ocpaws cost summary by account p	88	delete_ocpawscostsummarybyaccountp
352	Can view ocpaws cost summary by account p	88	view_ocpawscostsummarybyaccountp
353	Can add ocpaws cost summary by region p	89	add_ocpawscostsummarybyregionp
354	Can change ocpaws cost summary by region p	89	change_ocpawscostsummarybyregionp
355	Can delete ocpaws cost summary by region p	89	delete_ocpawscostsummarybyregionp
356	Can view ocpaws cost summary by region p	89	view_ocpawscostsummarybyregionp
357	Can add ocpaws cost summary by service p	90	add_ocpawscostsummarybyservicep
358	Can change ocpaws cost summary by service p	90	change_ocpawscostsummarybyservicep
359	Can delete ocpaws cost summary by service p	90	delete_ocpawscostsummarybyservicep
360	Can view ocpaws cost summary by service p	90	view_ocpawscostsummarybyservicep
361	Can add ocpaws cost summary p	91	add_ocpawscostsummaryp
362	Can change ocpaws cost summary p	91	change_ocpawscostsummaryp
363	Can delete ocpaws cost summary p	91	delete_ocpawscostsummaryp
364	Can view ocpaws cost summary p	91	view_ocpawscostsummaryp
365	Can add ocpaws database summary p	92	add_ocpawsdatabasesummaryp
366	Can change ocpaws database summary p	92	change_ocpawsdatabasesummaryp
367	Can delete ocpaws database summary p	92	delete_ocpawsdatabasesummaryp
368	Can view ocpaws database summary p	92	view_ocpawsdatabasesummaryp
369	Can add ocpaws network summary p	93	add_ocpawsnetworksummaryp
370	Can change ocpaws network summary p	93	change_ocpawsnetworksummaryp
371	Can delete ocpaws network summary p	93	delete_ocpawsnetworksummaryp
372	Can view ocpaws network summary p	93	view_ocpawsnetworksummaryp
373	Can add ocpaws storage summary p	94	add_ocpawsstoragesummaryp
374	Can change ocpaws storage summary p	94	change_ocpawsstoragesummaryp
375	Can delete ocpaws storage summary p	94	delete_ocpawsstoragesummaryp
376	Can view ocpaws storage summary p	94	view_ocpawsstoragesummaryp
377	Can add ocpaws tags summary	95	add_ocpawstagssummary
378	Can change ocpaws tags summary	95	change_ocpawstagssummary
379	Can delete ocpaws tags summary	95	delete_ocpawstagssummary
380	Can view ocpaws tags summary	95	view_ocpawstagssummary
381	Can add ocpaws tags values	96	add_ocpawstagsvalues
382	Can change ocpaws tags values	96	change_ocpawstagsvalues
383	Can delete ocpaws tags values	96	delete_ocpawstagsvalues
384	Can view ocpaws tags values	96	view_ocpawstagsvalues
385	Can add ocp azure compute summary p	97	add_ocpazurecomputesummaryp
386	Can change ocp azure compute summary p	97	change_ocpazurecomputesummaryp
387	Can delete ocp azure compute summary p	97	delete_ocpazurecomputesummaryp
388	Can view ocp azure compute summary p	97	view_ocpazurecomputesummaryp
389	Can add ocp azure cost line item daily summary p	98	add_ocpazurecostlineitemdailysummaryp
390	Can change ocp azure cost line item daily summary p	98	change_ocpazurecostlineitemdailysummaryp
391	Can delete ocp azure cost line item daily summary p	98	delete_ocpazurecostlineitemdailysummaryp
392	Can view ocp azure cost line item daily summary p	98	view_ocpazurecostlineitemdailysummaryp
393	Can add ocp azure cost line item project daily summary p	99	add_ocpazurecostlineitemprojectdailysummaryp
394	Can change ocp azure cost line item project daily summary p	99	change_ocpazurecostlineitemprojectdailysummaryp
395	Can delete ocp azure cost line item project daily summary p	99	delete_ocpazurecostlineitemprojectdailysummaryp
469	Can add ocpgcp cost summary by region p	118	add_ocpgcpcostsummarybyregionp
396	Can view ocp azure cost line item project daily summary p	99	view_ocpazurecostlineitemprojectdailysummaryp
397	Can add ocp azure cost summary by account p	100	add_ocpazurecostsummarybyaccountp
398	Can change ocp azure cost summary by account p	100	change_ocpazurecostsummarybyaccountp
399	Can delete ocp azure cost summary by account p	100	delete_ocpazurecostsummarybyaccountp
400	Can view ocp azure cost summary by account p	100	view_ocpazurecostsummarybyaccountp
401	Can add ocp azure cost summary by location p	101	add_ocpazurecostsummarybylocationp
402	Can change ocp azure cost summary by location p	101	change_ocpazurecostsummarybylocationp
403	Can delete ocp azure cost summary by location p	101	delete_ocpazurecostsummarybylocationp
404	Can view ocp azure cost summary by location p	101	view_ocpazurecostsummarybylocationp
405	Can add ocp azure cost summary by service p	102	add_ocpazurecostsummarybyservicep
406	Can change ocp azure cost summary by service p	102	change_ocpazurecostsummarybyservicep
407	Can delete ocp azure cost summary by service p	102	delete_ocpazurecostsummarybyservicep
408	Can view ocp azure cost summary by service p	102	view_ocpazurecostsummarybyservicep
409	Can add ocp azure cost summary p	103	add_ocpazurecostsummaryp
410	Can change ocp azure cost summary p	103	change_ocpazurecostsummaryp
411	Can delete ocp azure cost summary p	103	delete_ocpazurecostsummaryp
412	Can view ocp azure cost summary p	103	view_ocpazurecostsummaryp
413	Can add ocp azure database summary p	104	add_ocpazuredatabasesummaryp
414	Can change ocp azure database summary p	104	change_ocpazuredatabasesummaryp
415	Can delete ocp azure database summary p	104	delete_ocpazuredatabasesummaryp
416	Can view ocp azure database summary p	104	view_ocpazuredatabasesummaryp
417	Can add ocp azure network summary p	105	add_ocpazurenetworksummaryp
418	Can change ocp azure network summary p	105	change_ocpazurenetworksummaryp
419	Can delete ocp azure network summary p	105	delete_ocpazurenetworksummaryp
420	Can view ocp azure network summary p	105	view_ocpazurenetworksummaryp
421	Can add ocp azure storage summary p	106	add_ocpazurestoragesummaryp
422	Can change ocp azure storage summary p	106	change_ocpazurestoragesummaryp
423	Can delete ocp azure storage summary p	106	delete_ocpazurestoragesummaryp
424	Can view ocp azure storage summary p	106	view_ocpazurestoragesummaryp
425	Can add ocp azure tags summary	107	add_ocpazuretagssummary
426	Can change ocp azure tags summary	107	change_ocpazuretagssummary
427	Can delete ocp azure tags summary	107	delete_ocpazuretagssummary
428	Can view ocp azure tags summary	107	view_ocpazuretagssummary
429	Can add ocp azure tags values	108	add_ocpazuretagsvalues
430	Can change ocp azure tags values	108	change_ocpazuretagsvalues
431	Can delete ocp azure tags values	108	delete_ocpazuretagsvalues
432	Can view ocp azure tags values	108	view_ocpazuretagsvalues
433	Can add ocp cluster	109	add_ocpcluster
434	Can change ocp cluster	109	change_ocpcluster
435	Can delete ocp cluster	109	delete_ocpcluster
436	Can view ocp cluster	109	view_ocpcluster
437	Can add ocp cost summary by node p	110	add_ocpcostsummarybynodep
438	Can change ocp cost summary by node p	110	change_ocpcostsummarybynodep
439	Can delete ocp cost summary by node p	110	delete_ocpcostsummarybynodep
440	Can view ocp cost summary by node p	110	view_ocpcostsummarybynodep
441	Can add ocp cost summary by project p	111	add_ocpcostsummarybyprojectp
442	Can change ocp cost summary by project p	111	change_ocpcostsummarybyprojectp
443	Can delete ocp cost summary by project p	111	delete_ocpcostsummarybyprojectp
444	Can view ocp cost summary by project p	111	view_ocpcostsummarybyprojectp
445	Can add ocp cost summary p	112	add_ocpcostsummaryp
446	Can change ocp cost summary p	112	change_ocpcostsummaryp
447	Can delete ocp cost summary p	112	delete_ocpcostsummaryp
448	Can view ocp cost summary p	112	view_ocpcostsummaryp
449	Can add ocpgcp compute summary p	113	add_ocpgcpcomputesummaryp
450	Can change ocpgcp compute summary p	113	change_ocpgcpcomputesummaryp
451	Can delete ocpgcp compute summary p	113	delete_ocpgcpcomputesummaryp
452	Can view ocpgcp compute summary p	113	view_ocpgcpcomputesummaryp
453	Can add ocpgcp cost line item daily summary p	114	add_ocpgcpcostlineitemdailysummaryp
454	Can change ocpgcp cost line item daily summary p	114	change_ocpgcpcostlineitemdailysummaryp
455	Can delete ocpgcp cost line item daily summary p	114	delete_ocpgcpcostlineitemdailysummaryp
456	Can view ocpgcp cost line item daily summary p	114	view_ocpgcpcostlineitemdailysummaryp
457	Can add ocpgcp cost line item project daily summary p	115	add_ocpgcpcostlineitemprojectdailysummaryp
458	Can change ocpgcp cost line item project daily summary p	115	change_ocpgcpcostlineitemprojectdailysummaryp
459	Can delete ocpgcp cost line item project daily summary p	115	delete_ocpgcpcostlineitemprojectdailysummaryp
460	Can view ocpgcp cost line item project daily summary p	115	view_ocpgcpcostlineitemprojectdailysummaryp
461	Can add ocpgcp cost summary by account p	116	add_ocpgcpcostsummarybyaccountp
462	Can change ocpgcp cost summary by account p	116	change_ocpgcpcostsummarybyaccountp
463	Can delete ocpgcp cost summary by account p	116	delete_ocpgcpcostsummarybyaccountp
464	Can view ocpgcp cost summary by account p	116	view_ocpgcpcostsummarybyaccountp
465	Can add ocpgcp cost summary by gcp project p	117	add_ocpgcpcostsummarybygcpprojectp
466	Can change ocpgcp cost summary by gcp project p	117	change_ocpgcpcostsummarybygcpprojectp
467	Can delete ocpgcp cost summary by gcp project p	117	delete_ocpgcpcostsummarybygcpprojectp
468	Can view ocpgcp cost summary by gcp project p	117	view_ocpgcpcostsummarybygcpprojectp
470	Can change ocpgcp cost summary by region p	118	change_ocpgcpcostsummarybyregionp
471	Can delete ocpgcp cost summary by region p	118	delete_ocpgcpcostsummarybyregionp
472	Can view ocpgcp cost summary by region p	118	view_ocpgcpcostsummarybyregionp
473	Can add ocpgcp cost summary by service p	119	add_ocpgcpcostsummarybyservicep
474	Can change ocpgcp cost summary by service p	119	change_ocpgcpcostsummarybyservicep
475	Can delete ocpgcp cost summary by service p	119	delete_ocpgcpcostsummarybyservicep
476	Can view ocpgcp cost summary by service p	119	view_ocpgcpcostsummarybyservicep
477	Can add ocpgcp cost summary p	120	add_ocpgcpcostsummaryp
478	Can change ocpgcp cost summary p	120	change_ocpgcpcostsummaryp
479	Can delete ocpgcp cost summary p	120	delete_ocpgcpcostsummaryp
480	Can view ocpgcp cost summary p	120	view_ocpgcpcostsummaryp
481	Can add ocpgcp database summary p	121	add_ocpgcpdatabasesummaryp
482	Can change ocpgcp database summary p	121	change_ocpgcpdatabasesummaryp
483	Can delete ocpgcp database summary p	121	delete_ocpgcpdatabasesummaryp
484	Can view ocpgcp database summary p	121	view_ocpgcpdatabasesummaryp
485	Can add ocpgcp network summary p	122	add_ocpgcpnetworksummaryp
486	Can change ocpgcp network summary p	122	change_ocpgcpnetworksummaryp
487	Can delete ocpgcp network summary p	122	delete_ocpgcpnetworksummaryp
488	Can view ocpgcp network summary p	122	view_ocpgcpnetworksummaryp
489	Can add ocpgcp storage summary p	123	add_ocpgcpstoragesummaryp
490	Can change ocpgcp storage summary p	123	change_ocpgcpstoragesummaryp
491	Can delete ocpgcp storage summary p	123	delete_ocpgcpstoragesummaryp
492	Can view ocpgcp storage summary p	123	view_ocpgcpstoragesummaryp
493	Can add ocpgcp tags summary	124	add_ocpgcptagssummary
494	Can change ocpgcp tags summary	124	change_ocpgcptagssummary
495	Can delete ocpgcp tags summary	124	delete_ocpgcptagssummary
496	Can view ocpgcp tags summary	124	view_ocpgcptagssummary
497	Can add ocpgcp tags values	125	add_ocpgcptagsvalues
498	Can change ocpgcp tags values	125	change_ocpgcptagsvalues
499	Can delete ocpgcp tags values	125	delete_ocpgcptagsvalues
500	Can view ocpgcp tags values	125	view_ocpgcptagsvalues
501	Can add ocp node	126	add_ocpnode
502	Can change ocp node	126	change_ocpnode
503	Can delete ocp node	126	delete_ocpnode
504	Can view ocp node	126	view_ocpnode
505	Can add ocp pod summary by project p	127	add_ocppodsummarybyprojectp
506	Can change ocp pod summary by project p	127	change_ocppodsummarybyprojectp
507	Can delete ocp pod summary by project p	127	delete_ocppodsummarybyprojectp
508	Can view ocp pod summary by project p	127	view_ocppodsummarybyprojectp
509	Can add ocp pod summary p	128	add_ocppodsummaryp
510	Can change ocp pod summary p	128	change_ocppodsummaryp
511	Can delete ocp pod summary p	128	delete_ocppodsummaryp
512	Can view ocp pod summary p	128	view_ocppodsummaryp
513	Can add ocp project	129	add_ocpproject
514	Can change ocp project	129	change_ocpproject
515	Can delete ocp project	129	delete_ocpproject
516	Can view ocp project	129	view_ocpproject
517	Can add ocppvc	130	add_ocppvc
518	Can change ocppvc	130	change_ocppvc
519	Can delete ocppvc	130	delete_ocppvc
520	Can view ocppvc	130	view_ocppvc
521	Can add ocp storage volume label summary	131	add_ocpstoragevolumelabelsummary
522	Can change ocp storage volume label summary	131	change_ocpstoragevolumelabelsummary
523	Can delete ocp storage volume label summary	131	delete_ocpstoragevolumelabelsummary
524	Can view ocp storage volume label summary	131	view_ocpstoragevolumelabelsummary
525	Can add ocp tags values	132	add_ocptagsvalues
526	Can change ocp tags values	132	change_ocptagsvalues
527	Can delete ocp tags values	132	delete_ocptagsvalues
528	Can view ocp tags values	132	view_ocptagsvalues
529	Can add ocp usage line item daily summary	133	add_ocpusagelineitemdailysummary
530	Can change ocp usage line item daily summary	133	change_ocpusagelineitemdailysummary
531	Can delete ocp usage line item daily summary	133	delete_ocpusagelineitemdailysummary
532	Can view ocp usage line item daily summary	133	view_ocpusagelineitemdailysummary
533	Can add ocp usage pod label summary	134	add_ocpusagepodlabelsummary
534	Can change ocp usage pod label summary	134	change_ocpusagepodlabelsummary
535	Can delete ocp usage pod label summary	134	delete_ocpusagepodlabelsummary
536	Can view ocp usage pod label summary	134	view_ocpusagepodlabelsummary
537	Can add ocp volume summary by project p	135	add_ocpvolumesummarybyprojectp
538	Can change ocp volume summary by project p	135	change_ocpvolumesummarybyprojectp
539	Can delete ocp volume summary by project p	135	delete_ocpvolumesummarybyprojectp
540	Can view ocp volume summary by project p	135	view_ocpvolumesummarybyprojectp
541	Can add ocp volume summary p	136	add_ocpvolumesummaryp
542	Can change ocp volume summary p	136	change_ocpvolumesummaryp
543	Can delete ocp volume summary p	136	delete_ocpvolumesummaryp
544	Can view ocp volume summary p	136	view_ocpvolumesummaryp
545	Can add oci cost entry bill	137	add_ocicostentrybill
546	Can change oci cost entry bill	137	change_ocicostentrybill
547	Can delete oci cost entry bill	137	delete_ocicostentrybill
548	Can view oci cost entry bill	137	view_ocicostentrybill
549	Can add oci compute summary by account p	138	add_ocicomputesummarybyaccountp
550	Can change oci compute summary by account p	138	change_ocicomputesummarybyaccountp
551	Can delete oci compute summary by account p	138	delete_ocicomputesummarybyaccountp
552	Can view oci compute summary by account p	138	view_ocicomputesummarybyaccountp
553	Can add oci compute summary p	139	add_ocicomputesummaryp
554	Can change oci compute summary p	139	change_ocicomputesummaryp
555	Can delete oci compute summary p	139	delete_ocicomputesummaryp
556	Can view oci compute summary p	139	view_ocicomputesummaryp
557	Can add oci cost entry line item daily summary	140	add_ocicostentrylineitemdailysummary
558	Can change oci cost entry line item daily summary	140	change_ocicostentrylineitemdailysummary
559	Can delete oci cost entry line item daily summary	140	delete_ocicostentrylineitemdailysummary
560	Can view oci cost entry line item daily summary	140	view_ocicostentrylineitemdailysummary
561	Can add oci cost summary by account p	141	add_ocicostsummarybyaccountp
562	Can change oci cost summary by account p	141	change_ocicostsummarybyaccountp
563	Can delete oci cost summary by account p	141	delete_ocicostsummarybyaccountp
564	Can view oci cost summary by account p	141	view_ocicostsummarybyaccountp
565	Can add oci cost summary by region p	142	add_ocicostsummarybyregionp
566	Can change oci cost summary by region p	142	change_ocicostsummarybyregionp
567	Can delete oci cost summary by region p	142	delete_ocicostsummarybyregionp
568	Can view oci cost summary by region p	142	view_ocicostsummarybyregionp
569	Can add oci cost summary by service p	143	add_ocicostsummarybyservicep
570	Can change oci cost summary by service p	143	change_ocicostsummarybyservicep
571	Can delete oci cost summary by service p	143	delete_ocicostsummarybyservicep
572	Can view oci cost summary by service p	143	view_ocicostsummarybyservicep
573	Can add oci cost summary p	144	add_ocicostsummaryp
574	Can change oci cost summary p	144	change_ocicostsummaryp
575	Can delete oci cost summary p	144	delete_ocicostsummaryp
576	Can view oci cost summary p	144	view_ocicostsummaryp
577	Can add oci database summary p	145	add_ocidatabasesummaryp
578	Can change oci database summary p	145	change_ocidatabasesummaryp
579	Can delete oci database summary p	145	delete_ocidatabasesummaryp
580	Can view oci database summary p	145	view_ocidatabasesummaryp
581	Can add oci network summary p	146	add_ocinetworksummaryp
582	Can change oci network summary p	146	change_ocinetworksummaryp
583	Can delete oci network summary p	146	delete_ocinetworksummaryp
584	Can view oci network summary p	146	view_ocinetworksummaryp
585	Can add oci storage summary by account p	147	add_ocistoragesummarybyaccountp
586	Can change oci storage summary by account p	147	change_ocistoragesummarybyaccountp
587	Can delete oci storage summary by account p	147	delete_ocistoragesummarybyaccountp
588	Can view oci storage summary by account p	147	view_ocistoragesummarybyaccountp
589	Can add oci storage summary p	148	add_ocistoragesummaryp
590	Can change oci storage summary p	148	change_ocistoragesummaryp
591	Can delete oci storage summary p	148	delete_ocistoragesummaryp
592	Can view oci storage summary p	148	view_ocistoragesummaryp
593	Can add oci tags summary	149	add_ocitagssummary
594	Can change oci tags summary	149	change_ocitagssummary
595	Can delete oci tags summary	149	delete_ocitagssummary
596	Can view oci tags summary	149	view_ocitagssummary
597	Can add oci tags values	150	add_ocitagsvalues
598	Can change oci tags values	150	change_ocitagsvalues
599	Can delete oci tags values	150	delete_ocitagsvalues
600	Can view oci tags values	150	view_ocitagsvalues
601	Can add partitioned table	151	add_partitionedtable
602	Can change partitioned table	151	change_partitionedtable
603	Can delete partitioned table	151	delete_partitionedtable
604	Can view partitioned table	151	view_partitionedtable
605	Can add user settings	152	add_usersettings
606	Can change user settings	152	change_usersettings
607	Can delete user settings	152	delete_usersettings
608	Can view user settings	152	view_usersettings
609	Can add enabled tag keys	153	add_enabledtagkeys
610	Can change enabled tag keys	153	change_enabledtagkeys
611	Can delete enabled tag keys	153	delete_enabledtagkeys
612	Can view enabled tag keys	153	view_enabledtagkeys
613	Can add subs last processed	154	add_subslastprocessed
614	Can change subs last processed	154	change_subslastprocessed
615	Can delete subs last processed	154	delete_subslastprocessed
616	Can view subs last processed	154	view_subslastprocessed
617	Can add subs id map	155	add_subsidmap
618	Can change subs id map	155	change_subsidmap
619	Can delete subs id map	155	delete_subsidmap
620	Can view subs id map	155	view_subsidmap
621	Can add ocp pod summary by node p	156	add_ocppodsummarybynodep
622	Can change ocp pod summary by node p	156	change_ocppodsummarybynodep
623	Can delete ocp pod summary by node p	156	delete_ocppodsummarybynodep
624	Can view ocp pod summary by node p	156	view_ocppodsummarybynodep
625	Can add openshift cost category namespace	157	add_openshiftcostcategorynamespace
626	Can change openshift cost category namespace	157	change_openshiftcostcategorynamespace
627	Can delete openshift cost category namespace	157	delete_openshiftcostcategorynamespace
628	Can view openshift cost category namespace	157	view_openshiftcostcategorynamespace
629	Can add tag mapping	158	add_tagmapping
630	Can change tag mapping	158	change_tagmapping
631	Can delete tag mapping	158	delete_tagmapping
632	Can view tag mapping	158	view_tagmapping
633	Can add ocp network summary p	159	add_ocpnetworksummaryp
634	Can change ocp network summary p	159	change_ocpnetworksummaryp
635	Can delete ocp network summary p	159	delete_ocpnetworksummaryp
636	Can view ocp network summary p	159	view_ocpnetworksummaryp
637	Can add ocp network summary by project p	160	add_ocpnetworksummarybyprojectp
638	Can change ocp network summary by project p	160	change_ocpnetworksummarybyprojectp
639	Can delete ocp network summary by project p	160	delete_ocpnetworksummarybyprojectp
640	Can view ocp network summary by project p	160	view_ocpnetworksummarybyprojectp
641	Can add ocp network summary by node p	161	add_ocpnetworksummarybynodep
642	Can change ocp network summary by node p	161	change_ocpnetworksummarybynodep
643	Can delete ocp network summary by node p	161	delete_ocpnetworksummarybynodep
644	Can view ocp network summary by node p	161	view_ocpnetworksummarybynodep
645	Can add aws cost entry line item summary by e c2 compute p	162	add_awscostentrylineitemsummarybyec2computep
646	Can change aws cost entry line item summary by e c2 compute p	162	change_awscostentrylineitemsummarybyec2computep
647	Can delete aws cost entry line item summary by e c2 compute p	162	delete_awscostentrylineitemsummarybyec2computep
648	Can view aws cost entry line item summary by e c2 compute p	162	view_awscostentrylineitemsummarybyec2computep
649	Can add ocp virtual machine summary p	163	add_ocpvirtualmachinesummaryp
650	Can change ocp virtual machine summary p	163	change_ocpvirtualmachinesummaryp
651	Can delete ocp virtual machine summary p	163	delete_ocpvirtualmachinesummaryp
652	Can view ocp virtual machine summary p	163	view_ocpvirtualmachinesummaryp
653	Can add cost usage report manifest	164	add_costusagereportmanifest
654	Can change cost usage report manifest	164	change_costusagereportmanifest
655	Can delete cost usage report manifest	164	delete_costusagereportmanifest
656	Can view cost usage report manifest	164	view_costusagereportmanifest
657	Can add region mapping	165	add_regionmapping
658	Can change region mapping	165	change_regionmapping
659	Can delete region mapping	165	delete_regionmapping
660	Can view region mapping	165	view_regionmapping
661	Can add cost usage report status	166	add_costusagereportstatus
662	Can change cost usage report status	166	change_costusagereportstatus
663	Can delete cost usage report status	166	delete_costusagereportstatus
664	Can view cost usage report status	166	view_costusagereportstatus
665	Can add delayed celery tasks	167	add_delayedcelerytasks
666	Can change delayed celery tasks	167	change_delayedcelerytasks
667	Can delete delayed celery tasks	167	delete_delayedcelerytasks
668	Can view delayed celery tasks	167	view_delayedcelerytasks
669	Can add disk capacity	168	add_diskcapacity
670	Can change disk capacity	168	change_diskcapacity
671	Can delete disk capacity	168	delete_diskcapacity
672	Can view disk capacity	168	view_diskcapacity
673	Can add cost model	169	add_costmodel
674	Can change cost model	169	change_costmodel
675	Can delete cost model	169	delete_costmodel
676	Can view cost model	169	view_costmodel
677	Can add cost model audit	170	add_costmodelaudit
678	Can change cost model audit	170	change_costmodelaudit
679	Can delete cost model audit	170	delete_costmodelaudit
680	Can view cost model audit	170	view_costmodelaudit
681	Can add cost model map	171	add_costmodelmap
682	Can change cost model map	171	change_costmodelmap
683	Can delete cost model map	171	delete_costmodelmap
684	Can view cost model map	171	view_costmodelmap
\.


--
-- Data for Name: auth_user; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_user (id, password, last_login, is_superuser, username, first_name, last_name, email, is_staff, is_active, date_joined) FROM stdin;
\.


--
-- Data for Name: auth_user_groups; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_user_groups (id, user_id, group_id) FROM stdin;
\.


--
-- Data for Name: auth_user_user_permissions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_user_user_permissions (id, user_id, permission_id) FROM stdin;
\.


--
-- Data for Name: aux_table; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.aux_table (mt_key1, mt_key2, mt_comment) FROM stdin;
\.


--
-- Data for Name: compaction_queue; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.compaction_queue (cq_id, cq_database, cq_table, cq_partition, cq_state, cq_type, cq_tblproperties, cq_worker_id, cq_start, cq_run_as, cq_highest_write_id, cq_meta_info, cq_hadoop_job_id) FROM stdin;
\.


--
-- Data for Name: completed_compactions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.completed_compactions (cc_id, cc_database, cc_table, cc_partition, cc_state, cc_type, cc_tblproperties, cc_worker_id, cc_start, cc_end, cc_run_as, cc_highest_write_id, cc_meta_info, cc_hadoop_job_id) FROM stdin;
\.


--
-- Data for Name: completed_txn_components; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.completed_txn_components (ctc_txnid, ctc_database, ctc_table, ctc_partition, ctc_timestamp, ctc_writeid, ctc_update_delete) FROM stdin;
\.


--
-- Data for Name: delayed_celery_tasks; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.delayed_celery_tasks (id, task_name, task_args, task_kwargs, timeout_timestamp, provider_uuid, queue_name, metadata) FROM stdin;
\.


--
-- Data for Name: django_content_type; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.django_content_type (id, app_label, model) FROM stdin;
1	auth	permission
2	auth	group
3	auth	user
4	contenttypes	contenttype
5	sessions	session
6	api	customer
7	api	provider
8	api	providerauthentication
9	api	providerbillingsource
10	api	sources
11	api	tenant
12	api	user
13	api	providerinfrastructuremap
14	api	exchangerates
15	api	exchangeratedictionary
16	api	domain
17	key_metrics	customertotaldata
18	reporting	tenantapiprovider
19	reporting	openshiftcostcategory
20	reporting	ingressreports
21	reporting	ocpallcostlineitemdailysummary
22	reporting	ocpallcostlineitemprojectdailysummary
23	reporting	awsaccountalias
24	reporting	awsorganizationalunit
25	reporting	awscomputesummarybyaccountp
26	reporting	awscomputesummaryp
27	reporting	awscostentrybill
28	reporting	awscostentrylineitemdailysummary
29	reporting	awscostsummarybyaccountp
30	reporting	awscostsummarybyregionp
31	reporting	awscostsummarybyservicep
32	reporting	awscostsummaryp
33	reporting	awsdatabasesummaryp
34	reporting	awsnetworksummaryp
35	reporting	awsstoragesummarybyaccountp
36	reporting	awsstoragesummaryp
37	reporting	awstagssummary
38	reporting	awstagsvalues
39	reporting	awscategorysummary
40	reporting	awsenabledcategorykeys
41	reporting	azurecomputesummaryp
42	reporting	azurecostentrybill
43	reporting	azurecostentrylineitemdailysummary
44	reporting	azurecostsummarybyaccountp
45	reporting	azurecostsummarybylocationp
46	reporting	azurecostsummarybyservicep
47	reporting	azurecostsummaryp
48	reporting	azuredatabasesummaryp
49	reporting	azurenetworksummaryp
50	reporting	azurestoragesummaryp
51	reporting	azuretagssummary
52	reporting	azuretagsvalues
53	reporting	ocpusagereportperiod
54	reporting	costsummary
55	reporting	currencysettings
56	reporting	gcpcomputesummarybyaccountp
57	reporting	gcpcomputesummaryp
58	reporting	gcpcostentrybill
59	reporting	gcpcostentrylineitemdailysummary
60	reporting	gcpcostsummarybyaccountp
61	reporting	gcpcostsummarybyprojectp
62	reporting	gcpcostsummarybyregionp
63	reporting	gcpcostsummarybyservicep
64	reporting	gcpcostsummaryp
65	reporting	gcpdatabasesummaryp
66	reporting	gcpnetworksummaryp
67	reporting	gcpstoragesummarybyaccountp
68	reporting	gcpstoragesummarybyprojectp
69	reporting	gcpstoragesummarybyregionp
70	reporting	gcpstoragesummarybyservicep
71	reporting	gcpstoragesummaryp
72	reporting	gcptagssummary
73	reporting	gcptagsvalues
74	reporting	gcptopology
75	reporting	ocpallcomputesummarypt
76	reporting	ocpallcostlineitemdailysummaryp
77	reporting	ocpallcostlineitemprojectdailysummaryp
78	reporting	ocpallcostsummarybyaccountpt
79	reporting	ocpallcostsummarybyregionpt
80	reporting	ocpallcostsummarybyservicept
81	reporting	ocpallcostsummarypt
82	reporting	ocpalldatabasesummarypt
83	reporting	ocpallnetworksummarypt
84	reporting	ocpallstoragesummarypt
85	reporting	ocpawscomputesummaryp
86	reporting	ocpawscostlineitemdailysummaryp
87	reporting	ocpawscostlineitemprojectdailysummaryp
88	reporting	ocpawscostsummarybyaccountp
89	reporting	ocpawscostsummarybyregionp
90	reporting	ocpawscostsummarybyservicep
91	reporting	ocpawscostsummaryp
92	reporting	ocpawsdatabasesummaryp
93	reporting	ocpawsnetworksummaryp
94	reporting	ocpawsstoragesummaryp
95	reporting	ocpawstagssummary
96	reporting	ocpawstagsvalues
97	reporting	ocpazurecomputesummaryp
98	reporting	ocpazurecostlineitemdailysummaryp
99	reporting	ocpazurecostlineitemprojectdailysummaryp
100	reporting	ocpazurecostsummarybyaccountp
101	reporting	ocpazurecostsummarybylocationp
102	reporting	ocpazurecostsummarybyservicep
103	reporting	ocpazurecostsummaryp
104	reporting	ocpazuredatabasesummaryp
105	reporting	ocpazurenetworksummaryp
106	reporting	ocpazurestoragesummaryp
107	reporting	ocpazuretagssummary
108	reporting	ocpazuretagsvalues
109	reporting	ocpcluster
110	reporting	ocpcostsummarybynodep
111	reporting	ocpcostsummarybyprojectp
112	reporting	ocpcostsummaryp
113	reporting	ocpgcpcomputesummaryp
114	reporting	ocpgcpcostlineitemdailysummaryp
115	reporting	ocpgcpcostlineitemprojectdailysummaryp
116	reporting	ocpgcpcostsummarybyaccountp
117	reporting	ocpgcpcostsummarybygcpprojectp
118	reporting	ocpgcpcostsummarybyregionp
119	reporting	ocpgcpcostsummarybyservicep
120	reporting	ocpgcpcostsummaryp
121	reporting	ocpgcpdatabasesummaryp
122	reporting	ocpgcpnetworksummaryp
123	reporting	ocpgcpstoragesummaryp
124	reporting	ocpgcptagssummary
125	reporting	ocpgcptagsvalues
126	reporting	ocpnode
127	reporting	ocppodsummarybyprojectp
128	reporting	ocppodsummaryp
129	reporting	ocpproject
130	reporting	ocppvc
131	reporting	ocpstoragevolumelabelsummary
132	reporting	ocptagsvalues
133	reporting	ocpusagelineitemdailysummary
134	reporting	ocpusagepodlabelsummary
135	reporting	ocpvolumesummarybyprojectp
136	reporting	ocpvolumesummaryp
137	reporting	ocicostentrybill
138	reporting	ocicomputesummarybyaccountp
139	reporting	ocicomputesummaryp
140	reporting	ocicostentrylineitemdailysummary
141	reporting	ocicostsummarybyaccountp
142	reporting	ocicostsummarybyregionp
143	reporting	ocicostsummarybyservicep
144	reporting	ocicostsummaryp
145	reporting	ocidatabasesummaryp
146	reporting	ocinetworksummaryp
147	reporting	ocistoragesummarybyaccountp
148	reporting	ocistoragesummaryp
149	reporting	ocitagssummary
150	reporting	ocitagsvalues
151	reporting	partitionedtable
152	reporting	usersettings
153	reporting	enabledtagkeys
154	reporting	subslastprocessed
155	reporting	subsidmap
156	reporting	ocppodsummarybynodep
157	reporting	openshiftcostcategorynamespace
158	reporting	tagmapping
159	reporting	ocpnetworksummaryp
160	reporting	ocpnetworksummarybyprojectp
161	reporting	ocpnetworksummarybynodep
162	reporting	awscostentrylineitemsummarybyec2computep
163	reporting	ocpvirtualmachinesummaryp
164	reporting_common	costusagereportmanifest
165	reporting_common	regionmapping
166	reporting_common	costusagereportstatus
167	reporting_common	delayedcelerytasks
168	reporting_common	diskcapacity
169	cost_models	costmodel
170	cost_models	costmodelaudit
171	cost_models	costmodelmap
\.


--
-- Data for Name: django_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.django_migrations (id, app, name, applied) FROM stdin;
1	api	0001_initial	2025-05-09 21:41:51.814125+00
2	api	0030_auto_20201007_1403	2025-05-09 21:41:51.843265+00
3	api	0031_clone_schema	2025-05-09 21:41:51.897142+00
4	api	0032_presto_delete_log_trigger_func	2025-05-09 21:41:51.944063+00
5	api	0033_sources_name_text	2025-05-09 21:41:51.995182+00
6	api	0034_remove_sources_endpoint_id	2025-05-09 21:41:52.025108+00
7	api	0035_reapply_partition_and_clone_func	2025-05-09 21:41:52.073338+00
8	api	0036_reapply_check_migrations_func	2025-05-09 21:41:52.115187+00
9	api	0037_auto_20210223_2136	2025-05-09 21:41:52.126+00
10	api	0038_drop_app_needs_migrations_func	2025-05-09 21:41:52.166978+00
11	api	0039_create_hive_db	2025-05-09 21:41:52.720111+00
12	api	0040_auto_20210318_1514	2025-05-09 21:41:52.756793+00
13	api	0041_array_subtract_dbfunc	2025-05-09 21:41:52.803076+00
14	api	0042_reapply_clone_func	2025-05-09 21:41:52.842826+00
15	api	0039_create_hive_db	2025-05-09 21:41:52.862435+00
16	api	0043_apply_turbo_schema_clone_func	2025-05-09 21:41:52.89266+00
17	api	0040_auto_20210318_1514	2025-05-09 21:41:52.898794+00
18	api	0044_auto_20210505_1747	2025-05-09 21:41:52.972165+00
19	api	0041_array_subtract_dbfunc	2025-05-09 21:41:53.042608+00
20	api	0045_update_django_migration_sequences	2025-05-09 21:41:53.051136+00
21	api	0042_reapply_clone_func	2025-05-09 21:41:53.1004+00
22	api	0046_jsonb_sha256_text	2025-05-09 21:41:53.109676+00
23	api	0043_apply_turbo_schema_clone_func	2025-05-09 21:41:53.142268+00
24	api	0047_update_django_migration_sequences	2025-05-09 21:41:53.177602+00
25	api	0044_auto_20210505_1747	2025-05-09 21:41:53.20203+00
26	api	0048_new_partition_manager_func	2025-05-09 21:41:53.225703+00
27	api	0045_update_django_migration_sequences	2025-05-09 21:41:53.232381+00
28	api	0049_auto_20210818_2208	2025-05-09 21:41:53.269239+00
29	api	0046_jsonb_sha256_text	2025-05-09 21:41:53.269999+00
30	api	0050_exchangerates	2025-05-09 21:41:53.328725+00
31	api	0047_update_django_migration_sequences	2025-05-09 21:41:53.332186+00
32	api	0051_reapply_partition_trigger_func	2025-05-09 21:41:53.35564+00
33	api	0048_new_partition_manager_func	2025-05-09 21:41:53.392035+00
34	api	0052_sources_provider	2025-05-09 21:41:53.413186+00
35	api	0053_additional_context	2025-05-09 21:41:53.476217+00
36	api	0054_adding_oci_provider	2025-05-09 21:41:53.518777+00
37	api	0055_install_pg_stat_statements	2025-05-09 21:41:53.571407+00
38	api	0056_reapply_clone_schema_func	2025-05-09 21:41:53.62688+00
39	api	0057_add_org_ids	2025-05-09 21:41:53.72742+00
40	api	0058_exchangeratedictionary	2025-05-09 21:41:53.792761+00
41	api	0059_alter_tenant_schema	2025-05-09 21:41:53.908389+00
42	api	0060_provider_polling_timestamp	2025-05-09 21:41:53.946522+00
43	api	0061_alter_providerinfrastructuremap_unique_together	2025-05-09 21:41:53.993678+00
44	api	0062_add_infra_map_account_region	2025-05-09 21:41:54.063548+00
45	api	0063_remove_infra_map_account_region	2025-05-09 21:41:54.110453+00
46	api	0064_delete_dataexportrequest	2025-05-09 21:41:54.130302+00
47	api	0065_alter_exchangerates_currency_type	2025-05-09 21:41:54.159835+00
48	contenttypes	0001_initial	2025-05-09 21:41:54.208547+00
49	contenttypes	0002_remove_content_type_name	2025-05-09 21:41:54.262982+00
50	auth	0001_initial	2025-05-09 21:41:54.856303+00
51	auth	0002_alter_permission_name_max_length	2025-05-09 21:41:54.871601+00
52	auth	0003_alter_user_email_max_length	2025-05-09 21:41:54.921939+00
53	auth	0004_alter_user_username_opts	2025-05-09 21:41:54.948406+00
54	auth	0005_alter_user_last_login_null	2025-05-09 21:41:54.977439+00
55	auth	0006_require_contenttypes_0002	2025-05-09 21:41:54.987415+00
56	auth	0007_alter_validators_add_error_messages	2025-05-09 21:41:55.021807+00
57	auth	0008_alter_user_username_max_length	2025-05-09 21:41:55.102575+00
58	auth	0009_alter_user_last_name_max_length	2025-05-09 21:41:55.136361+00
59	auth	0010_alter_group_name_max_length	2025-05-09 21:41:55.20698+00
60	auth	0011_update_proxy_permissions	2025-05-09 21:41:55.252254+00
61	auth	0012_alter_user_first_name_max_length	2025-05-09 21:41:55.278198+00
62	cost_models	0001_initial	2025-05-09 21:41:55.310622+00
63	cost_models	0002_auto_20210318_1514	2025-05-09 21:41:55.332161+00
64	cost_models	0003_auto_20210615_2011	2025-05-09 21:41:55.35989+00
65	cost_models	0004_auto_20210903_1559	2025-05-09 21:41:55.38191+00
66	cost_models	0005_add_oci_provider	2025-05-09 21:41:55.406907+00
67	cost_models	0006_add_distribution_info	2025-05-09 21:41:55.423111+00
68	cost_models	0007_net_stor_distribute_default	2025-05-09 21:41:55.437177+00
69	key_metrics	0001_initial	2025-05-09 21:41:55.489443+00
70	reporting	0300_squash	2025-05-09 21:43:07.540514+00
71	reporting	0301_create_ocppod_by_node_p	2025-05-09 21:43:07.549437+00
72	reporting	0302_unified_enabledtags	2025-05-09 21:43:07.57438+00
73	reporting	0303_tags_data	2025-05-09 21:43:07.588992+00
74	reporting	0304_subslastprocessed	2025-05-09 21:43:08.088419+00
75	reporting	0305_pvc	2025-05-09 21:43:08.559102+00
76	reporting	0306_subsidmap	2025-05-09 21:43:08.777975+00
77	reporting	0307_ingressreports_customer	2025-05-09 21:43:08.935624+00
78	reporting	0308_ocpusagelineitemdailysummary_all_labels	2025-05-09 21:43:09.217214+00
79	reporting	0309_auto_20231004_1009	2025-05-09 21:43:10.004835+00
80	reporting	0310_delete_ocppodsummarybynodep	2025-05-09 21:43:10.020531+00
81	reporting	0311_actually_delete_ocppodsummarybynodep	2025-05-09 21:43:10.029274+00
82	reporting	0312_create_ocppod_by_node_p	2025-05-09 21:43:10.49102+00
83	reporting	0313_remove_deprecated_tags	2025-05-09 21:43:10.514477+00
84	reporting	0314_ocppod_by_node_p_data_migration	2025-05-09 21:43:10.520152+00
85	reporting	0315_openshiftcostcategorynamespace	2025-05-09 21:43:10.67677+00
86	reporting	0316_remove_openshiftcostcategory_namespace	2025-05-09 21:43:10.742791+00
87	reporting	0317_tagmapping	2025-05-09 21:43:10.756058+00
88	reporting	0318_ocpazurecomputesummaryp_subscription_name_and_more	2025-05-09 21:43:12.55642+00
89	reporting	0319_node_network_costs	2025-05-09 21:43:13.411909+00
90	reporting	0320_ocpcloud_projsumm_network_fields	2025-05-09 21:43:14.346571+00
91	reporting	0321_awscostentrylineitemsummarybyec2compute	2025-05-09 21:43:14.486001+00
92	reporting	0322_alter_awscostentrylineitemsummarybyec2compute_instance_name	2025-05-09 21:43:14.537151+00
93	reporting	0323_remove_awscostentrylineitemsummarybyec2compute_availability_zone_and_org_unit	2025-05-09 21:43:14.719723+00
94	reporting	0324_ocppvc_csi_volume_handle	2025-05-09 21:43:14.957757+00
95	reporting	0325_awscostentrylineitemsummarybyec2computep	2025-05-09 21:43:15.281086+00
96	reporting	0326_delete_awscostentrylineitemsummarybyec2compute	2025-05-09 21:43:15.292295+00
97	reporting	0327_ocpvirtualmachinesummaryp	2025-05-09 21:43:15.433703+00
98	reporting	0328_alter_ocpusagelineitemdailysummary_monthly_cost_type	2025-05-09 21:43:15.510049+00
99	reporting	0329_ocp_vm_storage	2025-05-09 21:43:16.466963+00
100	reporting	0330_alter_ocpusagelineitemdailysummary_monthly_cost_type	2025-05-09 21:43:16.54414+00
101	reporting	0331_remove_costsummary_project_markup_cost_and_more	2025-05-09 21:43:17.508871+00
102	reporting_common	0001_initial	2025-05-09 21:43:18.103858+00
103	reporting_common	0027_auto_20210412_1731	2025-05-09 21:43:18.174631+00
104	reporting_common	0028_costusagereportmanifest_operator_version	2025-05-09 21:43:18.197019+00
105	reporting_common	0029_costusagereportmanifest_operator_info	2025-05-09 21:43:18.262261+00
106	reporting_common	0030_costusagereportmanifest_cluster_id	2025-05-09 21:43:18.281913+00
107	reporting_common	0031_costusagereportmanifest_export_time	2025-05-09 21:43:18.301639+00
108	reporting_common	0032_costusagereportmanifest_last_reports	2025-05-09 21:43:18.324253+00
109	reporting_common	0033_costusagereportmanifest_reports_tracker	2025-05-09 21:43:18.346584+00
110	reporting_common	0034_costusagereportmanifest_operator_daily_files	2025-05-09 21:43:18.383863+00
111	reporting_common	0035_alter_costusagereportmanifest_s3_parquet_cleared	2025-05-09 21:43:18.418587+00
112	reporting_common	0036_auto_20240129_1604	2025-05-09 21:43:18.552839+00
113	reporting_common	0037_manifest_report_data_migration	2025-05-09 21:43:18.920842+00
114	reporting_common	0038_auto_20240208_1759	2025-05-09 21:43:18.98113+00
115	reporting_common	0039_auto_20240213_1715	2025-05-09 21:43:19.091353+00
116	reporting_common	0040_delayedcelerytasks	2025-05-09 21:43:19.124575+00
117	reporting_common	0041_diskcapacity	2025-05-09 21:43:19.699433+00
118	reporting_common	0042_alter_costusagereportmanifest_s3_parquet_cleared	2025-05-09 21:43:19.719686+00
119	sessions	0001_initial	2025-05-09 21:43:19.757294+00
\.


--
-- Data for Name: django_session; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.django_session (session_key, session_data, expire_date) FROM stdin;
\.


--
-- Data for Name: hive_locks; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.hive_locks (hl_lock_ext_id, hl_lock_int_id, hl_txnid, hl_db, hl_table, hl_partition, hl_lock_state, hl_lock_type, hl_last_heartbeat, hl_acquired_at, hl_user, hl_host, hl_heartbeat_count, hl_agent_info, hl_blockedby_ext_id, hl_blockedby_int_id) FROM stdin;
\.


--
-- Data for Name: materialization_rebuild_locks; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.materialization_rebuild_locks (mrl_txn_id, mrl_db_name, mrl_tbl_name, mrl_last_heartbeat) FROM stdin;
\.


--
-- Data for Name: min_history_level; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.min_history_level (mhl_txnid, mhl_min_open_txnid) FROM stdin;
\.


--
-- Data for Name: next_compaction_queue_id; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.next_compaction_queue_id (ncq_next) FROM stdin;
1
\.


--
-- Data for Name: next_lock_id; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.next_lock_id (nl_next) FROM stdin;
1
\.


--
-- Data for Name: next_txn_id; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.next_txn_id (ntxn_next) FROM stdin;
1
\.


--
-- Data for Name: next_write_id; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.next_write_id (nwi_database, nwi_table, nwi_next) FROM stdin;
\.


--
-- Data for Name: region_mapping; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.region_mapping (id, region, region_name) FROM stdin;
\.


--
-- Data for Name: repl_txn_map; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.repl_txn_map (rtm_repl_policy, rtm_src_txn_id, rtm_target_txn_id) FROM stdin;
\.


--
-- Data for Name: reporting_common_costusagereportmanifest; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.reporting_common_costusagereportmanifest (id, assembly_id, billing_period_start_datetime, num_total_files, provider_id, s3_csv_cleared, s3_parquet_cleared, operator_version, cluster_channel, operator_airgapped, operator_certified, operator_errors, cluster_id, report_tracker, operator_daily_reports, s3_parquet_cleared_tracker, daily_archive_start_date, creation_datetime, export_datetime, completed_datetime, state) FROM stdin;
\.


--
-- Data for Name: reporting_common_costusagereportstatus; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.reporting_common_costusagereportstatus (id, report_name, etag, manifest_id, started_datetime, completed_datetime, celery_task_id, failed_status, status) FROM stdin;
\.


--
-- Data for Name: reporting_common_diskcapacity; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.reporting_common_diskcapacity (product_substring, capacity, provider_type) FROM stdin;
P1	4	Azure
P2	8	Azure
P3	16	Azure
P4	32	Azure
P6	64	Azure
P10	128	Azure
P15	256	Azure
P20	512	Azure
P30	1024	Azure
P40	2048	Azure
P50	4096	Azure
P60	8192	Azure
P70	16384	Azure
P80	32767	Azure
S4	32	Azure
S6	64	Azure
S10	128	Azure
S15	256	Azure
S20	512	Azure
S30	1024	Azure
S40	2048	Azure
S50	4096	Azure
S60	8192	Azure
S70	16384	Azure
S80	32767	Azure
E1	4	Azure
E2	8	Azure
E3	16	Azure
E4	32	Azure
E6	64	Azure
E10	128	Azure
E15	256	Azure
E20	512	Azure
E30	1024	Azure
E40	2048	Azure
E50	4096	Azure
E60	8192	Azure
E70	16384	Azure
E80	32767	Azure
\.


--
-- Data for Name: runtime_stats; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.runtime_stats (rs_id, create_time, weight, payload) FROM stdin;
\.


--
-- Data for Name: txn_components; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.txn_components (tc_txnid, tc_database, tc_table, tc_partition, tc_operation_type, tc_writeid) FROM stdin;
\.


--
-- Data for Name: txn_to_write_id; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.txn_to_write_id (t2w_txnid, t2w_database, t2w_table, t2w_writeid) FROM stdin;
\.


--
-- Data for Name: txns; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.txns (txn_id, txn_state, txn_started, txn_last_heartbeat, txn_user, txn_host, txn_agent_info, txn_meta_info, txn_heartbeat_count, txn_type) FROM stdin;
\.


--
-- Data for Name: worker_cache_table; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.worker_cache_table (cache_key, value, expires) FROM stdin;
\.


--
-- Data for Name: write_set; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.write_set (ws_database, ws_table, ws_partition, ws_txnid, ws_commit_id, ws_operation_type) FROM stdin;
\.


--
-- Name: MASTER_KEYS_KEY_ID_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public."MASTER_KEYS_KEY_ID_seq"', 1, false);


--
-- Name: __customer_total_data_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.__customer_total_data_id_seq', 1, false);


--
-- Name: api_customer_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_customer_id_seq', 1, false);


--
-- Name: api_domain_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_domain_id_seq', 1, false);


--
-- Name: api_exchangeratedictionary_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_exchangeratedictionary_id_seq', 1, false);


--
-- Name: api_exchangerates_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_exchangerates_id_seq', 1, false);


--
-- Name: api_providerauthentication_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_providerauthentication_id_seq', 1, false);


--
-- Name: api_providerbillingsource_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_providerbillingsource_id_seq', 1, false);


--
-- Name: api_providerinfrastructuremap_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_providerinfrastructuremap_id_seq', 1, false);


--
-- Name: api_tenant_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_tenant_id_seq', 1, true);


--
-- Name: api_user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.api_user_id_seq', 1, false);


--
-- Name: auth_group_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.auth_group_id_seq', 1, false);


--
-- Name: auth_group_permissions_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.auth_group_permissions_id_seq', 1, false);


--
-- Name: auth_permission_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.auth_permission_id_seq', 684, true);


--
-- Name: auth_user_groups_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.auth_user_groups_id_seq', 1, false);


--
-- Name: auth_user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.auth_user_id_seq', 1, false);


--
-- Name: auth_user_user_permissions_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.auth_user_user_permissions_id_seq', 1, false);


--
-- Name: delayed_celery_tasks_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.delayed_celery_tasks_id_seq', 1, false);


--
-- Name: django_content_type_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.django_content_type_id_seq', 171, true);


--
-- Name: django_migrations_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.django_migrations_id_seq', 119, true);


--
-- Name: region_mapping_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.region_mapping_id_seq', 1, false);


--
-- Name: reporting_common_costusagereportmanifest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.reporting_common_costusagereportmanifest_id_seq', 1, false);


--
-- Name: reporting_common_costusagereportstatus_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.reporting_common_costusagereportstatus_id_seq', 1, false);


--
-- Name: BUCKETING_COLS BUCKETING_COLS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."BUCKETING_COLS"
    ADD CONSTRAINT "BUCKETING_COLS_pkey" PRIMARY KEY ("SD_ID", "INTEGER_IDX");


--
-- Name: CDS CDS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."CDS"
    ADD CONSTRAINT "CDS_pkey" PRIMARY KEY ("CD_ID");


--
-- Name: COLUMNS_V2 COLUMNS_V2_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."COLUMNS_V2"
    ADD CONSTRAINT "COLUMNS_V2_pkey" PRIMARY KEY ("CD_ID", "COLUMN_NAME");


--
-- Name: CTLGS CTLGS_NAME_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."CTLGS"
    ADD CONSTRAINT "CTLGS_NAME_key" UNIQUE ("NAME");


--
-- Name: CTLGS CTLGS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."CTLGS"
    ADD CONSTRAINT "CTLGS_pkey" PRIMARY KEY ("CTLG_ID");


--
-- Name: DATABASE_PARAMS DATABASE_PARAMS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DATABASE_PARAMS"
    ADD CONSTRAINT "DATABASE_PARAMS_pkey" PRIMARY KEY ("DB_ID", "PARAM_KEY");


--
-- Name: DB_PRIVS DBPRIVILEGEINDEX; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DB_PRIVS"
    ADD CONSTRAINT "DBPRIVILEGEINDEX" UNIQUE ("AUTHORIZER", "DB_ID", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "DB_PRIV", "GRANTOR", "GRANTOR_TYPE");


--
-- Name: DBS DBS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DBS"
    ADD CONSTRAINT "DBS_pkey" PRIMARY KEY ("DB_ID");


--
-- Name: DB_PRIVS DB_PRIVS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DB_PRIVS"
    ADD CONSTRAINT "DB_PRIVS_pkey" PRIMARY KEY ("DB_GRANT_ID");


--
-- Name: DELEGATION_TOKENS DELEGATION_TOKENS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DELEGATION_TOKENS"
    ADD CONSTRAINT "DELEGATION_TOKENS_pkey" PRIMARY KEY ("TOKEN_IDENT");


--
-- Name: FUNCS FUNCS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."FUNCS"
    ADD CONSTRAINT "FUNCS_pkey" PRIMARY KEY ("FUNC_ID");


--
-- Name: FUNC_RU FUNC_RU_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."FUNC_RU"
    ADD CONSTRAINT "FUNC_RU_pkey" PRIMARY KEY ("FUNC_ID", "INTEGER_IDX");


--
-- Name: GLOBAL_PRIVS GLOBALPRIVILEGEINDEX; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."GLOBAL_PRIVS"
    ADD CONSTRAINT "GLOBALPRIVILEGEINDEX" UNIQUE ("AUTHORIZER", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "USER_PRIV", "GRANTOR", "GRANTOR_TYPE");


--
-- Name: GLOBAL_PRIVS GLOBAL_PRIVS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."GLOBAL_PRIVS"
    ADD CONSTRAINT "GLOBAL_PRIVS_pkey" PRIMARY KEY ("USER_GRANT_ID");


--
-- Name: IDXS IDXS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."IDXS"
    ADD CONSTRAINT "IDXS_pkey" PRIMARY KEY ("INDEX_ID");


--
-- Name: INDEX_PARAMS INDEX_PARAMS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."INDEX_PARAMS"
    ADD CONSTRAINT "INDEX_PARAMS_pkey" PRIMARY KEY ("INDEX_ID", "PARAM_KEY");


--
-- Name: I_SCHEMA I_SCHEMA_NAME_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."I_SCHEMA"
    ADD CONSTRAINT "I_SCHEMA_NAME_key" UNIQUE ("NAME");


--
-- Name: I_SCHEMA I_SCHEMA_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."I_SCHEMA"
    ADD CONSTRAINT "I_SCHEMA_pkey" PRIMARY KEY ("SCHEMA_ID");


--
-- Name: KEY_CONSTRAINTS KEY_CONSTRAINTS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."KEY_CONSTRAINTS"
    ADD CONSTRAINT "KEY_CONSTRAINTS_pkey" PRIMARY KEY ("CONSTRAINT_NAME", "POSITION");


--
-- Name: MASTER_KEYS MASTER_KEYS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."MASTER_KEYS"
    ADD CONSTRAINT "MASTER_KEYS_pkey" PRIMARY KEY ("KEY_ID");


--
-- Name: MV_CREATION_METADATA MV_CREATION_METADATA_PK; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."MV_CREATION_METADATA"
    ADD CONSTRAINT "MV_CREATION_METADATA_PK" PRIMARY KEY ("MV_CREATION_METADATA_ID");


--
-- Name: NOTIFICATION_LOG NOTIFICATION_LOG_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."NOTIFICATION_LOG"
    ADD CONSTRAINT "NOTIFICATION_LOG_pkey" PRIMARY KEY ("NL_ID");


--
-- Name: NOTIFICATION_SEQUENCE NOTIFICATION_SEQUENCE_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."NOTIFICATION_SEQUENCE"
    ADD CONSTRAINT "NOTIFICATION_SEQUENCE_pkey" PRIMARY KEY ("NNI_ID");


--
-- Name: NUCLEUS_TABLES NUCLEUS_TABLES_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."NUCLEUS_TABLES"
    ADD CONSTRAINT "NUCLEUS_TABLES_pkey" PRIMARY KEY ("CLASS_NAME");


--
-- Name: PARTITIONS PARTITIONS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITIONS"
    ADD CONSTRAINT "PARTITIONS_pkey" PRIMARY KEY ("PART_ID");


--
-- Name: PARTITION_EVENTS PARTITION_EVENTS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITION_EVENTS"
    ADD CONSTRAINT "PARTITION_EVENTS_pkey" PRIMARY KEY ("PART_NAME_ID");


--
-- Name: PARTITION_KEYS PARTITION_KEYS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITION_KEYS"
    ADD CONSTRAINT "PARTITION_KEYS_pkey" PRIMARY KEY ("TBL_ID", "PKEY_NAME");


--
-- Name: PARTITION_KEY_VALS PARTITION_KEY_VALS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITION_KEY_VALS"
    ADD CONSTRAINT "PARTITION_KEY_VALS_pkey" PRIMARY KEY ("PART_ID", "INTEGER_IDX");


--
-- Name: PARTITION_PARAMS PARTITION_PARAMS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITION_PARAMS"
    ADD CONSTRAINT "PARTITION_PARAMS_pkey" PRIMARY KEY ("PART_ID", "PARAM_KEY");


--
-- Name: PART_COL_PRIVS PART_COL_PRIVS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PART_COL_PRIVS"
    ADD CONSTRAINT "PART_COL_PRIVS_pkey" PRIMARY KEY ("PART_COLUMN_GRANT_ID");


--
-- Name: PART_COL_STATS PART_COL_STATS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PART_COL_STATS"
    ADD CONSTRAINT "PART_COL_STATS_pkey" PRIMARY KEY ("CS_ID");


--
-- Name: PART_PRIVS PART_PRIVS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PART_PRIVS"
    ADD CONSTRAINT "PART_PRIVS_pkey" PRIMARY KEY ("PART_GRANT_ID");


--
-- Name: METASTORE_DB_PROPERTIES PROPERTY_KEY_PK; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."METASTORE_DB_PROPERTIES"
    ADD CONSTRAINT "PROPERTY_KEY_PK" PRIMARY KEY ("PROPERTY_KEY");


--
-- Name: ROLES ROLEENTITYINDEX; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."ROLES"
    ADD CONSTRAINT "ROLEENTITYINDEX" UNIQUE ("ROLE_NAME");


--
-- Name: ROLES ROLES_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."ROLES"
    ADD CONSTRAINT "ROLES_pkey" PRIMARY KEY ("ROLE_ID");


--
-- Name: ROLE_MAP ROLE_MAP_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."ROLE_MAP"
    ADD CONSTRAINT "ROLE_MAP_pkey" PRIMARY KEY ("ROLE_GRANT_ID");


--
-- Name: SCHEMA_VERSION SCHEMA_VERSION_SCHEMA_ID_VERSION_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SCHEMA_VERSION"
    ADD CONSTRAINT "SCHEMA_VERSION_SCHEMA_ID_VERSION_key" UNIQUE ("SCHEMA_ID", "VERSION");


--
-- Name: SCHEMA_VERSION SCHEMA_VERSION_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SCHEMA_VERSION"
    ADD CONSTRAINT "SCHEMA_VERSION_pkey" PRIMARY KEY ("SCHEMA_VERSION_ID");


--
-- Name: SDS SDS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SDS"
    ADD CONSTRAINT "SDS_pkey" PRIMARY KEY ("SD_ID");


--
-- Name: SD_PARAMS SD_PARAMS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SD_PARAMS"
    ADD CONSTRAINT "SD_PARAMS_pkey" PRIMARY KEY ("SD_ID", "PARAM_KEY");


--
-- Name: SEQUENCE_TABLE SEQUENCE_TABLE_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SEQUENCE_TABLE"
    ADD CONSTRAINT "SEQUENCE_TABLE_pkey" PRIMARY KEY ("SEQUENCE_NAME");


--
-- Name: SERDES SERDES_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SERDES"
    ADD CONSTRAINT "SERDES_pkey" PRIMARY KEY ("SERDE_ID");


--
-- Name: SERDE_PARAMS SERDE_PARAMS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SERDE_PARAMS"
    ADD CONSTRAINT "SERDE_PARAMS_pkey" PRIMARY KEY ("SERDE_ID", "PARAM_KEY");


--
-- Name: SKEWED_COL_NAMES SKEWED_COL_NAMES_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_COL_NAMES"
    ADD CONSTRAINT "SKEWED_COL_NAMES_pkey" PRIMARY KEY ("SD_ID", "INTEGER_IDX");


--
-- Name: SKEWED_COL_VALUE_LOC_MAP SKEWED_COL_VALUE_LOC_MAP_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_COL_VALUE_LOC_MAP"
    ADD CONSTRAINT "SKEWED_COL_VALUE_LOC_MAP_pkey" PRIMARY KEY ("SD_ID", "STRING_LIST_ID_KID");


--
-- Name: SKEWED_STRING_LIST_VALUES SKEWED_STRING_LIST_VALUES_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_STRING_LIST_VALUES"
    ADD CONSTRAINT "SKEWED_STRING_LIST_VALUES_pkey" PRIMARY KEY ("STRING_LIST_ID", "INTEGER_IDX");


--
-- Name: SKEWED_STRING_LIST SKEWED_STRING_LIST_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_STRING_LIST"
    ADD CONSTRAINT "SKEWED_STRING_LIST_pkey" PRIMARY KEY ("STRING_LIST_ID");


--
-- Name: SKEWED_VALUES SKEWED_VALUES_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_VALUES"
    ADD CONSTRAINT "SKEWED_VALUES_pkey" PRIMARY KEY ("SD_ID_OID", "INTEGER_IDX");


--
-- Name: SORT_COLS SORT_COLS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SORT_COLS"
    ADD CONSTRAINT "SORT_COLS_pkey" PRIMARY KEY ("SD_ID", "INTEGER_IDX");


--
-- Name: TABLE_PARAMS TABLE_PARAMS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TABLE_PARAMS"
    ADD CONSTRAINT "TABLE_PARAMS_pkey" PRIMARY KEY ("TBL_ID", "PARAM_KEY");


--
-- Name: TAB_COL_STATS TAB_COL_STATS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TAB_COL_STATS"
    ADD CONSTRAINT "TAB_COL_STATS_pkey" PRIMARY KEY ("CS_ID");


--
-- Name: TBLS TBLS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TBLS"
    ADD CONSTRAINT "TBLS_pkey" PRIMARY KEY ("TBL_ID");


--
-- Name: TBL_COL_PRIVS TBL_COL_PRIVS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TBL_COL_PRIVS"
    ADD CONSTRAINT "TBL_COL_PRIVS_pkey" PRIMARY KEY ("TBL_COLUMN_GRANT_ID");


--
-- Name: TBL_PRIVS TBL_PRIVS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TBL_PRIVS"
    ADD CONSTRAINT "TBL_PRIVS_pkey" PRIMARY KEY ("TBL_GRANT_ID");


--
-- Name: TYPES TYPES_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TYPES"
    ADD CONSTRAINT "TYPES_pkey" PRIMARY KEY ("TYPES_ID");


--
-- Name: TYPE_FIELDS TYPE_FIELDS_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TYPE_FIELDS"
    ADD CONSTRAINT "TYPE_FIELDS_pkey" PRIMARY KEY ("TYPE_NAME", "FIELD_NAME");


--
-- Name: IDXS UNIQUEINDEX; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."IDXS"
    ADD CONSTRAINT "UNIQUEINDEX" UNIQUE ("INDEX_NAME", "ORIG_TBL_ID");


--
-- Name: PARTITIONS UNIQUEPARTITION; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITIONS"
    ADD CONSTRAINT "UNIQUEPARTITION" UNIQUE ("PART_NAME", "TBL_ID");


--
-- Name: TBLS UNIQUETABLE; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TBLS"
    ADD CONSTRAINT "UNIQUETABLE" UNIQUE ("TBL_NAME", "DB_ID");


--
-- Name: DBS UNIQUE_DATABASE; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DBS"
    ADD CONSTRAINT "UNIQUE_DATABASE" UNIQUE ("NAME", "CTLG_NAME");


--
-- Name: TYPES UNIQUE_TYPE; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TYPES"
    ADD CONSTRAINT "UNIQUE_TYPE" UNIQUE ("TYPE_NAME");


--
-- Name: WM_MAPPING UNIQUE_WM_MAPPING; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_MAPPING"
    ADD CONSTRAINT "UNIQUE_WM_MAPPING" UNIQUE ("RP_ID", "ENTITY_TYPE", "ENTITY_NAME");


--
-- Name: WM_POOL UNIQUE_WM_POOL; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_POOL"
    ADD CONSTRAINT "UNIQUE_WM_POOL" UNIQUE ("RP_ID", "PATH");


--
-- Name: WM_RESOURCEPLAN UNIQUE_WM_RESOURCEPLAN; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_RESOURCEPLAN"
    ADD CONSTRAINT "UNIQUE_WM_RESOURCEPLAN" UNIQUE ("NAME");


--
-- Name: WM_TRIGGER UNIQUE_WM_TRIGGER; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_TRIGGER"
    ADD CONSTRAINT "UNIQUE_WM_TRIGGER" UNIQUE ("RP_ID", "NAME");


--
-- Name: ROLE_MAP USERROLEMAPINDEX; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."ROLE_MAP"
    ADD CONSTRAINT "USERROLEMAPINDEX" UNIQUE ("PRINCIPAL_NAME", "ROLE_ID", "GRANTOR", "GRANTOR_TYPE");


--
-- Name: VERSION VERSION_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."VERSION"
    ADD CONSTRAINT "VERSION_pkey" PRIMARY KEY ("VER_ID");


--
-- Name: WM_MAPPING WM_MAPPING_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_MAPPING"
    ADD CONSTRAINT "WM_MAPPING_pkey" PRIMARY KEY ("MAPPING_ID");


--
-- Name: WM_POOL_TO_TRIGGER WM_POOL_TO_TRIGGER_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_POOL_TO_TRIGGER"
    ADD CONSTRAINT "WM_POOL_TO_TRIGGER_pkey" PRIMARY KEY ("POOL_ID", "TRIGGER_ID");


--
-- Name: WM_POOL WM_POOL_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_POOL"
    ADD CONSTRAINT "WM_POOL_pkey" PRIMARY KEY ("POOL_ID");


--
-- Name: WM_RESOURCEPLAN WM_RESOURCEPLAN_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_RESOURCEPLAN"
    ADD CONSTRAINT "WM_RESOURCEPLAN_pkey" PRIMARY KEY ("RP_ID");


--
-- Name: WM_TRIGGER WM_TRIGGER_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_TRIGGER"
    ADD CONSTRAINT "WM_TRIGGER_pkey" PRIMARY KEY ("TRIGGER_ID");


--
-- Name: __customer_total_data __customer_total_data_date_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.__customer_total_data
    ADD CONSTRAINT __customer_total_data_date_key UNIQUE (date);


--
-- Name: __customer_total_data __customer_total_data_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.__customer_total_data
    ADD CONSTRAINT __customer_total_data_pkey PRIMARY KEY (id);


--
-- Name: api_customer api_customer_account_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_customer
    ADD CONSTRAINT api_customer_account_id_key UNIQUE (account_id);


--
-- Name: api_customer api_customer_org_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_customer
    ADD CONSTRAINT api_customer_org_id_key UNIQUE (org_id);


--
-- Name: api_customer api_customer_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_customer
    ADD CONSTRAINT api_customer_pkey PRIMARY KEY (id);


--
-- Name: api_customer api_customer_schema_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_customer
    ADD CONSTRAINT api_customer_schema_name_key UNIQUE (schema_name);


--
-- Name: api_customer api_customer_uuid_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_customer
    ADD CONSTRAINT api_customer_uuid_key UNIQUE (uuid);


--
-- Name: api_domain api_domain_domain_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_domain
    ADD CONSTRAINT api_domain_domain_key UNIQUE (domain);


--
-- Name: api_domain api_domain_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_domain
    ADD CONSTRAINT api_domain_pkey PRIMARY KEY (id);


--
-- Name: api_exchangeratedictionary api_exchangeratedictionary_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_exchangeratedictionary
    ADD CONSTRAINT api_exchangeratedictionary_pkey PRIMARY KEY (id);


--
-- Name: api_exchangerates api_exchangerates_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_exchangerates
    ADD CONSTRAINT api_exchangerates_pkey PRIMARY KEY (id);


--
-- Name: api_provider api_provider_authentication_id_billin_63c82ec5_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_provider
    ADD CONSTRAINT api_provider_authentication_id_billin_63c82ec5_uniq UNIQUE (authentication_id, billing_source_id, customer_id);


--
-- Name: api_provider api_provider_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_provider
    ADD CONSTRAINT api_provider_pkey PRIMARY KEY (uuid);


--
-- Name: api_providerauthentication api_providerauthentication_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_providerauthentication
    ADD CONSTRAINT api_providerauthentication_pkey PRIMARY KEY (id);


--
-- Name: api_providerauthentication api_providerauthentication_uuid_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_providerauthentication
    ADD CONSTRAINT api_providerauthentication_uuid_key UNIQUE (uuid);


--
-- Name: api_providerbillingsource api_providerbillingsource_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_providerbillingsource
    ADD CONSTRAINT api_providerbillingsource_pkey PRIMARY KEY (id);


--
-- Name: api_providerbillingsource api_providerbillingsource_uuid_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_providerbillingsource
    ADD CONSTRAINT api_providerbillingsource_uuid_key UNIQUE (uuid);


--
-- Name: api_providerinfrastructuremap api_providerinfrastructu_infrastructure_type_infr_9fe23dba_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_providerinfrastructuremap
    ADD CONSTRAINT api_providerinfrastructu_infrastructure_type_infr_9fe23dba_uniq UNIQUE (infrastructure_type, infrastructure_provider_id);


--
-- Name: api_providerinfrastructuremap api_providerinfrastructuremap_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_providerinfrastructuremap
    ADD CONSTRAINT api_providerinfrastructuremap_pkey PRIMARY KEY (id);


--
-- Name: api_sources api_sources_koku_uuid_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_sources
    ADD CONSTRAINT api_sources_koku_uuid_key UNIQUE (koku_uuid);


--
-- Name: api_sources api_sources_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_sources
    ADD CONSTRAINT api_sources_pkey PRIMARY KEY (source_id);


--
-- Name: api_sources api_sources_source_uuid_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_sources
    ADD CONSTRAINT api_sources_source_uuid_key UNIQUE (source_uuid);


--
-- Name: api_tenant api_tenant_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_tenant
    ADD CONSTRAINT api_tenant_pkey PRIMARY KEY (id);


--
-- Name: api_tenant api_tenant_schema_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_tenant
    ADD CONSTRAINT api_tenant_schema_name_key UNIQUE (schema_name);


--
-- Name: api_user api_user_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_user
    ADD CONSTRAINT api_user_pkey PRIMARY KEY (id);


--
-- Name: api_user api_user_username_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_user
    ADD CONSTRAINT api_user_username_key UNIQUE (username);


--
-- Name: api_user api_user_uuid_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_user
    ADD CONSTRAINT api_user_uuid_key UNIQUE (uuid);


--
-- Name: auth_group auth_group_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_group
    ADD CONSTRAINT auth_group_name_key UNIQUE (name);


--
-- Name: auth_group_permissions auth_group_permissions_group_id_permission_id_0cd325b0_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_group_permissions
    ADD CONSTRAINT auth_group_permissions_group_id_permission_id_0cd325b0_uniq UNIQUE (group_id, permission_id);


--
-- Name: auth_group_permissions auth_group_permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_group_permissions
    ADD CONSTRAINT auth_group_permissions_pkey PRIMARY KEY (id);


--
-- Name: auth_group auth_group_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_group
    ADD CONSTRAINT auth_group_pkey PRIMARY KEY (id);


--
-- Name: auth_permission auth_permission_content_type_id_codename_01ab375a_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_permission
    ADD CONSTRAINT auth_permission_content_type_id_codename_01ab375a_uniq UNIQUE (content_type_id, codename);


--
-- Name: auth_permission auth_permission_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_permission
    ADD CONSTRAINT auth_permission_pkey PRIMARY KEY (id);


--
-- Name: auth_user_groups auth_user_groups_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user_groups
    ADD CONSTRAINT auth_user_groups_pkey PRIMARY KEY (id);


--
-- Name: auth_user_groups auth_user_groups_user_id_group_id_94350c0c_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user_groups
    ADD CONSTRAINT auth_user_groups_user_id_group_id_94350c0c_uniq UNIQUE (user_id, group_id);


--
-- Name: auth_user auth_user_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user
    ADD CONSTRAINT auth_user_pkey PRIMARY KEY (id);


--
-- Name: auth_user_user_permissions auth_user_user_permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user_user_permissions
    ADD CONSTRAINT auth_user_user_permissions_pkey PRIMARY KEY (id);


--
-- Name: auth_user_user_permissions auth_user_user_permissions_user_id_permission_id_14a6b632_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user_user_permissions
    ADD CONSTRAINT auth_user_user_permissions_user_id_permission_id_14a6b632_uniq UNIQUE (user_id, permission_id);


--
-- Name: auth_user auth_user_username_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user
    ADD CONSTRAINT auth_user_username_key UNIQUE (username);


--
-- Name: aux_table aux_table_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.aux_table
    ADD CONSTRAINT aux_table_pkey PRIMARY KEY (mt_key1, mt_key2);


--
-- Name: compaction_queue compaction_queue_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.compaction_queue
    ADD CONSTRAINT compaction_queue_pkey PRIMARY KEY (cq_id);


--
-- Name: completed_compactions completed_compactions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.completed_compactions
    ADD CONSTRAINT completed_compactions_pkey PRIMARY KEY (cc_id);


--
-- Name: delayed_celery_tasks delayed_celery_tasks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.delayed_celery_tasks
    ADD CONSTRAINT delayed_celery_tasks_pkey PRIMARY KEY (id);


--
-- Name: django_content_type django_content_type_app_label_model_76bd3d3b_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.django_content_type
    ADD CONSTRAINT django_content_type_app_label_model_76bd3d3b_uniq UNIQUE (app_label, model);


--
-- Name: django_content_type django_content_type_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.django_content_type
    ADD CONSTRAINT django_content_type_pkey PRIMARY KEY (id);


--
-- Name: django_migrations django_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.django_migrations
    ADD CONSTRAINT django_migrations_pkey PRIMARY KEY (id);


--
-- Name: django_session django_session_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.django_session
    ADD CONSTRAINT django_session_pkey PRIMARY KEY (session_key);


--
-- Name: hive_locks hive_locks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.hive_locks
    ADD CONSTRAINT hive_locks_pkey PRIMARY KEY (hl_lock_ext_id, hl_lock_int_id);


--
-- Name: materialization_rebuild_locks materialization_rebuild_locks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.materialization_rebuild_locks
    ADD CONSTRAINT materialization_rebuild_locks_pkey PRIMARY KEY (mrl_txn_id);


--
-- Name: min_history_level min_history_level_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.min_history_level
    ADD CONSTRAINT min_history_level_pkey PRIMARY KEY (mhl_txnid);


--
-- Name: region_mapping region_mapping_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.region_mapping
    ADD CONSTRAINT region_mapping_pkey PRIMARY KEY (id);


--
-- Name: region_mapping region_mapping_region_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.region_mapping
    ADD CONSTRAINT region_mapping_region_key UNIQUE (region);


--
-- Name: region_mapping region_mapping_region_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.region_mapping
    ADD CONSTRAINT region_mapping_region_name_key UNIQUE (region_name);


--
-- Name: repl_txn_map repl_txn_map_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.repl_txn_map
    ADD CONSTRAINT repl_txn_map_pkey PRIMARY KEY (rtm_repl_policy, rtm_src_txn_id);


--
-- Name: reporting_common_costusagereportstatus reporting_common_costusa_manifest_id_report_name_8b7bf9d2_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reporting_common_costusagereportstatus
    ADD CONSTRAINT reporting_common_costusa_manifest_id_report_name_8b7bf9d2_uniq UNIQUE (manifest_id, report_name);


--
-- Name: reporting_common_costusagereportmanifest reporting_common_costusa_provider_id_assembly_id_32d4d2f1_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reporting_common_costusagereportmanifest
    ADD CONSTRAINT reporting_common_costusa_provider_id_assembly_id_32d4d2f1_uniq UNIQUE (provider_id, assembly_id);


--
-- Name: reporting_common_costusagereportmanifest reporting_common_costusagereportmanifest_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reporting_common_costusagereportmanifest
    ADD CONSTRAINT reporting_common_costusagereportmanifest_pkey PRIMARY KEY (id);


--
-- Name: reporting_common_costusagereportstatus reporting_common_costusagereportstatus_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reporting_common_costusagereportstatus
    ADD CONSTRAINT reporting_common_costusagereportstatus_pkey PRIMARY KEY (id);


--
-- Name: reporting_common_diskcapacity reporting_common_diskcap_product_substring_capaci_06daf246_uniq; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reporting_common_diskcapacity
    ADD CONSTRAINT reporting_common_diskcap_product_substring_capaci_06daf246_uniq UNIQUE (product_substring, capacity, provider_type);


--
-- Name: reporting_common_diskcapacity reporting_common_diskcapacity_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reporting_common_diskcapacity
    ADD CONSTRAINT reporting_common_diskcapacity_pkey PRIMARY KEY (product_substring);


--
-- Name: runtime_stats runtime_stats_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.runtime_stats
    ADD CONSTRAINT runtime_stats_pkey PRIMARY KEY (rs_id);


--
-- Name: txns txns_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.txns
    ADD CONSTRAINT txns_pkey PRIMARY KEY (txn_id);


--
-- Name: worker_cache_table worker_cache_table_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.worker_cache_table
    ADD CONSTRAINT worker_cache_table_pkey PRIMARY KEY (cache_key);


--
-- Name: BUCKETING_COLS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "BUCKETING_COLS_N49" ON public."BUCKETING_COLS" USING btree ("SD_ID");


--
-- Name: CONSTRAINTS_CONSTRAINT_TYPE_INDEX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "CONSTRAINTS_CONSTRAINT_TYPE_INDEX" ON public."KEY_CONSTRAINTS" USING btree ("CONSTRAINT_TYPE");


--
-- Name: CONSTRAINTS_PARENT_TBLID_INDEX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "CONSTRAINTS_PARENT_TBLID_INDEX" ON public."KEY_CONSTRAINTS" USING btree ("PARENT_TBL_ID");


--
-- Name: DATABASE_PARAMS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "DATABASE_PARAMS_N49" ON public."DATABASE_PARAMS" USING btree ("DB_ID");


--
-- Name: DB_PRIVS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "DB_PRIVS_N49" ON public."DB_PRIVS" USING btree ("DB_ID");


--
-- Name: FUNCS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "FUNCS_N49" ON public."FUNCS" USING btree ("DB_ID");


--
-- Name: FUNC_RU_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "FUNC_RU_N49" ON public."FUNC_RU" USING btree ("FUNC_ID");


--
-- Name: IDXS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "IDXS_N49" ON public."IDXS" USING btree ("ORIG_TBL_ID");


--
-- Name: IDXS_N50; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "IDXS_N50" ON public."IDXS" USING btree ("INDEX_TBL_ID");


--
-- Name: IDXS_N51; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "IDXS_N51" ON public."IDXS" USING btree ("SD_ID");


--
-- Name: INDEX_PARAMS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "INDEX_PARAMS_N49" ON public."INDEX_PARAMS" USING btree ("INDEX_ID");


--
-- Name: MV_UNIQUE_TABLE; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "MV_UNIQUE_TABLE" ON public."MV_CREATION_METADATA" USING btree ("TBL_NAME", "DB_NAME");


--
-- Name: PARTITIONCOLUMNPRIVILEGEINDEX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PARTITIONCOLUMNPRIVILEGEINDEX" ON public."PART_COL_PRIVS" USING btree ("AUTHORIZER", "PART_ID", "COLUMN_NAME", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "PART_COL_PRIV", "GRANTOR", "GRANTOR_TYPE");


--
-- Name: PARTITIONEVENTINDEX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PARTITIONEVENTINDEX" ON public."PARTITION_EVENTS" USING btree ("PARTITION_NAME");


--
-- Name: PARTITIONS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PARTITIONS_N49" ON public."PARTITIONS" USING btree ("TBL_ID");


--
-- Name: PARTITIONS_N50; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PARTITIONS_N50" ON public."PARTITIONS" USING btree ("SD_ID");


--
-- Name: PARTITION_KEYS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PARTITION_KEYS_N49" ON public."PARTITION_KEYS" USING btree ("TBL_ID");


--
-- Name: PARTITION_KEY_VALS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PARTITION_KEY_VALS_N49" ON public."PARTITION_KEY_VALS" USING btree ("PART_ID");


--
-- Name: PARTITION_PARAMS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PARTITION_PARAMS_N49" ON public."PARTITION_PARAMS" USING btree ("PART_ID");


--
-- Name: PARTPRIVILEGEINDEX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PARTPRIVILEGEINDEX" ON public."PART_PRIVS" USING btree ("AUTHORIZER", "PART_ID", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "PART_PRIV", "GRANTOR", "GRANTOR_TYPE");


--
-- Name: PART_COL_PRIVS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PART_COL_PRIVS_N49" ON public."PART_COL_PRIVS" USING btree ("PART_ID");


--
-- Name: PART_COL_STATS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PART_COL_STATS_N49" ON public."PART_COL_STATS" USING btree ("PART_ID");


--
-- Name: PART_PRIVS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PART_PRIVS_N49" ON public."PART_PRIVS" USING btree ("PART_ID");


--
-- Name: PCS_STATS_IDX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "PCS_STATS_IDX" ON public."PART_COL_STATS" USING btree ("CAT_NAME", "DB_NAME", "TABLE_NAME", "COLUMN_NAME", "PARTITION_NAME");


--
-- Name: ROLE_MAP_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "ROLE_MAP_N49" ON public."ROLE_MAP" USING btree ("ROLE_ID");


--
-- Name: SDS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "SDS_N49" ON public."SDS" USING btree ("SERDE_ID");


--
-- Name: SD_PARAMS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "SD_PARAMS_N49" ON public."SD_PARAMS" USING btree ("SD_ID");


--
-- Name: SERDE_PARAMS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "SERDE_PARAMS_N49" ON public."SERDE_PARAMS" USING btree ("SERDE_ID");


--
-- Name: SORT_COLS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "SORT_COLS_N49" ON public."SORT_COLS" USING btree ("SD_ID");


--
-- Name: TABLECOLUMNPRIVILEGEINDEX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TABLECOLUMNPRIVILEGEINDEX" ON public."TBL_COL_PRIVS" USING btree ("AUTHORIZER", "TBL_ID", "COLUMN_NAME", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "TBL_COL_PRIV", "GRANTOR", "GRANTOR_TYPE");


--
-- Name: TABLEPRIVILEGEINDEX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TABLEPRIVILEGEINDEX" ON public."TBL_PRIVS" USING btree ("AUTHORIZER", "TBL_ID", "PRINCIPAL_NAME", "PRINCIPAL_TYPE", "TBL_PRIV", "GRANTOR", "GRANTOR_TYPE");


--
-- Name: TABLE_PARAMS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TABLE_PARAMS_N49" ON public."TABLE_PARAMS" USING btree ("TBL_ID");


--
-- Name: TAB_COL_STATS_IDX; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TAB_COL_STATS_IDX" ON public."TAB_COL_STATS" USING btree ("CAT_NAME", "DB_NAME", "TABLE_NAME", "COLUMN_NAME");


--
-- Name: TAB_COL_STATS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TAB_COL_STATS_N49" ON public."TAB_COL_STATS" USING btree ("TBL_ID");


--
-- Name: TBLS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TBLS_N49" ON public."TBLS" USING btree ("DB_ID");


--
-- Name: TBLS_N50; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TBLS_N50" ON public."TBLS" USING btree ("SD_ID");


--
-- Name: TBL_COL_PRIVS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TBL_COL_PRIVS_N49" ON public."TBL_COL_PRIVS" USING btree ("TBL_ID");


--
-- Name: TBL_PRIVS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TBL_PRIVS_N49" ON public."TBL_PRIVS" USING btree ("TBL_ID");


--
-- Name: TYPE_FIELDS_N49; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX "TYPE_FIELDS_N49" ON public."TYPE_FIELDS" USING btree ("TYPE_NAME");


--
-- Name: UNIQUEFUNCTION; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX "UNIQUEFUNCTION" ON public."FUNCS" USING btree ("FUNC_NAME", "DB_ID");


--
-- Name: api_customer_account_id_206bec02_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_customer_account_id_206bec02_like ON public.api_customer USING btree (account_id varchar_pattern_ops);


--
-- Name: api_customer_org_id_a5f7fbf8_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_customer_org_id_a5f7fbf8_like ON public.api_customer USING btree (org_id varchar_pattern_ops);


--
-- Name: api_customer_schema_name_6b716c4b_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_customer_schema_name_6b716c4b_like ON public.api_customer USING btree (schema_name text_pattern_ops);


--
-- Name: api_domain_domain_cf8d9ba9_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_domain_domain_cf8d9ba9_like ON public.api_domain USING btree (domain varchar_pattern_ops);


--
-- Name: api_domain_is_primary_f7f31717; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_domain_is_primary_f7f31717 ON public.api_domain USING btree (is_primary);


--
-- Name: api_domain_tenant_id_a0b9faa1; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_domain_tenant_id_a0b9faa1 ON public.api_domain USING btree (tenant_id);


--
-- Name: api_provider_authentication_id_201fd4b9; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_provider_authentication_id_201fd4b9 ON public.api_provider USING btree (authentication_id);


--
-- Name: api_provider_billing_source_id_cb6b5a6f; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_provider_billing_source_id_cb6b5a6f ON public.api_provider USING btree (billing_source_id);


--
-- Name: api_provider_created_by_id_e740fc35; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_provider_created_by_id_e740fc35 ON public.api_provider USING btree (created_by_id);


--
-- Name: api_provider_customer_id_87062290; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_provider_customer_id_87062290 ON public.api_provider USING btree (customer_id);


--
-- Name: api_provider_infrastructure_id_1a0be3a0; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_provider_infrastructure_id_1a0be3a0 ON public.api_provider USING btree (infrastructure_id);


--
-- Name: api_providerinfrastructure_infrastructure_provider_id_cfa87d94; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_providerinfrastructure_infrastructure_provider_id_cfa87d94 ON public.api_providerinfrastructuremap USING btree (infrastructure_provider_id);


--
-- Name: api_sources_provider_id_445890f5; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_sources_provider_id_445890f5 ON public.api_sources USING btree (provider_id);


--
-- Name: api_tenant_schema_name_733d339b_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_tenant_schema_name_733d339b_like ON public.api_tenant USING btree (schema_name varchar_pattern_ops);


--
-- Name: api_user_customer_id_90bd21ef; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_user_customer_id_90bd21ef ON public.api_user USING btree (customer_id);


--
-- Name: api_user_username_cf4e88d2_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX api_user_username_cf4e88d2_like ON public.api_user USING btree (username varchar_pattern_ops);


--
-- Name: auth_group_name_a6ea08ec_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_group_name_a6ea08ec_like ON public.auth_group USING btree (name varchar_pattern_ops);


--
-- Name: auth_group_permissions_group_id_b120cbf9; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_group_permissions_group_id_b120cbf9 ON public.auth_group_permissions USING btree (group_id);


--
-- Name: auth_group_permissions_permission_id_84c5c92e; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_group_permissions_permission_id_84c5c92e ON public.auth_group_permissions USING btree (permission_id);


--
-- Name: auth_permission_content_type_id_2f476e4b; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_permission_content_type_id_2f476e4b ON public.auth_permission USING btree (content_type_id);


--
-- Name: auth_user_groups_group_id_97559544; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_user_groups_group_id_97559544 ON public.auth_user_groups USING btree (group_id);


--
-- Name: auth_user_groups_user_id_6a12ed8b; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_user_groups_user_id_6a12ed8b ON public.auth_user_groups USING btree (user_id);


--
-- Name: auth_user_user_permissions_permission_id_1fbb5f2c; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_user_user_permissions_permission_id_1fbb5f2c ON public.auth_user_user_permissions USING btree (permission_id);


--
-- Name: auth_user_user_permissions_user_id_a95ead1b; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_user_user_permissions_user_id_a95ead1b ON public.auth_user_user_permissions USING btree (user_id);


--
-- Name: auth_user_username_6821ab7c_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX auth_user_username_6821ab7c_like ON public.auth_user USING btree (username varchar_pattern_ops);


--
-- Name: completed_txn_components_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX completed_txn_components_index ON public.completed_txn_components USING btree (ctc_database, ctc_table, ctc_partition);


--
-- Name: django_session_expire_date_a5c62663; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX django_session_expire_date_a5c62663 ON public.django_session USING btree (expire_date);


--
-- Name: django_session_session_key_c0390e0f_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX django_session_session_key_c0390e0f_like ON public.django_session USING btree (session_key varchar_pattern_ops);


--
-- Name: hl_txnid_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX hl_txnid_index ON public.hive_locks USING hash (hl_txnid);


--
-- Name: idx_runtime_stats_create_time; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_runtime_stats_create_time ON public.runtime_stats USING btree (create_time);


--
-- Name: min_history_level_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX min_history_level_idx ON public.min_history_level USING btree (mhl_min_open_txnid);


--
-- Name: next_write_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX next_write_id_idx ON public.next_write_id USING btree (nwi_database, nwi_table);


--
-- Name: region_mapping_region_9c0d71ba_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX region_mapping_region_9c0d71ba_like ON public.region_mapping USING btree (region varchar_pattern_ops);


--
-- Name: region_mapping_region_name_ad295b89_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX region_mapping_region_name_ad295b89_like ON public.region_mapping USING btree (region_name varchar_pattern_ops);


--
-- Name: reporting_common_costusagereportmanifest_provider_id_6abb15de; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX reporting_common_costusagereportmanifest_provider_id_6abb15de ON public.reporting_common_costusagereportmanifest USING btree (provider_id);


--
-- Name: reporting_common_costusagereportstatus_manifest_id_62ef64b9; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX reporting_common_costusagereportstatus_manifest_id_62ef64b9 ON public.reporting_common_costusagereportstatus USING btree (manifest_id);


--
-- Name: reporting_common_diskcapacity_product_substring_a48d6ab1_like; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX reporting_common_diskcapacity_product_substring_a48d6ab1_like ON public.reporting_common_diskcapacity USING btree (product_substring varchar_pattern_ops);


--
-- Name: tbl_to_txn_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX tbl_to_txn_id_idx ON public.txn_to_write_id USING btree (t2w_database, t2w_table, t2w_txnid);


--
-- Name: tbl_to_write_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX tbl_to_write_id_idx ON public.txn_to_write_id USING btree (t2w_database, t2w_table, t2w_writeid);


--
-- Name: tc_txnid_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX tc_txnid_index ON public.txn_components USING hash (tc_txnid);


--
-- Name: worker_cache_table_expires; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX worker_cache_table_expires ON public.worker_cache_table USING btree (expires);


--
-- Name: BUCKETING_COLS BUCKETING_COLS_SD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."BUCKETING_COLS"
    ADD CONSTRAINT "BUCKETING_COLS_SD_ID_fkey" FOREIGN KEY ("SD_ID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: COLUMNS_V2 COLUMNS_V2_CD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."COLUMNS_V2"
    ADD CONSTRAINT "COLUMNS_V2_CD_ID_fkey" FOREIGN KEY ("CD_ID") REFERENCES public."CDS"("CD_ID") DEFERRABLE;


--
-- Name: DATABASE_PARAMS DATABASE_PARAMS_DB_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DATABASE_PARAMS"
    ADD CONSTRAINT "DATABASE_PARAMS_DB_ID_fkey" FOREIGN KEY ("DB_ID") REFERENCES public."DBS"("DB_ID") DEFERRABLE;


--
-- Name: DBS DBS_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DBS"
    ADD CONSTRAINT "DBS_FK1" FOREIGN KEY ("CTLG_NAME") REFERENCES public."CTLGS"("NAME");


--
-- Name: DB_PRIVS DB_PRIVS_DB_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."DB_PRIVS"
    ADD CONSTRAINT "DB_PRIVS_DB_ID_fkey" FOREIGN KEY ("DB_ID") REFERENCES public."DBS"("DB_ID") DEFERRABLE;


--
-- Name: FUNCS FUNCS_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."FUNCS"
    ADD CONSTRAINT "FUNCS_FK1" FOREIGN KEY ("DB_ID") REFERENCES public."DBS"("DB_ID") DEFERRABLE;


--
-- Name: FUNC_RU FUNC_RU_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."FUNC_RU"
    ADD CONSTRAINT "FUNC_RU_FK1" FOREIGN KEY ("FUNC_ID") REFERENCES public."FUNCS"("FUNC_ID") DEFERRABLE;


--
-- Name: IDXS IDXS_INDEX_TBL_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."IDXS"
    ADD CONSTRAINT "IDXS_INDEX_TBL_ID_fkey" FOREIGN KEY ("INDEX_TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: IDXS IDXS_ORIG_TBL_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."IDXS"
    ADD CONSTRAINT "IDXS_ORIG_TBL_ID_fkey" FOREIGN KEY ("ORIG_TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: IDXS IDXS_SD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."IDXS"
    ADD CONSTRAINT "IDXS_SD_ID_fkey" FOREIGN KEY ("SD_ID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: INDEX_PARAMS INDEX_PARAMS_INDEX_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."INDEX_PARAMS"
    ADD CONSTRAINT "INDEX_PARAMS_INDEX_ID_fkey" FOREIGN KEY ("INDEX_ID") REFERENCES public."IDXS"("INDEX_ID") DEFERRABLE;


--
-- Name: I_SCHEMA I_SCHEMA_DB_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."I_SCHEMA"
    ADD CONSTRAINT "I_SCHEMA_DB_ID_fkey" FOREIGN KEY ("DB_ID") REFERENCES public."DBS"("DB_ID");


--
-- Name: MV_TABLES_USED MV_TABLES_USED_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."MV_TABLES_USED"
    ADD CONSTRAINT "MV_TABLES_USED_FK1" FOREIGN KEY ("MV_CREATION_METADATA_ID") REFERENCES public."MV_CREATION_METADATA"("MV_CREATION_METADATA_ID") DEFERRABLE;


--
-- Name: MV_TABLES_USED MV_TABLES_USED_FK2; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."MV_TABLES_USED"
    ADD CONSTRAINT "MV_TABLES_USED_FK2" FOREIGN KEY ("TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: PARTITIONS PARTITIONS_SD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITIONS"
    ADD CONSTRAINT "PARTITIONS_SD_ID_fkey" FOREIGN KEY ("SD_ID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: PARTITIONS PARTITIONS_TBL_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITIONS"
    ADD CONSTRAINT "PARTITIONS_TBL_ID_fkey" FOREIGN KEY ("TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: PARTITION_KEYS PARTITION_KEYS_TBL_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITION_KEYS"
    ADD CONSTRAINT "PARTITION_KEYS_TBL_ID_fkey" FOREIGN KEY ("TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: PARTITION_KEY_VALS PARTITION_KEY_VALS_PART_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITION_KEY_VALS"
    ADD CONSTRAINT "PARTITION_KEY_VALS_PART_ID_fkey" FOREIGN KEY ("PART_ID") REFERENCES public."PARTITIONS"("PART_ID") DEFERRABLE;


--
-- Name: PARTITION_PARAMS PARTITION_PARAMS_PART_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PARTITION_PARAMS"
    ADD CONSTRAINT "PARTITION_PARAMS_PART_ID_fkey" FOREIGN KEY ("PART_ID") REFERENCES public."PARTITIONS"("PART_ID") DEFERRABLE;


--
-- Name: PART_COL_PRIVS PART_COL_PRIVS_PART_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PART_COL_PRIVS"
    ADD CONSTRAINT "PART_COL_PRIVS_PART_ID_fkey" FOREIGN KEY ("PART_ID") REFERENCES public."PARTITIONS"("PART_ID") DEFERRABLE;


--
-- Name: PART_COL_STATS PART_COL_STATS_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PART_COL_STATS"
    ADD CONSTRAINT "PART_COL_STATS_fkey" FOREIGN KEY ("PART_ID") REFERENCES public."PARTITIONS"("PART_ID") DEFERRABLE;


--
-- Name: PART_PRIVS PART_PRIVS_PART_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."PART_PRIVS"
    ADD CONSTRAINT "PART_PRIVS_PART_ID_fkey" FOREIGN KEY ("PART_ID") REFERENCES public."PARTITIONS"("PART_ID") DEFERRABLE;


--
-- Name: ROLE_MAP ROLE_MAP_ROLE_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."ROLE_MAP"
    ADD CONSTRAINT "ROLE_MAP_ROLE_ID_fkey" FOREIGN KEY ("ROLE_ID") REFERENCES public."ROLES"("ROLE_ID") DEFERRABLE;


--
-- Name: SCHEMA_VERSION SCHEMA_VERSION_CD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SCHEMA_VERSION"
    ADD CONSTRAINT "SCHEMA_VERSION_CD_ID_fkey" FOREIGN KEY ("CD_ID") REFERENCES public."CDS"("CD_ID");


--
-- Name: SCHEMA_VERSION SCHEMA_VERSION_SCHEMA_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SCHEMA_VERSION"
    ADD CONSTRAINT "SCHEMA_VERSION_SCHEMA_ID_fkey" FOREIGN KEY ("SCHEMA_ID") REFERENCES public."I_SCHEMA"("SCHEMA_ID");


--
-- Name: SCHEMA_VERSION SCHEMA_VERSION_SERDE_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SCHEMA_VERSION"
    ADD CONSTRAINT "SCHEMA_VERSION_SERDE_ID_fkey" FOREIGN KEY ("SERDE_ID") REFERENCES public."SERDES"("SERDE_ID");


--
-- Name: SDS SDS_CD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SDS"
    ADD CONSTRAINT "SDS_CD_ID_fkey" FOREIGN KEY ("CD_ID") REFERENCES public."CDS"("CD_ID") DEFERRABLE;


--
-- Name: SDS SDS_SERDE_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SDS"
    ADD CONSTRAINT "SDS_SERDE_ID_fkey" FOREIGN KEY ("SERDE_ID") REFERENCES public."SERDES"("SERDE_ID") DEFERRABLE;


--
-- Name: SD_PARAMS SD_PARAMS_SD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SD_PARAMS"
    ADD CONSTRAINT "SD_PARAMS_SD_ID_fkey" FOREIGN KEY ("SD_ID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: SERDE_PARAMS SERDE_PARAMS_SERDE_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SERDE_PARAMS"
    ADD CONSTRAINT "SERDE_PARAMS_SERDE_ID_fkey" FOREIGN KEY ("SERDE_ID") REFERENCES public."SERDES"("SERDE_ID") DEFERRABLE;


--
-- Name: SKEWED_COL_NAMES SKEWED_COL_NAMES_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_COL_NAMES"
    ADD CONSTRAINT "SKEWED_COL_NAMES_fkey" FOREIGN KEY ("SD_ID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: SKEWED_COL_VALUE_LOC_MAP SKEWED_COL_VALUE_LOC_MAP_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_COL_VALUE_LOC_MAP"
    ADD CONSTRAINT "SKEWED_COL_VALUE_LOC_MAP_fkey1" FOREIGN KEY ("SD_ID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: SKEWED_COL_VALUE_LOC_MAP SKEWED_COL_VALUE_LOC_MAP_fkey2; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_COL_VALUE_LOC_MAP"
    ADD CONSTRAINT "SKEWED_COL_VALUE_LOC_MAP_fkey2" FOREIGN KEY ("STRING_LIST_ID_KID") REFERENCES public."SKEWED_STRING_LIST"("STRING_LIST_ID") DEFERRABLE;


--
-- Name: SKEWED_STRING_LIST_VALUES SKEWED_STRING_LIST_VALUES_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_STRING_LIST_VALUES"
    ADD CONSTRAINT "SKEWED_STRING_LIST_VALUES_fkey" FOREIGN KEY ("STRING_LIST_ID") REFERENCES public."SKEWED_STRING_LIST"("STRING_LIST_ID") DEFERRABLE;


--
-- Name: SKEWED_VALUES SKEWED_VALUES_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_VALUES"
    ADD CONSTRAINT "SKEWED_VALUES_fkey1" FOREIGN KEY ("STRING_LIST_ID_EID") REFERENCES public."SKEWED_STRING_LIST"("STRING_LIST_ID") DEFERRABLE;


--
-- Name: SKEWED_VALUES SKEWED_VALUES_fkey2; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SKEWED_VALUES"
    ADD CONSTRAINT "SKEWED_VALUES_fkey2" FOREIGN KEY ("SD_ID_OID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: SORT_COLS SORT_COLS_SD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."SORT_COLS"
    ADD CONSTRAINT "SORT_COLS_SD_ID_fkey" FOREIGN KEY ("SD_ID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: TABLE_PARAMS TABLE_PARAMS_TBL_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TABLE_PARAMS"
    ADD CONSTRAINT "TABLE_PARAMS_TBL_ID_fkey" FOREIGN KEY ("TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: TAB_COL_STATS TAB_COL_STATS_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TAB_COL_STATS"
    ADD CONSTRAINT "TAB_COL_STATS_fkey" FOREIGN KEY ("TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: TBLS TBLS_DB_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TBLS"
    ADD CONSTRAINT "TBLS_DB_ID_fkey" FOREIGN KEY ("DB_ID") REFERENCES public."DBS"("DB_ID") DEFERRABLE;


--
-- Name: TBLS TBLS_SD_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TBLS"
    ADD CONSTRAINT "TBLS_SD_ID_fkey" FOREIGN KEY ("SD_ID") REFERENCES public."SDS"("SD_ID") DEFERRABLE;


--
-- Name: TBL_COL_PRIVS TBL_COL_PRIVS_TBL_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TBL_COL_PRIVS"
    ADD CONSTRAINT "TBL_COL_PRIVS_TBL_ID_fkey" FOREIGN KEY ("TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: TBL_PRIVS TBL_PRIVS_TBL_ID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TBL_PRIVS"
    ADD CONSTRAINT "TBL_PRIVS_TBL_ID_fkey" FOREIGN KEY ("TBL_ID") REFERENCES public."TBLS"("TBL_ID") DEFERRABLE;


--
-- Name: TYPE_FIELDS TYPE_FIELDS_TYPE_NAME_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."TYPE_FIELDS"
    ADD CONSTRAINT "TYPE_FIELDS_TYPE_NAME_fkey" FOREIGN KEY ("TYPE_NAME") REFERENCES public."TYPES"("TYPES_ID") DEFERRABLE;


--
-- Name: WM_MAPPING WM_MAPPING_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_MAPPING"
    ADD CONSTRAINT "WM_MAPPING_FK1" FOREIGN KEY ("RP_ID") REFERENCES public."WM_RESOURCEPLAN"("RP_ID") DEFERRABLE;


--
-- Name: WM_MAPPING WM_MAPPING_FK2; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_MAPPING"
    ADD CONSTRAINT "WM_MAPPING_FK2" FOREIGN KEY ("POOL_ID") REFERENCES public."WM_POOL"("POOL_ID") DEFERRABLE;


--
-- Name: WM_POOL WM_POOL_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_POOL"
    ADD CONSTRAINT "WM_POOL_FK1" FOREIGN KEY ("RP_ID") REFERENCES public."WM_RESOURCEPLAN"("RP_ID") DEFERRABLE;


--
-- Name: WM_POOL_TO_TRIGGER WM_POOL_TO_TRIGGER_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_POOL_TO_TRIGGER"
    ADD CONSTRAINT "WM_POOL_TO_TRIGGER_FK1" FOREIGN KEY ("POOL_ID") REFERENCES public."WM_POOL"("POOL_ID") DEFERRABLE;


--
-- Name: WM_POOL_TO_TRIGGER WM_POOL_TO_TRIGGER_FK2; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_POOL_TO_TRIGGER"
    ADD CONSTRAINT "WM_POOL_TO_TRIGGER_FK2" FOREIGN KEY ("TRIGGER_ID") REFERENCES public."WM_TRIGGER"("TRIGGER_ID") DEFERRABLE;


--
-- Name: WM_RESOURCEPLAN WM_RESOURCEPLAN_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_RESOURCEPLAN"
    ADD CONSTRAINT "WM_RESOURCEPLAN_FK1" FOREIGN KEY ("DEFAULT_POOL_ID") REFERENCES public."WM_POOL"("POOL_ID") DEFERRABLE;


--
-- Name: WM_TRIGGER WM_TRIGGER_FK1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."WM_TRIGGER"
    ADD CONSTRAINT "WM_TRIGGER_FK1" FOREIGN KEY ("RP_ID") REFERENCES public."WM_RESOURCEPLAN"("RP_ID") DEFERRABLE;


--
-- Name: api_domain api_domain_tenant_id_a0b9faa1_fk_api_tenant_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_domain
    ADD CONSTRAINT api_domain_tenant_id_a0b9faa1_fk_api_tenant_id FOREIGN KEY (tenant_id) REFERENCES public.api_tenant(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: api_provider api_provider_authentication_id_201fd4b9_fk_api_provi; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_provider
    ADD CONSTRAINT api_provider_authentication_id_201fd4b9_fk_api_provi FOREIGN KEY (authentication_id) REFERENCES public.api_providerauthentication(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: api_provider api_provider_billing_source_id_cb6b5a6f_fk_api_provi; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_provider
    ADD CONSTRAINT api_provider_billing_source_id_cb6b5a6f_fk_api_provi FOREIGN KEY (billing_source_id) REFERENCES public.api_providerbillingsource(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: api_provider api_provider_created_by_id_e740fc35_fk_api_user_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_provider
    ADD CONSTRAINT api_provider_created_by_id_e740fc35_fk_api_user_id FOREIGN KEY (created_by_id) REFERENCES public.api_user(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: api_provider api_provider_customer_id_87062290_fk_api_customer_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_provider
    ADD CONSTRAINT api_provider_customer_id_87062290_fk_api_customer_id FOREIGN KEY (customer_id) REFERENCES public.api_customer(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: api_provider api_provider_infrastructure_id_1a0be3a0_fk_api_provi; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_provider
    ADD CONSTRAINT api_provider_infrastructure_id_1a0be3a0_fk_api_provi FOREIGN KEY (infrastructure_id) REFERENCES public.api_providerinfrastructuremap(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: api_providerinfrastructuremap api_providerinfrastr_infrastructure_provi_cfa87d94_fk_api_provi; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_providerinfrastructuremap
    ADD CONSTRAINT api_providerinfrastr_infrastructure_provi_cfa87d94_fk_api_provi FOREIGN KEY (infrastructure_provider_id) REFERENCES public.api_provider(uuid) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: api_user api_user_customer_id_90bd21ef_fk_api_customer_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_user
    ADD CONSTRAINT api_user_customer_id_90bd21ef_fk_api_customer_id FOREIGN KEY (customer_id) REFERENCES public.api_customer(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_group_permissions auth_group_permissio_permission_id_84c5c92e_fk_auth_perm; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_group_permissions
    ADD CONSTRAINT auth_group_permissio_permission_id_84c5c92e_fk_auth_perm FOREIGN KEY (permission_id) REFERENCES public.auth_permission(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_group_permissions auth_group_permissions_group_id_b120cbf9_fk_auth_group_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_group_permissions
    ADD CONSTRAINT auth_group_permissions_group_id_b120cbf9_fk_auth_group_id FOREIGN KEY (group_id) REFERENCES public.auth_group(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_permission auth_permission_content_type_id_2f476e4b_fk_django_co; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_permission
    ADD CONSTRAINT auth_permission_content_type_id_2f476e4b_fk_django_co FOREIGN KEY (content_type_id) REFERENCES public.django_content_type(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_user_groups auth_user_groups_group_id_97559544_fk_auth_group_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user_groups
    ADD CONSTRAINT auth_user_groups_group_id_97559544_fk_auth_group_id FOREIGN KEY (group_id) REFERENCES public.auth_group(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_user_groups auth_user_groups_user_id_6a12ed8b_fk_auth_user_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user_groups
    ADD CONSTRAINT auth_user_groups_user_id_6a12ed8b_fk_auth_user_id FOREIGN KEY (user_id) REFERENCES public.auth_user(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_user_user_permissions auth_user_user_permi_permission_id_1fbb5f2c_fk_auth_perm; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user_user_permissions
    ADD CONSTRAINT auth_user_user_permi_permission_id_1fbb5f2c_fk_auth_perm FOREIGN KEY (permission_id) REFERENCES public.auth_permission(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: auth_user_user_permissions auth_user_user_permissions_user_id_a95ead1b_fk_auth_user_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_user_user_permissions
    ADD CONSTRAINT auth_user_user_permissions_user_id_a95ead1b_fk_auth_user_id FOREIGN KEY (user_id) REFERENCES public.auth_user(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: reporting_common_costusagereportstatus reporting_common_cos_manifest_id_62ef64b9_fk_reporting; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reporting_common_costusagereportstatus
    ADD CONSTRAINT reporting_common_cos_manifest_id_62ef64b9_fk_reporting FOREIGN KEY (manifest_id) REFERENCES public.reporting_common_costusagereportmanifest(id) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: reporting_common_costusagereportmanifest reporting_common_cos_provider_id_6abb15de_fk_api_provi; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reporting_common_costusagereportmanifest
    ADD CONSTRAINT reporting_common_cos_provider_id_6abb15de_fk_api_provi FOREIGN KEY (provider_id) REFERENCES public.api_provider(uuid) DEFERRABLE INITIALLY DEFERRED;


--
-- Name: txn_components txn_components_tc_txnid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.txn_components
    ADD CONSTRAINT txn_components_tc_txnid_fkey FOREIGN KEY (tc_txnid) REFERENCES public.txns(txn_id);


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: pg_database_owner
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

--
-- Database "unleash" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 16.9 (Debian 16.9-1.pgdg120+1)
-- Dumped by pg_dump version 16.9 (Debian 16.9-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: unleash; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE unleash WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE_PROVIDER = libc LOCALE = 'en_US.utf8';


ALTER DATABASE unleash OWNER TO postgres;

\connect unleash

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: assign_unleash_permission_to_role(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.assign_unleash_permission_to_role(permission_name text, role_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
    var_role_id int;
    var_permission text;
BEGIN
    var_role_id := (SELECT r.id FROM roles r WHERE r.name = role_name);
    var_permission := (SELECT p.permission FROM permissions p WHERE p.permission = permission_name);

    IF NOT EXISTS (
        SELECT 1
        FROM role_permission AS rp
        WHERE rp.role_id = var_role_id AND rp.permission = var_permission
    ) THEN
        INSERT INTO role_permission(role_id, permission) VALUES (var_role_id, var_permission);
    END IF;
END
$$;


ALTER FUNCTION public.assign_unleash_permission_to_role(permission_name text, role_name text) OWNER TO postgres;

--
-- Name: assign_unleash_permission_to_role_for_all_environments(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.assign_unleash_permission_to_role_for_all_environments(permission_name text, role_name text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
    var_role_id int;
    var_permission text;
BEGIN
    var_role_id := (SELECT id FROM roles r WHERE r.name = role_name);
    var_permission := (SELECT p.permission FROM permissions p WHERE p.permission = permission_name);

    INSERT INTO role_permission (role_id, permission, environment)
        SELECT var_role_id, var_permission, e.name
        FROM environments e
        WHERE NOT EXISTS (
            SELECT 1
            FROM role_permission rp
            WHERE rp.role_id = var_role_id
            AND rp.permission = var_permission
            AND rp.environment = e.name
        );
END;
$$;


ALTER FUNCTION public.assign_unleash_permission_to_role_for_all_environments(permission_name text, role_name text) OWNER TO postgres;

--
-- Name: unleash_update_stat_environment_changes_counter(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.unleash_update_stat_environment_changes_counter() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
        BEGIN
            IF NEW.environment IS NOT NULL THEN
                INSERT INTO stat_environment_updates(day, environment, updates) SELECT DATE_TRUNC('Day', NEW.created_at), NEW.environment, 1 ON CONFLICT (day, environment) DO UPDATE SET updates = stat_environment_updates.updates + 1;
            END IF;

            return null;
        END;
    $$;


ALTER FUNCTION public.unleash_update_stat_environment_changes_counter() OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: addons; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.addons (
    id integer NOT NULL,
    provider text NOT NULL,
    description text,
    enabled boolean DEFAULT true,
    parameters json,
    events json,
    created_at timestamp with time zone DEFAULT now(),
    projects jsonb DEFAULT '[]'::jsonb,
    environments jsonb DEFAULT '[]'::jsonb
);


ALTER TABLE public.addons OWNER TO postgres;

--
-- Name: addons_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.addons_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.addons_id_seq OWNER TO postgres;

--
-- Name: addons_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.addons_id_seq OWNED BY public.addons.id;


--
-- Name: api_token_project; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_token_project (
    secret text NOT NULL,
    project text NOT NULL
);


ALTER TABLE public.api_token_project OWNER TO postgres;

--
-- Name: api_tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.api_tokens (
    secret text NOT NULL,
    username text NOT NULL,
    type text NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    expires_at timestamp with time zone,
    seen_at timestamp with time zone,
    environment character varying,
    alias text,
    token_name text,
    created_by_user_id integer
);


ALTER TABLE public.api_tokens OWNER TO postgres;

--
-- Name: banners; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.banners (
    id integer NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    message text NOT NULL,
    variant text,
    sticky boolean DEFAULT false,
    icon text,
    link text,
    link_text text,
    dialog_title text,
    dialog text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.banners OWNER TO postgres;

--
-- Name: change_request_approvals; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.change_request_approvals (
    id integer NOT NULL,
    change_request_id integer NOT NULL,
    created_by integer NOT NULL,
    created_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.change_request_approvals OWNER TO postgres;

--
-- Name: change_request_approvals_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.change_request_approvals_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.change_request_approvals_id_seq OWNER TO postgres;

--
-- Name: change_request_approvals_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.change_request_approvals_id_seq OWNED BY public.change_request_approvals.id;


--
-- Name: change_request_comments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.change_request_comments (
    id integer NOT NULL,
    change_request integer NOT NULL,
    text text NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    created_by integer NOT NULL
);


ALTER TABLE public.change_request_comments OWNER TO postgres;

--
-- Name: change_request_comments_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.change_request_comments_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.change_request_comments_id_seq OWNER TO postgres;

--
-- Name: change_request_comments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.change_request_comments_id_seq OWNED BY public.change_request_comments.id;


--
-- Name: change_request_events; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.change_request_events (
    id integer NOT NULL,
    feature character varying(255),
    action character varying(255) NOT NULL,
    payload jsonb DEFAULT '[]'::jsonb NOT NULL,
    created_by integer NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    change_request_id integer NOT NULL
);


ALTER TABLE public.change_request_events OWNER TO postgres;

--
-- Name: change_request_events_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.change_request_events_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.change_request_events_id_seq OWNER TO postgres;

--
-- Name: change_request_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.change_request_events_id_seq OWNED BY public.change_request_events.id;


--
-- Name: change_request_rejections; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.change_request_rejections (
    id integer NOT NULL,
    change_request_id integer NOT NULL,
    created_by integer NOT NULL,
    created_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.change_request_rejections OWNER TO postgres;

--
-- Name: change_request_rejections_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.change_request_rejections_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.change_request_rejections_id_seq OWNER TO postgres;

--
-- Name: change_request_rejections_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.change_request_rejections_id_seq OWNED BY public.change_request_rejections.id;


--
-- Name: change_request_schedule; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.change_request_schedule (
    change_request integer NOT NULL,
    scheduled_at timestamp without time zone NOT NULL,
    created_by integer,
    status text,
    failure_reason text
);


ALTER TABLE public.change_request_schedule OWNER TO postgres;

--
-- Name: change_request_settings; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.change_request_settings (
    project character varying(255) NOT NULL,
    environment character varying(100) NOT NULL,
    required_approvals integer DEFAULT 1
);


ALTER TABLE public.change_request_settings OWNER TO postgres;

--
-- Name: change_requests; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.change_requests (
    id integer NOT NULL,
    environment character varying(100),
    state character varying(255) NOT NULL,
    project character varying(255),
    created_by integer NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    min_approvals integer DEFAULT 1,
    title text
);


ALTER TABLE public.change_requests OWNER TO postgres;

--
-- Name: change_requests_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.change_requests_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.change_requests_id_seq OWNER TO postgres;

--
-- Name: change_requests_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.change_requests_id_seq OWNED BY public.change_requests.id;


--
-- Name: client_applications; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.client_applications (
    app_name character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    seen_at timestamp with time zone,
    strategies json,
    description character varying(255),
    icon character varying(255),
    url character varying(255),
    color character varying(255),
    announced boolean DEFAULT false,
    created_by text
);


ALTER TABLE public.client_applications OWNER TO postgres;

--
-- Name: client_applications_usage; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.client_applications_usage (
    app_name character varying(255) NOT NULL,
    project character varying(255) NOT NULL,
    environment character varying(100) NOT NULL
);


ALTER TABLE public.client_applications_usage OWNER TO postgres;

--
-- Name: client_instances; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.client_instances (
    app_name character varying(255) NOT NULL,
    instance_id character varying(255) NOT NULL,
    client_ip character varying(255),
    last_seen timestamp with time zone DEFAULT now(),
    created_at timestamp with time zone DEFAULT now(),
    sdk_version character varying(255),
    environment character varying(255) DEFAULT 'default'::character varying NOT NULL
);


ALTER TABLE public.client_instances OWNER TO postgres;

--
-- Name: client_metrics_env; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.client_metrics_env (
    feature_name character varying(255) NOT NULL,
    app_name character varying(255) NOT NULL,
    environment character varying(100) NOT NULL,
    "timestamp" timestamp with time zone NOT NULL,
    yes bigint DEFAULT 0,
    no bigint DEFAULT 0
);


ALTER TABLE public.client_metrics_env OWNER TO postgres;

--
-- Name: client_metrics_env_variants; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.client_metrics_env_variants (
    feature_name character varying(255) NOT NULL,
    app_name character varying(255) NOT NULL,
    environment character varying(100) NOT NULL,
    "timestamp" timestamp with time zone NOT NULL,
    variant text NOT NULL,
    count integer DEFAULT 0
);


ALTER TABLE public.client_metrics_env_variants OWNER TO postgres;

--
-- Name: context_fields; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.context_fields (
    name character varying(255) NOT NULL,
    description text,
    sort_order integer DEFAULT 10,
    legal_values json,
    created_at timestamp without time zone DEFAULT now(),
    updated_at timestamp without time zone DEFAULT now(),
    stickiness boolean DEFAULT false
);


ALTER TABLE public.context_fields OWNER TO postgres;

--
-- Name: dependent_features; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.dependent_features (
    parent character varying(255) NOT NULL,
    child character varying(255) NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    variants jsonb DEFAULT '[]'::jsonb NOT NULL
);


ALTER TABLE public.dependent_features OWNER TO postgres;

--
-- Name: environments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.environments (
    name character varying(100) NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    sort_order integer DEFAULT 9999,
    type text NOT NULL,
    enabled boolean DEFAULT true,
    protected boolean DEFAULT false
);


ALTER TABLE public.environments OWNER TO postgres;

--
-- Name: events; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.events (
    id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    type character varying(255) NOT NULL,
    created_by character varying(255) NOT NULL,
    data json,
    tags json DEFAULT '[]'::json,
    project text,
    environment text,
    feature_name text,
    pre_data jsonb,
    announced boolean DEFAULT false NOT NULL,
    created_by_user_id integer
);


ALTER TABLE public.events OWNER TO postgres;

--
-- Name: events_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.events_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.events_id_seq OWNER TO postgres;

--
-- Name: events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.events_id_seq OWNED BY public.events.id;


--
-- Name: favorite_features; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.favorite_features (
    feature character varying(255) NOT NULL,
    user_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.favorite_features OWNER TO postgres;

--
-- Name: favorite_projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.favorite_projects (
    project character varying(255) NOT NULL,
    user_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.favorite_projects OWNER TO postgres;

--
-- Name: feature_environments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.feature_environments (
    environment character varying(100) DEFAULT 'default'::character varying NOT NULL,
    feature_name character varying(255) NOT NULL,
    enabled boolean NOT NULL,
    variants jsonb DEFAULT '[]'::jsonb NOT NULL,
    last_seen_at timestamp with time zone
);


ALTER TABLE public.feature_environments OWNER TO postgres;

--
-- Name: feature_strategies; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.feature_strategies (
    id text NOT NULL,
    feature_name character varying(255) NOT NULL,
    project_name character varying(255) NOT NULL,
    environment character varying(100) DEFAULT 'default'::character varying NOT NULL,
    strategy_name character varying(255) NOT NULL,
    parameters jsonb DEFAULT '{}'::jsonb NOT NULL,
    constraints jsonb,
    sort_order integer DEFAULT 9999 NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    title text,
    disabled boolean DEFAULT false,
    variants jsonb DEFAULT '[]'::jsonb NOT NULL,
    created_by_user_id integer
);


ALTER TABLE public.feature_strategies OWNER TO postgres;

--
-- Name: feature_strategy_segment; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.feature_strategy_segment (
    feature_strategy_id text NOT NULL,
    segment_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.feature_strategy_segment OWNER TO postgres;

--
-- Name: feature_tag; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.feature_tag (
    feature_name character varying(255) NOT NULL,
    tag_type text NOT NULL,
    tag_value text NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    created_by_user_id integer
);


ALTER TABLE public.feature_tag OWNER TO postgres;

--
-- Name: feature_types; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.feature_types (
    id character varying(255) NOT NULL,
    name character varying NOT NULL,
    description character varying,
    lifetime_days integer,
    created_at timestamp with time zone DEFAULT now(),
    created_by_user_id integer
);


ALTER TABLE public.feature_types OWNER TO postgres;

--
-- Name: features; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.features (
    created_at timestamp with time zone DEFAULT now(),
    name character varying(255) NOT NULL,
    description text,
    archived boolean DEFAULT false,
    variants json DEFAULT '[]'::json,
    type character varying DEFAULT 'release'::character varying,
    stale boolean DEFAULT false,
    project character varying DEFAULT 'default'::character varying,
    last_seen_at timestamp with time zone,
    impression_data boolean DEFAULT false,
    archived_at timestamp with time zone,
    potentially_stale boolean,
    created_by_user_id integer
);


ALTER TABLE public.features OWNER TO postgres;

--
-- Name: features_view; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.features_view AS
 SELECT features.name,
    features.description,
    features.type,
    features.project,
    features.stale,
    features.impression_data,
    features.created_at,
    features.archived_at,
    features.last_seen_at,
    feature_environments.last_seen_at AS env_last_seen_at,
    feature_environments.enabled,
    feature_environments.environment,
    feature_environments.variants,
    environments.name AS environment_name,
    environments.type AS environment_type,
    environments.sort_order AS environment_sort_order,
    feature_strategies.id AS strategy_id,
    feature_strategies.strategy_name,
    feature_strategies.parameters,
    feature_strategies.constraints,
    feature_strategies.sort_order,
    fss.segment_id AS segments,
    feature_strategies.title AS strategy_title,
    feature_strategies.disabled AS strategy_disabled,
    feature_strategies.variants AS strategy_variants
   FROM ((((public.features
     LEFT JOIN public.feature_environments ON (((feature_environments.feature_name)::text = (features.name)::text)))
     LEFT JOIN public.feature_strategies ON ((((feature_strategies.feature_name)::text = (feature_environments.feature_name)::text) AND ((feature_strategies.environment)::text = (feature_environments.environment)::text))))
     LEFT JOIN public.environments ON (((feature_environments.environment)::text = (environments.name)::text)))
     LEFT JOIN public.feature_strategy_segment fss ON ((fss.feature_strategy_id = feature_strategies.id)));


ALTER VIEW public.features_view OWNER TO postgres;

--
-- Name: feedback; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.feedback (
    id integer NOT NULL,
    category text NOT NULL,
    user_type text,
    difficulty_score integer,
    positive text,
    areas_for_improvement text,
    created_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.feedback OWNER TO postgres;

--
-- Name: feedback_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.feedback_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.feedback_id_seq OWNER TO postgres;

--
-- Name: feedback_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.feedback_id_seq OWNED BY public.feedback.id;


--
-- Name: group_role; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.group_role (
    group_id integer NOT NULL,
    role_id integer NOT NULL,
    created_by text,
    created_at timestamp with time zone DEFAULT now(),
    project text NOT NULL
);


ALTER TABLE public.group_role OWNER TO postgres;

--
-- Name: group_user; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.group_user (
    group_id integer NOT NULL,
    user_id integer NOT NULL,
    created_by text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.group_user OWNER TO postgres;

--
-- Name: groups; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.groups (
    id integer NOT NULL,
    name text NOT NULL,
    description text,
    created_by text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    mappings_sso jsonb DEFAULT '[]'::jsonb,
    root_role_id integer
);


ALTER TABLE public.groups OWNER TO postgres;

--
-- Name: groups_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.groups_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.groups_id_seq OWNER TO postgres;

--
-- Name: groups_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.groups_id_seq OWNED BY public.groups.id;


--
-- Name: incoming_webhook_tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.incoming_webhook_tokens (
    id integer NOT NULL,
    token text NOT NULL,
    name text NOT NULL,
    incoming_webhook_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by_user_id integer
);


ALTER TABLE public.incoming_webhook_tokens OWNER TO postgres;

--
-- Name: incoming_webhook_tokens_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.incoming_webhook_tokens_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.incoming_webhook_tokens_id_seq OWNER TO postgres;

--
-- Name: incoming_webhook_tokens_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.incoming_webhook_tokens_id_seq OWNED BY public.incoming_webhook_tokens.id;


--
-- Name: incoming_webhooks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.incoming_webhooks (
    id integer NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    name text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by_user_id integer
);


ALTER TABLE public.incoming_webhooks OWNER TO postgres;

--
-- Name: incoming_webhooks_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.incoming_webhooks_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.incoming_webhooks_id_seq OWNER TO postgres;

--
-- Name: incoming_webhooks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.incoming_webhooks_id_seq OWNED BY public.incoming_webhooks.id;


--
-- Name: last_seen_at_metrics; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.last_seen_at_metrics (
    feature_name character varying(255) NOT NULL,
    environment character varying(100) NOT NULL,
    last_seen_at timestamp with time zone NOT NULL
);


ALTER TABLE public.last_seen_at_metrics OWNER TO postgres;

--
-- Name: login_history; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.login_history (
    id integer NOT NULL,
    username text NOT NULL,
    auth_type text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    successful boolean NOT NULL,
    ip inet,
    failure_reason text
);


ALTER TABLE public.login_history OWNER TO postgres;

--
-- Name: login_events_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.login_events_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.login_events_id_seq OWNER TO postgres;

--
-- Name: login_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.login_events_id_seq OWNED BY public.login_history.id;


--
-- Name: message_banners_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.message_banners_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.message_banners_id_seq OWNER TO postgres;

--
-- Name: message_banners_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.message_banners_id_seq OWNED BY public.banners.id;


--
-- Name: migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.migrations (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    run_on timestamp without time zone NOT NULL
);


ALTER TABLE public.migrations OWNER TO postgres;

--
-- Name: migrations_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.migrations_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.migrations_id_seq OWNER TO postgres;

--
-- Name: migrations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.migrations_id_seq OWNED BY public.migrations.id;


--
-- Name: notifications; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.notifications (
    id integer NOT NULL,
    event_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.notifications OWNER TO postgres;

--
-- Name: notifications_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.notifications_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.notifications_id_seq OWNER TO postgres;

--
-- Name: notifications_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.notifications_id_seq OWNED BY public.notifications.id;


--
-- Name: observable_events; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.observable_events (
    id integer NOT NULL,
    payload jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    source text NOT NULL,
    source_id integer NOT NULL,
    created_by_incoming_webhook_token_id integer,
    announced boolean DEFAULT false NOT NULL
);


ALTER TABLE public.observable_events OWNER TO postgres;

--
-- Name: observable_events_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.observable_events_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.observable_events_id_seq OWNER TO postgres;

--
-- Name: observable_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.observable_events_id_seq OWNED BY public.observable_events.id;


--
-- Name: permissions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.permissions (
    id integer NOT NULL,
    permission character varying(255) NOT NULL,
    display_name text,
    type character varying(255),
    created_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.permissions OWNER TO postgres;

--
-- Name: permissions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.permissions_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.permissions_id_seq OWNER TO postgres;

--
-- Name: permissions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.permissions_id_seq OWNED BY public.permissions.id;


--
-- Name: personal_access_tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.personal_access_tokens (
    secret text NOT NULL,
    description text,
    user_id integer NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    seen_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    id integer NOT NULL
);


ALTER TABLE public.personal_access_tokens OWNER TO postgres;

--
-- Name: personal_access_tokens_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.personal_access_tokens_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.personal_access_tokens_id_seq OWNER TO postgres;

--
-- Name: personal_access_tokens_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.personal_access_tokens_id_seq OWNED BY public.personal_access_tokens.id;


--
-- Name: project_environments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.project_environments (
    project_id character varying(255) NOT NULL,
    environment_name character varying(100) NOT NULL,
    default_strategy jsonb
);


ALTER TABLE public.project_environments OWNER TO postgres;

--
-- Name: project_settings; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.project_settings (
    project character varying(255) NOT NULL,
    default_stickiness character varying(100),
    project_mode character varying(100) DEFAULT 'open'::character varying NOT NULL,
    feature_limit integer,
    feature_naming_pattern text,
    feature_naming_example text,
    feature_naming_description text,
    CONSTRAINT project_settings_project_mode_values CHECK (((project_mode)::text = ANY ((ARRAY['open'::character varying, 'protected'::character varying, 'private'::character varying])::text[])))
);


ALTER TABLE public.project_settings OWNER TO postgres;

--
-- Name: project_stats; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.project_stats (
    project character varying(255) NOT NULL,
    avg_time_to_prod_current_window double precision DEFAULT 0,
    project_changes_current_window integer DEFAULT 0,
    project_changes_past_window integer DEFAULT 0,
    features_created_current_window integer DEFAULT 0,
    features_created_past_window integer DEFAULT 0,
    features_archived_current_window integer DEFAULT 0,
    features_archived_past_window integer DEFAULT 0,
    project_members_added_current_window integer DEFAULT 0
);


ALTER TABLE public.project_stats OWNER TO postgres;

--
-- Name: projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.projects (
    id character varying(255) NOT NULL,
    name character varying NOT NULL,
    description character varying,
    created_at timestamp without time zone DEFAULT now(),
    health integer DEFAULT 100,
    updated_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.projects OWNER TO postgres;

--
-- Name: public_signup_tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.public_signup_tokens (
    secret text NOT NULL,
    name text,
    expires_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by text,
    role_id integer NOT NULL,
    url text,
    enabled boolean DEFAULT true
);


ALTER TABLE public.public_signup_tokens OWNER TO postgres;

--
-- Name: public_signup_tokens_user; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.public_signup_tokens_user (
    secret text NOT NULL,
    user_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.public_signup_tokens_user OWNER TO postgres;

--
-- Name: reset_tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.reset_tokens (
    reset_token text NOT NULL,
    user_id integer,
    expires_at timestamp with time zone NOT NULL,
    used_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now(),
    created_by text
);


ALTER TABLE public.reset_tokens OWNER TO postgres;

--
-- Name: role_permission; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.role_permission (
    role_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    permission_id integer,
    environment character varying(100),
    permission text,
    created_by_user_id integer
);


ALTER TABLE public.role_permission OWNER TO postgres;

--
-- Name: role_user; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.role_user (
    role_id integer NOT NULL,
    user_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    project character varying(255) NOT NULL,
    created_by_user_id integer
);


ALTER TABLE public.role_user OWNER TO postgres;

--
-- Name: roles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.roles (
    id integer NOT NULL,
    name text NOT NULL,
    description text,
    type text DEFAULT 'custom'::text NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone,
    created_by_user_id integer
);


ALTER TABLE public.roles OWNER TO postgres;

--
-- Name: roles_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.roles_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.roles_id_seq OWNER TO postgres;

--
-- Name: roles_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.roles_id_seq OWNED BY public.roles.id;


--
-- Name: segments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.segments (
    id integer NOT NULL,
    name text NOT NULL,
    description text,
    created_by text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    constraints jsonb DEFAULT '[]'::jsonb NOT NULL,
    segment_project_id character varying(255)
);


ALTER TABLE public.segments OWNER TO postgres;

--
-- Name: segments_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.segments_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.segments_id_seq OWNER TO postgres;

--
-- Name: segments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.segments_id_seq OWNED BY public.segments.id;


--
-- Name: settings; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.settings (
    name character varying(255) NOT NULL,
    content json
);


ALTER TABLE public.settings OWNER TO postgres;

--
-- Name: stat_environment_updates; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.stat_environment_updates (
    day date NOT NULL,
    environment text NOT NULL,
    updates bigint DEFAULT 0 NOT NULL
);


ALTER TABLE public.stat_environment_updates OWNER TO postgres;

--
-- Name: strategies; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.strategies (
    created_at timestamp with time zone DEFAULT now(),
    name character varying(255) NOT NULL,
    description text,
    parameters json,
    built_in integer DEFAULT 0,
    deprecated boolean DEFAULT false,
    sort_order integer DEFAULT 9999,
    display_name text,
    title text
);


ALTER TABLE public.strategies OWNER TO postgres;

--
-- Name: tag_types; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tag_types (
    name text NOT NULL,
    description text,
    icon text,
    created_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.tag_types OWNER TO postgres;

--
-- Name: tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tags (
    type text NOT NULL,
    value text NOT NULL,
    created_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.tags OWNER TO postgres;

--
-- Name: unleash_session; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.unleash_session (
    sid character varying NOT NULL,
    sess json NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    expired timestamp with time zone NOT NULL
);


ALTER TABLE public.unleash_session OWNER TO postgres;

--
-- Name: user_feedback; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.user_feedback (
    user_id integer NOT NULL,
    feedback_id text NOT NULL,
    given timestamp with time zone,
    nevershow boolean DEFAULT false NOT NULL
);


ALTER TABLE public.user_feedback OWNER TO postgres;

--
-- Name: user_notifications; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.user_notifications (
    notification_id integer NOT NULL,
    user_id integer NOT NULL,
    read_at timestamp with time zone
);


ALTER TABLE public.user_notifications OWNER TO postgres;

--
-- Name: user_splash; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.user_splash (
    user_id integer NOT NULL,
    splash_id text NOT NULL,
    seen boolean DEFAULT false NOT NULL
);


ALTER TABLE public.user_splash OWNER TO postgres;

--
-- Name: users; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.users (
    id integer NOT NULL,
    name character varying(255),
    username character varying(255),
    email character varying(255),
    image_url character varying(255),
    password_hash character varying(255),
    login_attempts integer DEFAULT 0,
    created_at timestamp without time zone DEFAULT now(),
    seen_at timestamp without time zone,
    settings json,
    permissions json DEFAULT '[]'::json,
    deleted_at timestamp with time zone,
    is_service boolean DEFAULT false,
    created_by_user_id integer,
    is_system boolean DEFAULT false NOT NULL
);


ALTER TABLE public.users OWNER TO postgres;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.users_id_seq OWNER TO postgres;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: addons id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.addons ALTER COLUMN id SET DEFAULT nextval('public.addons_id_seq'::regclass);


--
-- Name: banners id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.banners ALTER COLUMN id SET DEFAULT nextval('public.message_banners_id_seq'::regclass);


--
-- Name: change_request_approvals id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_approvals ALTER COLUMN id SET DEFAULT nextval('public.change_request_approvals_id_seq'::regclass);


--
-- Name: change_request_comments id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_comments ALTER COLUMN id SET DEFAULT nextval('public.change_request_comments_id_seq'::regclass);


--
-- Name: change_request_events id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_events ALTER COLUMN id SET DEFAULT nextval('public.change_request_events_id_seq'::regclass);


--
-- Name: change_request_rejections id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_rejections ALTER COLUMN id SET DEFAULT nextval('public.change_request_rejections_id_seq'::regclass);


--
-- Name: change_requests id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_requests ALTER COLUMN id SET DEFAULT nextval('public.change_requests_id_seq'::regclass);


--
-- Name: events id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.events ALTER COLUMN id SET DEFAULT nextval('public.events_id_seq'::regclass);


--
-- Name: feedback id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feedback ALTER COLUMN id SET DEFAULT nextval('public.feedback_id_seq'::regclass);


--
-- Name: groups id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.groups ALTER COLUMN id SET DEFAULT nextval('public.groups_id_seq'::regclass);


--
-- Name: incoming_webhook_tokens id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.incoming_webhook_tokens ALTER COLUMN id SET DEFAULT nextval('public.incoming_webhook_tokens_id_seq'::regclass);


--
-- Name: incoming_webhooks id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.incoming_webhooks ALTER COLUMN id SET DEFAULT nextval('public.incoming_webhooks_id_seq'::regclass);


--
-- Name: login_history id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.login_history ALTER COLUMN id SET DEFAULT nextval('public.login_events_id_seq'::regclass);


--
-- Name: migrations id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.migrations ALTER COLUMN id SET DEFAULT nextval('public.migrations_id_seq'::regclass);


--
-- Name: notifications id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.notifications ALTER COLUMN id SET DEFAULT nextval('public.notifications_id_seq'::regclass);


--
-- Name: observable_events id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.observable_events ALTER COLUMN id SET DEFAULT nextval('public.observable_events_id_seq'::regclass);


--
-- Name: permissions id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.permissions ALTER COLUMN id SET DEFAULT nextval('public.permissions_id_seq'::regclass);


--
-- Name: personal_access_tokens id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.personal_access_tokens ALTER COLUMN id SET DEFAULT nextval('public.personal_access_tokens_id_seq'::regclass);


--
-- Name: roles id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.roles ALTER COLUMN id SET DEFAULT nextval('public.roles_id_seq'::regclass);


--
-- Name: segments id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.segments ALTER COLUMN id SET DEFAULT nextval('public.segments_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Data for Name: addons; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.addons (id, provider, description, enabled, parameters, events, created_at, projects, environments) FROM stdin;
\.


--
-- Data for Name: api_token_project; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_token_project (secret, project) FROM stdin;
\.


--
-- Data for Name: api_tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.api_tokens (secret, username, type, created_at, expires_at, seen_at, environment, alias, token_name, created_by_user_id) FROM stdin;
*:*.dbffffc83b1f92eeaf133a7eb878d4c58231acc159b5e1478ce53cfc	admin	admin	2025-05-09 21:37:54.472109+00	\N	\N	\N	\N	admin	\N
*:development.dbffffc83b1f92eeaf133a7eb878d4c58231acc159b5e1478ce53cfc	admin	client	2025-05-09 21:37:54.4725+00	\N	2025-05-09 22:19:54.287+00	development	\N	admin	\N
\.


--
-- Data for Name: banners; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.banners (id, enabled, message, variant, sticky, icon, link, link_text, dialog_title, dialog, created_at) FROM stdin;
\.


--
-- Data for Name: change_request_approvals; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.change_request_approvals (id, change_request_id, created_by, created_at) FROM stdin;
\.


--
-- Data for Name: change_request_comments; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.change_request_comments (id, change_request, text, created_at, created_by) FROM stdin;
\.


--
-- Data for Name: change_request_events; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.change_request_events (id, feature, action, payload, created_by, created_at, change_request_id) FROM stdin;
\.


--
-- Data for Name: change_request_rejections; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.change_request_rejections (id, change_request_id, created_by, created_at) FROM stdin;
\.


--
-- Data for Name: change_request_schedule; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.change_request_schedule (change_request, scheduled_at, created_by, status, failure_reason) FROM stdin;
\.


--
-- Data for Name: change_request_settings; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.change_request_settings (project, environment, required_approvals) FROM stdin;
\.


--
-- Data for Name: change_requests; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.change_requests (id, environment, state, project, created_by, created_at, min_approvals, title) FROM stdin;
\.


--
-- Data for Name: client_applications; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.client_applications (app_name, created_at, updated_at, seen_at, strategies, description, icon, url, color, announced, created_by) FROM stdin;
Cost Management	2025-05-09 21:42:19.302437+00	2025-05-09 21:42:19.301+00	2025-05-09 21:42:19.301+00	["applicationHostname","default","gradualRolloutRandom","gradualRolloutSessionId","gradualRolloutUserId","remoteAddress","userWithId","flexibleRollout"]	\N	\N	\N	\N	t	::ffff:172.18.0.9
\.


--
-- Data for Name: client_applications_usage; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.client_applications_usage (app_name, project, environment) FROM stdin;
Cost Management	default	development
\.


--
-- Data for Name: client_instances; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.client_instances (app_name, instance_id, client_ip, last_seen, created_at, sdk_version, environment) FROM stdin;
Cost Management	unleash-client-python	::ffff:172.18.0.9	2025-05-09 22:21:14.937+00	2025-05-09 21:42:19.316119+00	unleash-client-python:4.4.1	development
\.


--
-- Data for Name: client_metrics_env; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.client_metrics_env (feature_name, app_name, environment, "timestamp", yes, no) FROM stdin;
\.


--
-- Data for Name: client_metrics_env_variants; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.client_metrics_env_variants (feature_name, app_name, environment, "timestamp", variant, count) FROM stdin;
\.


--
-- Data for Name: context_fields; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.context_fields (name, description, sort_order, legal_values, created_at, updated_at, stickiness) FROM stdin;
environment	Allows you to constrain on application environment	0	\N	2025-05-09 21:37:52.010128	2025-05-09 21:37:52.010128	f
userId	Allows you to constrain on userId	1	\N	2025-05-09 21:37:52.010128	2025-05-09 21:37:52.010128	f
appName	Allows you to constrain on application name	2	\N	2025-05-09 21:37:52.010128	2025-05-09 21:37:52.010128	f
currentTime	Allows you to constrain on date values	3	\N	2025-05-09 21:37:52.908356	2025-05-09 21:37:52.908356	f
sessionId	Allows you to constrain on sessionId	4	\N	2025-05-09 21:37:53.555644	2025-05-09 21:37:53.555644	t
schema	Allows you to set a constraint on schema (account/org ids)	10	[]	2025-05-09 21:37:56.383707	2025-05-09 21:37:56.383707	t
source_uuid	Allows you to set a constraint on Cost Mgmt source uuid	10	[]	2025-05-09 21:37:56.383707	2025-05-09 21:37:56.383707	t
\.


--
-- Data for Name: dependent_features; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.dependent_features (parent, child, enabled, variants) FROM stdin;
\.


--
-- Data for Name: environments; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.environments (name, created_at, sort_order, type, enabled, protected) FROM stdin;
default	2025-05-09 21:37:52.641754+00	1	production	f	t
development	2025-05-09 21:37:52.668312+00	2	development	t	f
production	2025-05-09 21:37:52.668312+00	3	production	t	f
\.


--
-- Data for Name: events; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.events (id, created_at, type, created_by, data, tags, project, environment, feature_name, pre_data, announced, created_by_user_id) FROM stdin;
1	2025-05-09 21:37:51.893999+00	strategy-created	migration	{"name":"default","description":"Default on or off Strategy."}	[]	\N	\N	\N	\N	t	\N
2	2025-05-09 21:37:51.975136+00	strategy-created	migration	{"name":"userWithId","description":"Active for users with a userId defined in the userIds-list","parameters":[{"name":"userIds","type":"list","description":"","required":false}]}	[]	\N	\N	\N	\N	t	\N
3	2025-05-09 21:37:51.975136+00	strategy-created	migration	{"name":"applicationHostname","description":"Active for client instances with a hostName in the hostNames-list.","parameters":[{"name":"hostNames","type":"list","description":"List of hostnames to enable the feature toggle for.","required":false}]}	[]	\N	\N	\N	\N	t	\N
4	2025-05-09 21:37:51.975136+00	strategy-created	migration	{"name":"remoteAddress","description":"Active for remote addresses defined in the IPs list.","parameters":[{"name":"IPs","type":"list","description":"List of IPs to enable the feature toggle for.","required":true}]}	[]	\N	\N	\N	\N	t	\N
5	2025-05-09 21:37:52.003766+00	strategy-created	migration	{"name":"flexibleRollout","description":"Gradually activate feature toggle based on sane stickiness","parameters":[{"name":"rollout","type":"percentage","description":"","required":false},{"name":"stickiness","type":"string","description":"Used define stickiness. Possible values: default, userId, sessionId, random","required":true},{"name":"groupId","type":"string","description":"Used to define a activation groups, which allows you to correlate across feature toggles.","required":true}]}	[]	\N	\N	\N	\N	t	\N
6	2025-05-09 21:37:54.58111+00	drop-features	import	{"name":"all-features"}	[]	\N	\N	\N	\N	t	-1337
7	2025-05-09 21:37:54.587498+00	api-token-created	init-api-tokens	{"tokenName":"admin","type":"client","environment":"development","projects":["*"],"username":"admin","alias":null,"project":"*","createdAt":"2025-05-09T21:37:54.472Z"}	[]	*	development	\N	\N	t	-1337
8	2025-05-09 21:37:54.588468+00	api-token-created	init-api-tokens	{"tokenName":"admin","type":"admin","environment":"*","projects":["*"],"username":"admin","alias":null,"project":"*","createdAt":"2025-05-09T21:37:54.472Z"}	[]	*	*	\N	\N	t	-1337
9	2025-05-09 21:37:56.383707+00	feature-created	admin	{"name":"cost-management.backend.schema-flag-template","description":"template for schema context.","type":"permission","project":"default","stale":false,"createdAt":"2025-05-09T21:37:56.383Z","lastSeenAt":null,"impressionData":false,"archivedAt":null,"archived":false}	[]	default	\N	cost-management.backend.schema-flag-template	\N	t	1
10	2025-05-09 21:37:56.383707+00	feature-created	admin	{"name":"cost-management.backend.source-uuid-flag-template","description":"template for source-uuid context.","type":"permission","project":"default","stale":false,"createdAt":"2025-05-09T21:37:56.383Z","lastSeenAt":null,"impressionData":false,"archivedAt":null,"archived":false}	[]	default	\N	cost-management.backend.source-uuid-flag-template	\N	t	1
11	2025-05-09 21:37:56.383707+00	context-field-created	admin	{"name":"schema","description":"Allows you to set a constraint on schema (account/org ids)","legalValues":[],"stickiness":true}	[]	\N	\N	\N	\N	t	1
12	2025-05-09 21:37:56.383707+00	context-field-created	admin	{"name":"source_uuid","description":"Allows you to set a constraint on Cost Mgmt source uuid","legalValues":[],"stickiness":true}	[]	\N	\N	\N	\N	t	1
13	2025-05-09 21:37:56.383707+00	feature-strategy-add	admin	{"id":"7f3bf742-d364-42a7-a5b3-991a83f1e11c","name":"flexibleRollout","title":null,"disabled":false,"constraints":[{"values":["org1234567"],"inverted":false,"operator":"IN","contextName":"schema","caseInsensitive":false}],"parameters":{"groupId":"cost-management.backend.schema-flag-template","rollout":"100","stickiness":"default"},"variants":[],"sortOrder":0,"segments":[]}	[]	default	development	cost-management.backend.schema-flag-template	\N	t	-1337
14	2025-05-09 21:37:56.383707+00	feature-strategy-add	admin	{"id":"984342ca-f964-45a4-baae-b00818f20c54","name":"flexibleRollout","title":null,"disabled":false,"constraints":[{"values":["72d89ed3-facc-48e3-813e-872c1d1aedfe"],"inverted":false,"operator":"IN","contextName":"source_uuid","caseInsensitive":false}],"parameters":{"groupId":"cost-management.backend.source-uuid-flag-template","rollout":"100","stickiness":"default"},"variants":[],"sortOrder":0,"segments":[]}	[]	default	development	cost-management.backend.source-uuid-flag-template	\N	t	-1337
15	2025-05-09 21:37:56.383707+00	feature-environment-enabled	admin	\N	[]	default	development	cost-management.backend.schema-flag-template	\N	t	1
16	2025-05-09 21:37:56.383707+00	feature-environment-enabled	admin	\N	[]	default	development	cost-management.backend.source-uuid-flag-template	\N	t	1
17	2025-05-09 21:37:56.383707+00	features-imported	admin	\N	[]	default	development	\N	\N	t	1
18	2025-05-09 21:42:54.34909+00	application-created	::ffff:172.18.0.9	{"appName":"Cost Management","createdAt":"2025-05-09T21:42:19.302Z","updatedAt":"2025-05-09T21:42:19.301Z","description":null,"strategies":["applicationHostname","default","gradualRolloutRandom","gradualRolloutSessionId","gradualRolloutUserId","remoteAddress","userWithId","flexibleRollout"],"createdBy":"::ffff:172.18.0.9","url":null,"color":null,"icon":null}	[]	\N	\N	\N	\N	t	-1337
\.


--
-- Data for Name: favorite_features; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.favorite_features (feature, user_id, created_at) FROM stdin;
\.


--
-- Data for Name: favorite_projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.favorite_projects (project, user_id, created_at) FROM stdin;
\.


--
-- Data for Name: feature_environments; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.feature_environments (environment, feature_name, enabled, variants, last_seen_at) FROM stdin;
production	cost-management.backend.schema-flag-template	f	[]	\N
production	cost-management.backend.source-uuid-flag-template	f	[]	\N
development	cost-management.backend.schema-flag-template	t	[]	\N
development	cost-management.backend.source-uuid-flag-template	t	[]	\N
\.


--
-- Data for Name: feature_strategies; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.feature_strategies (id, feature_name, project_name, environment, strategy_name, parameters, constraints, sort_order, created_at, title, disabled, variants, created_by_user_id) FROM stdin;
7f3bf742-d364-42a7-a5b3-991a83f1e11c	cost-management.backend.schema-flag-template	default	development	flexibleRollout	{"groupId": "cost-management.backend.schema-flag-template", "rollout": "100", "stickiness": "default"}	[{"values": ["org1234567"], "inverted": false, "operator": "IN", "contextName": "schema", "caseInsensitive": false}]	0	2025-05-09 21:37:56.383707+00	\N	f	[]	\N
984342ca-f964-45a4-baae-b00818f20c54	cost-management.backend.source-uuid-flag-template	default	development	flexibleRollout	{"groupId": "cost-management.backend.source-uuid-flag-template", "rollout": "100", "stickiness": "default"}	[{"values": ["72d89ed3-facc-48e3-813e-872c1d1aedfe"], "inverted": false, "operator": "IN", "contextName": "source_uuid", "caseInsensitive": false}]	0	2025-05-09 21:37:56.383707+00	\N	f	[]	\N
\.


--
-- Data for Name: feature_strategy_segment; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.feature_strategy_segment (feature_strategy_id, segment_id, created_at) FROM stdin;
\.


--
-- Data for Name: feature_tag; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.feature_tag (feature_name, tag_type, tag_value, created_at, created_by_user_id) FROM stdin;
\.


--
-- Data for Name: feature_types; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.feature_types (id, name, description, lifetime_days, created_at, created_by_user_id) FROM stdin;
release	Release	Release feature toggles are used to release new features.	40	2025-05-09 21:37:52.110427+00	\N
experiment	Experiment	Experiment feature toggles are used to test and verify multiple different versions of a feature.	40	2025-05-09 21:37:52.110427+00	\N
operational	Operational	Operational feature toggles are used to control aspects of a rollout.	7	2025-05-09 21:37:52.110427+00	\N
kill-switch	Kill switch	Kill switch feature toggles are used to quickly turn on or off critical functionality in your system.	\N	2025-05-09 21:37:52.110427+00	\N
permission	Permission	Permission feature toggles are used to control permissions in your system.	\N	2025-05-09 21:37:52.110427+00	\N
\.


--
-- Data for Name: features; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.features (created_at, name, description, archived, variants, type, stale, project, last_seen_at, impression_data, archived_at, potentially_stale, created_by_user_id) FROM stdin;
2025-05-09 21:37:56.383707+00	cost-management.backend.schema-flag-template	template for schema context.	f	[]	permission	f	default	\N	f	\N	\N	1
2025-05-09 21:37:56.383707+00	cost-management.backend.source-uuid-flag-template	template for source-uuid context.	f	[]	permission	f	default	\N	f	\N	\N	1
\.


--
-- Data for Name: feedback; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.feedback (id, category, user_type, difficulty_score, positive, areas_for_improvement, created_at) FROM stdin;
\.


--
-- Data for Name: group_role; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.group_role (group_id, role_id, created_by, created_at, project) FROM stdin;
\.


--
-- Data for Name: group_user; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.group_user (group_id, user_id, created_by, created_at) FROM stdin;
\.


--
-- Data for Name: groups; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.groups (id, name, description, created_by, created_at, mappings_sso, root_role_id) FROM stdin;
\.


--
-- Data for Name: incoming_webhook_tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.incoming_webhook_tokens (id, token, name, incoming_webhook_id, created_at, created_by_user_id) FROM stdin;
\.


--
-- Data for Name: incoming_webhooks; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.incoming_webhooks (id, enabled, name, created_at, created_by_user_id) FROM stdin;
\.


--
-- Data for Name: last_seen_at_metrics; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.last_seen_at_metrics (feature_name, environment, last_seen_at) FROM stdin;
\.


--
-- Data for Name: login_history; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.login_history (id, username, auth_type, created_at, successful, ip, failure_reason) FROM stdin;
\.


--
-- Data for Name: migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.migrations (id, name, run_on) FROM stdin;
1	/20141020151056-initial-schema	2025-05-09 21:37:51.873
2	/20141110144153-add-description-to-features	2025-05-09 21:37:51.88
3	/20141117200435-add-parameters-template-to-strategies	2025-05-09 21:37:51.885
4	/20141117202209-insert-default-strategy	2025-05-09 21:37:51.89
5	/20141118071458-default-strategy-event	2025-05-09 21:37:51.895
6	/20141215210141-005-archived-flag-to-features	2025-05-09 21:37:51.901
7	/20150210152531-006-rename-eventtype	2025-05-09 21:37:51.906
8	/20160618193924-add-strategies-to-features	2025-05-09 21:37:51.912
9	/20161027134128-create-metrics	2025-05-09 21:37:51.928
10	/20161104074441-create-client-instances	2025-05-09 21:37:51.939
11	/20161205203516-create-client-applications	2025-05-09 21:37:51.953
12	/20161212101749-better-strategy-parameter-definitions	2025-05-09 21:37:51.965
13	/20170211085502-built-in-strategies	2025-05-09 21:37:51.972
14	/20170211090541-add-default-strategies	2025-05-09 21:37:51.981
15	/20170306233934-timestamp-with-tz	2025-05-09 21:37:51.99
16	/20170628205541-add-sdk-version-to-client-instances	2025-05-09 21:37:51.994
17	/20190123204125-add-variants-to-features	2025-05-09 21:37:52
18	/20191023184858-flexible-rollout-strategy	2025-05-09 21:37:52.007
19	/20200102184820-create-context-fields	2025-05-09 21:37:52.026
20	/20200227202711-settings	2025-05-09 21:37:52.041
21	/20200329191251-settings-secret	2025-05-09 21:37:52.046
22	/20200416201319-create-users	2025-05-09 21:37:52.074
23	/20200429175747-users-settings	2025-05-09 21:37:52.079
24	/20200805091409-add-feature-toggle-type	2025-05-09 21:37:52.096
25	/20200805094311-add-feature-type-to-features	2025-05-09 21:37:52.102
26	/20200806091734-add-stale-flag-to-features	2025-05-09 21:37:52.107
27	/20200810200901-add-created-at-to-feature-types	2025-05-09 21:37:52.111
28	/20200928194947-add-projects	2025-05-09 21:37:52.129
29	/20200928195238-add-project-id-to-features	2025-05-09 21:37:52.134
30	/20201216140726-add-last-seen-to-features	2025-05-09 21:37:52.139
31	/20210105083014-add-tag-and-tag-types	2025-05-09 21:37:52.186
32	/20210119084617-add-addon-table	2025-05-09 21:37:52.213
33	/20210121115438-add-deprecated-column-to-strategies	2025-05-09 21:37:52.221
34	/20210127094440-add-tags-column-to-events	2025-05-09 21:37:52.229
35	/20210208203708-add-stickiness-to-context	2025-05-09 21:37:52.234
36	/20210212114759-add-session-table	2025-05-09 21:37:52.257
37	/20210217195834-rbac-tables	2025-05-09 21:37:52.285
38	/20210218090213-generate-server-identifier	2025-05-09 21:37:52.289
39	/20210302080040-add-pk-to-client-instances	2025-05-09 21:37:52.298
40	/20210304115810-change-default-timestamp-to-now	2025-05-09 21:37:52.305
41	/20210304141005-add-announce-field-to-application	2025-05-09 21:37:52.31
42	/20210304150739-add-created-by-to-application	2025-05-09 21:37:52.314
43	/20210322104356-api-tokens-table	2025-05-09 21:37:52.334
44	/20210322104357-api-tokens-convert-enterprise	2025-05-09 21:37:52.339
45	/20210323073508-reset-application-announcements	2025-05-09 21:37:52.344
46	/20210409120136-create-reset-token-table	2025-05-09 21:37:52.363
47	/20210414141220-fix-misspellings-in-role-descriptions	2025-05-09 21:37:52.367
48	/20210415173116-rbac-rename-roles	2025-05-09 21:37:52.372
49	/20210421133845-add-sort-order-to-strategies	2025-05-09 21:37:52.377
50	/20210421135405-add-display-name-and-update-description-for-strategies	2025-05-09 21:37:52.383
51	/20210423103647-lowercase-all-emails	2025-05-09 21:37:52.388
52	/20210428062103-user-permission-to-rbac	2025-05-09 21:37:52.393
53	/20210428103922-patch-role-table	2025-05-09 21:37:52.399
54	/20210428103923-onboard-projects-to-rbac	2025-05-09 21:37:52.406
55	/20210428103924-patch-admin-role-name	2025-05-09 21:37:52.411
56	/20210428103924-patch-admin_role	2025-05-09 21:37:52.417
57	/20210428103924-patch-role_permissions	2025-05-09 21:37:52.422
58	/20210504101429-deprecate-strategies	2025-05-09 21:37:52.427
59	/20210520171325-update-role-descriptions	2025-05-09 21:37:52.432
60	/20210602115555-create-feedback-table	2025-05-09 21:37:52.453
61	/20210610085817-features-strategies-table	2025-05-09 21:37:52.478
62	/20210615115226-migrate-strategies-to-feature-strategies	2025-05-09 21:37:52.483
63	/20210618091331-project-environments-table	2025-05-09 21:37:52.494
64	/20210618100913-add-cascade-for-user-feedback	2025-05-09 21:37:52.501
65	/20210624114602-change-type-of-feature-archived	2025-05-09 21:37:52.517
66	/20210624114855-drop-strategies-column-from-features	2025-05-09 21:37:52.522
67	/20210624115109-drop-enabled-column-from-features	2025-05-09 21:37:52.526
68	/20210625102126-connect-default-project-to-global-environment	2025-05-09 21:37:52.532
69	/20210629130734-add-health-rating-to-project	2025-05-09 21:37:52.536
70	/20210830113948-connect-projects-to-global-envrionments	2025-05-09 21:37:52.541
71	/20210831072631-add-sort-order-and-type-to-env	2025-05-09 21:37:52.554
72	/20210907124058-add-dbcritic-indices	2025-05-09 21:37:52.597
73	/20210907124850-add-dbcritic-primary-keys	2025-05-09 21:37:52.606
74	/20210908100701-add-enabled-to-environments	2025-05-09 21:37:52.611
75	/20210909085651-add-protected-field-to-environments	2025-05-09 21:37:52.62
76	/20210913103159-api-keys-scoping	2025-05-09 21:37:52.625
77	/20210915122001-add-project-and-environment-columns-to-events	2025-05-09 21:37:52.639
78	/20210920104218-rename-global-env-to-default-env	2025-05-09 21:37:52.645
79	/20210921105032-client-api-tokens-default	2025-05-09 21:37:52.65
80	/20210922084509-add-non-null-constraint-to-environment-type	2025-05-09 21:37:52.655
81	/20210922120521-add-tag-type-permission	2025-05-09 21:37:52.66
82	/20210928065411-remove-displayname-from-environments	2025-05-09 21:37:52.664
83	/20210928080601-add-development-and-production-environments	2025-05-09 21:37:52.67
84	/20210928082228-connect-default-environment-to-all-existing-projects	2025-05-09 21:37:52.675
85	/20211004104917-client-metrics-env	2025-05-09 21:37:52.693
86	/20211011094226-add-environment-to-client-instances	2025-05-09 21:37:52.708
87	/20211013093114-feature-strategies-parameters-not-null	2025-05-09 21:37:52.712
88	/20211029094324-set-sort-order-env	2025-05-09 21:37:52.72
89	/20211105104316-add-feature-name-column-to-events	2025-05-09 21:37:52.732
90	/20211105105509-add-predata-column-to-events	2025-05-09 21:37:52.739
91	/20211108130333-create-user-splash-table	2025-05-09 21:37:52.759
92	/20211109103930-add-splash-entry-for-users	2025-05-09 21:37:52.763
93	/20211126112551-disable-default-environment	2025-05-09 21:37:52.769
94	/20211130142314-add-updated-at-to-projects	2025-05-09 21:37:52.774
95	/20211202120808-add-custom-roles	2025-05-09 21:37:52.804
96	/20211209205201-drop-client-metrics	2025-05-09 21:37:52.812
97	/20220103134659-add-permissions-to-project-roles	2025-05-09 21:37:52.82
98	/20220103143843-add-permissions-to-editor-role	2025-05-09 21:37:52.825
99	/20220111112804-update-permission-descriptions	2025-05-09 21:37:52.831
100	/20220111115613-move-feature-toggle-permission	2025-05-09 21:37:52.837
101	/20220111120346-roles-unique-name	2025-05-09 21:37:52.847
102	/20220111121010-update-project-for-editor-role	2025-05-09 21:37:52.853
103	/20220111125620-role-permission-empty-string-for-non-environment-type	2025-05-09 21:37:52.859
104	/20220119182603-update-toggle-types-description	2025-05-09 21:37:52.864
105	/20220125200908-convert-old-feature-events	2025-05-09 21:37:52.869
106	/20220128081242-add-impressiondata-to-features	2025-05-09 21:37:52.874
107	/20220129113106-metrics-counters-as-bigint	2025-05-09 21:37:52.895
108	/20220131082150-reset-feedback-form	2025-05-09 21:37:52.901
109	/20220224081422-remove-project-column-from-roles	2025-05-09 21:37:52.905
110	/20220224111626-add-current-time-context-field	2025-05-09 21:37:52.909
111	/20220307130902-add-segments	2025-05-09 21:37:52.942
112	/20220331085057-add-api-link-table	2025-05-09 21:37:52.956
113	/20220405103233-add-segments-name-index	2025-05-09 21:37:52.967
114	/20220408081222-clean-up-duplicate-foreign-key-role-permission	2025-05-09 21:37:52.972
115	/20220411103724-add-legal-value-description	2025-05-09 21:37:52.992
116	/20220425090847-add-token-permission	2025-05-09 21:37:52.997
117	/20220511111823-patch-broken-feature-strategies	2025-05-09 21:37:53.003
118	/20220511124923-fix-patch-broken-feature-strategies	2025-05-09 21:37:53.007
119	/20220528143630-dont-cascade-environment-deletion-to-apitokens	2025-05-09 21:37:53.019
120	/20220603081324-add-archive-at-to-feature-toggle	2025-05-09 21:37:53.025
121	/20220704115624-add-user-groups	2025-05-09 21:37:53.066
122	/20220711084613-add-projects-and-environments-for-addons	2025-05-09 21:37:53.071
123	/20220808084524-add-group-permissions	2025-05-09 21:37:53.076
124	/20220808110415-add-projects-foreign-key	2025-05-09 21:37:53.082
125	/20220816121136-add-metadata-to-api-keys	2025-05-09 21:37:53.088
126	/20220817130250-alter-api-tokens	2025-05-09 21:37:53.092
127	/20220908093515-add-public-signup-tokens	2025-05-09 21:37:53.12
128	/20220912165344-pat-tokens	2025-05-09 21:37:53.135
129	/20220916093515-add-url-to-public-signup-tokens	2025-05-09 21:37:53.139
130	/20220927110212-add-enabled-to-public-signup-tokens	2025-05-09 21:37:53.144
131	/20221010114644-pat-auto-increment	2025-05-09 21:37:53.177
132	/20221011155007-add-user-groups-mappings	2025-05-09 21:37:53.182
133	/20221103111940-fix-migrations	2025-05-09 21:37:53.189
134	/20221103112200-change-request	2025-05-09 21:37:53.225
135	/20221103125732-change-request-remove-unique	2025-05-09 21:37:53.232
136	/20221104123349-change-request-approval	2025-05-09 21:37:53.243
137	/20221107121635-move-variants-to-per-environment	2025-05-09 21:37:53.259
138	/20221107132528-change-request-project-options	2025-05-09 21:37:53.265
139	/20221108114358-add-change-request-permissions	2025-05-09 21:37:53.27
140	/20221110104933-add-change-request-settings	2025-05-09 21:37:53.28
141	/20221110144113-revert-change-request-project-options	2025-05-09 21:37:53.285
142	/20221114150559-change-request-comments	2025-05-09 21:37:53.305
143	/20221115072335-add-required-approvals	2025-05-09 21:37:53.315
144	/20221121114357-add-permission-for-environment-variants	2025-05-09 21:37:53.321
145	/20221121133546-soft-delete-user	2025-05-09 21:37:53.328
146	/20221124123914-add-favorites	2025-05-09 21:37:53.346
147	/20221125185244-change-request-unique-approvals	2025-05-09 21:37:53.357
148	/20221128165141-change-request-min-approvals	2025-05-09 21:37:53.362
149	/20221205122253-skip-change-request	2025-05-09 21:37:53.367
150	/20221220160345-user-pat-permissions	2025-05-09 21:37:53.372
151	/20221221144132-service-account-users	2025-05-09 21:37:53.376
152	/20230125065315-project-stats-table	2025-05-09 21:37:53.388
153	/20230127111638-new-project-stats-field	2025-05-09 21:37:53.393
154	/20230130113337-revert-user-pat-permissions	2025-05-09 21:37:53.398
155	/20230208084046-project-api-token-permissions	2025-05-09 21:37:53.403
156	/20230208093627-assign-project-api-token-permissions-editor	2025-05-09 21:37:53.408
157	/20230208093750-assign-project-api-token-permissions-owner	2025-05-09 21:37:53.412
158	/20230208093942-assign-project-api-token-permissions-member	2025-05-09 21:37:53.418
159	/20230222084211-add-login-events-table	2025-05-09 21:37:53.439
160	/20230222154915-create-notiications-table	2025-05-09 21:37:53.456
161	/20230224093446-drop-createdBy-from-notifications-table	2025-05-09 21:37:53.461
162	/20230227115320-rename-login-events-table-to-sign-on-log	2025-05-09 21:37:53.466
163	/20230227120500-change-display-name-for-variants-per-env-permission	2025-05-09 21:37:53.471
164	/20230227123106-add-setting-for-sign-on-log-retention	2025-05-09 21:37:53.476
165	/20230302133740-rename-sign-on-log-table-to-login-history	2025-05-09 21:37:53.481
166	/20230306103400-add-project-column-to-segments	2025-05-09 21:37:53.488
167	/20230306103400-remove-direct-link-from-segment-permissions-to-admin	2025-05-09 21:37:53.493
168	/20230309174400-add-project-segment-permission	2025-05-09 21:37:53.497
169	/20230314131041-project-settings	2025-05-09 21:37:53.509
170	/20230316092547-remove-project-stats-column	2025-05-09 21:37:53.513
171	/20230411085947-skip-change-request-ui	2025-05-09 21:37:53.519
172	/20230412062635-add-change-request-title	2025-05-09 21:37:53.527
173	/20230412125618-add-title-to-strategy	2025-05-09 21:37:53.534
174	/20230414105818-add-root-role-to-groups	2025-05-09 21:37:53.54
175	/20230419104126-add-disabled-field-to-feature-strategy	2025-05-09 21:37:53.547
176	/20230420125500-v5-strategy-changes	2025-05-09 21:37:53.552
177	/20230420211308-update-context-fields-add-sessionId	2025-05-09 21:37:53.557
178	/20230424090942-project-default-strategy-settings	2025-05-09 21:37:53.567
179	/20230504145945-variant-metrics	2025-05-09 21:37:53.585
180	/20230510113903-fix-api-token-username-migration	2025-05-09 21:37:53.592
181	/20230615122909-fix-env-sort-order	2025-05-09 21:37:53.601
182	/20230619105029-new-fine-grained-api-token-permissions	2025-05-09 21:37:53.606
183	/20230619110243-assign-apitoken-permissions-to-rootroles	2025-05-09 21:37:53.616
184	/20230621141239-refactor-api-token-permissions	2025-05-09 21:37:53.621
185	/20230630080126-delete-deprecated-permissions	2025-05-09 21:37:53.626
186	/20230706123907-events-announced-column	2025-05-09 21:37:53.631
187	/20230711094214-add-potentially-stale-flag	2025-05-09 21:37:53.637
188	/20230711163311-project-feature-limit	2025-05-09 21:37:53.642
189	/20230712091834-strategy-variants	2025-05-09 21:37:53.649
190	/20230802092725-add-last-seen-column-to-feature-environments	2025-05-09 21:37:53.656
191	/20230802141830-add-feature-and-environment-last-seen-at-to-features-view	2025-05-09 21:37:53.663
192	/20230803061359-change-request-optional-feature	2025-05-09 21:37:53.668
193	/20230808104232-update-root-roles-descriptions	2025-05-09 21:37:53.674
194	/20230814095253-change-request-rejections	2025-05-09 21:37:53.691
195	/20230814115436-change-request-timzone-timestamps	2025-05-09 21:37:53.702
196	/20230815065908-change-request-approve-reject-permission	2025-05-09 21:37:53.707
197	/20230817095805-client-applications-usage-table	2025-05-09 21:37:53.733
198	/20230818124614-update-client-applications-usage-table	2025-05-09 21:37:53.761
199	/20230830121352-update-client-applications-usage-table	2025-05-09 21:37:53.765
200	/20230905122605-add-feature-naming-description	2025-05-09 21:37:53.776
201	/20230919104006-dependent-features	2025-05-09 21:37:53.78
202	/20230927071830-reset-pnps-feedback	2025-05-09 21:37:53.803
203	/20230927172930-events-announced-index	2025-05-09 21:37:53.806
204	/20231002122426-update-dependency-permission	2025-05-09 21:37:53.816
205	/20231003113443-last-seen-at-metrics-table	2025-05-09 21:37:53.821
206	/20231004120900-create-changes-stats-table-and-trigger	2025-05-09 21:37:53.861
207	/20231012082537-message-banners	2025-05-09 21:37:53.885
208	/20231019110154-rename-message-banners-table-to-banners	2025-05-09 21:37:53.89
209	/20231024121307-add-change-request-schedule	2025-05-09 21:37:53.9
210	/20231025093422-default-project-mode	2025-05-09 21:37:53.904
211	/20231030091931-add-created-by-and-status-change-request-schedule	2025-05-09 21:37:53.909
212	/20231103064746-change-request-schedule-change-type	2025-05-09 21:37:53.971
213	/20231121153304-add-permission-create-tag-type	2025-05-09 21:37:53.98
214	/20231122121456-dedupe-any-duplicate-permissions	2025-05-09 21:37:53.989
215	/20231123100052-drop-last-seen-foreign-key	2025-05-09 21:37:53.995
216	/20231123155649-favor-permission-name-over-id	2025-05-09 21:37:54.03
217	/20231211121444-features-created-by	2025-05-09 21:37:54.036
218	/20231211122322-feature-types-created-by	2025-05-09 21:37:54.041
219	/20231211122351-feature-tag-created-by	2025-05-09 21:37:54.049
220	/20231211122426-feature-strategies-created-by	2025-05-09 21:37:54.054
221	/20231211132341-add-created-by-to-role-permission	2025-05-09 21:37:54.058
222	/20231211133008-add-created-by-to-role-user	2025-05-09 21:37:54.063
223	/20231211133920-add-created-by-to-roles	2025-05-09 21:37:54.068
224	/20231211134130-add-created-by-to-users	2025-05-09 21:37:54.073
225	/20231211134633-add-created-by-to-apitokens	2025-05-09 21:37:54.078
226	/20231212094044-event-created-by-user-id	2025-05-09 21:37:54.089
227	/20231213111906-add-reason-to-change-request-schedule	2025-05-09 21:37:54.093
228	/20231215105713-incoming-webhooks	2025-05-09 21:37:54.13
229	/20231218165612-inc-webhook-tokens-rename-secret-to-token	2025-05-09 21:37:54.136
230	/20231219100343-rename-new-columns-to-created-by-user-id	2025-05-09 21:37:54.141
231	/20231221143955-feedback-table	2025-05-09 21:37:54.16
232	/20231222071533-unleash-system-user	2025-05-09 21:37:54.168
233	/20240102142100-incoming-webhooks-created-by	2025-05-09 21:37:54.175
234	/20240102205517-observable-events	2025-05-09 21:37:54.216
\.


--
-- Data for Name: notifications; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.notifications (id, event_id, created_at) FROM stdin;
\.


--
-- Data for Name: observable_events; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.observable_events (id, payload, created_at, source, source_id, created_by_incoming_webhook_token_id, announced) FROM stdin;
\.


--
-- Data for Name: permissions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.permissions (id, permission, display_name, type, created_at) FROM stdin;
1	ADMIN	Admin	root	2025-05-09 21:37:52.777238+00
2	CREATE_FEATURE	Create feature toggles	project	2025-05-09 21:37:52.777238+00
3	CREATE_STRATEGY	Create activation strategies	root	2025-05-09 21:37:52.777238+00
4	CREATE_ADDON	Create addons	root	2025-05-09 21:37:52.777238+00
5	DELETE_ADDON	Delete addons	root	2025-05-09 21:37:52.777238+00
6	UPDATE_ADDON	Update addons	root	2025-05-09 21:37:52.777238+00
7	UPDATE_FEATURE	Update feature toggles	project	2025-05-09 21:37:52.777238+00
8	DELETE_FEATURE	Delete feature toggles	project	2025-05-09 21:37:52.777238+00
9	UPDATE_APPLICATION	Update applications	root	2025-05-09 21:37:52.777238+00
10	UPDATE_TAG_TYPE	Update tag types	root	2025-05-09 21:37:52.777238+00
11	DELETE_TAG_TYPE	Delete tag types	root	2025-05-09 21:37:52.777238+00
12	CREATE_PROJECT	Create projects	root	2025-05-09 21:37:52.777238+00
13	UPDATE_PROJECT	Update project	project	2025-05-09 21:37:52.777238+00
14	DELETE_PROJECT	Delete project	project	2025-05-09 21:37:52.777238+00
15	UPDATE_STRATEGY	Update strategies	root	2025-05-09 21:37:52.777238+00
16	DELETE_STRATEGY	Delete strategies	root	2025-05-09 21:37:52.777238+00
17	UPDATE_CONTEXT_FIELD	Update context fields	root	2025-05-09 21:37:52.777238+00
18	CREATE_CONTEXT_FIELD	Create context fields	root	2025-05-09 21:37:52.777238+00
19	DELETE_CONTEXT_FIELD	Delete context fields	root	2025-05-09 21:37:52.777238+00
20	READ_ROLE	Read roles	root	2025-05-09 21:37:52.777238+00
25	CREATE_FEATURE_STRATEGY	Create activation strategies	environment	2025-05-09 21:37:52.777238+00
26	UPDATE_FEATURE_STRATEGY	Update activation strategies	environment	2025-05-09 21:37:52.777238+00
27	DELETE_FEATURE_STRATEGY	Delete activation strategies	environment	2025-05-09 21:37:52.777238+00
50	CREATE_CLIENT_API_TOKEN	Create CLIENT API tokens	root	2025-05-09 21:37:53.604181+00
29	UPDATE_FEATURE_VARIANTS	Create/edit variants	project	2025-05-09 21:37:52.777238+00
30	MOVE_FEATURE_TOGGLE	Change feature toggle project	project	2025-05-09 21:37:52.835506+00
31	CREATE_SEGMENT	Create segments	root	2025-05-09 21:37:52.912651+00
32	UPDATE_SEGMENT	Edit segments	root	2025-05-09 21:37:52.912651+00
33	DELETE_SEGMENT	Delete segments	root	2025-05-09 21:37:52.912651+00
42	READ_PROJECT_API_TOKEN	Read api tokens for a specific project	project	2025-05-09 21:37:53.401911+00
43	CREATE_PROJECT_API_TOKEN	Create api tokens for a specific project	project	2025-05-09 21:37:53.401911+00
44	DELETE_PROJECT_API_TOKEN	Delete api tokens for a specific project	project	2025-05-09 21:37:53.401911+00
37	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	Update variants	environment	2025-05-09 21:37:53.319181+00
28	UPDATE_FEATURE_ENVIRONMENT	Enable/disable toggles	environment	2025-05-09 21:37:52.777238+00
36	APPLY_CHANGE_REQUEST	Apply change requests	environment	2025-05-09 21:37:53.268737+00
51	UPDATE_CLIENT_API_TOKEN	Update CLIENT API tokens	root	2025-05-09 21:37:53.604181+00
45	UPDATE_PROJECT_SEGMENT	Create/edit project segment	project	2025-05-09 21:37:53.496148+00
38	SKIP_CHANGE_REQUEST	Skip change request process	environment	2025-05-09 21:37:53.365733+00
52	DELETE_CLIENT_API_TOKEN	Delete CLIENT API tokens	root	2025-05-09 21:37:53.604181+00
53	READ_CLIENT_API_TOKEN	Read CLIENT API tokens	root	2025-05-09 21:37:53.604181+00
35	APPROVE_CHANGE_REQUEST	Approve/Reject change requests	environment	2025-05-09 21:37:53.268737+00
54	CREATE_FRONTEND_API_TOKEN	Create FRONTEND API tokens	root	2025-05-09 21:37:53.604181+00
55	UPDATE_FRONTEND_API_TOKEN	Update FRONTEND API tokens	root	2025-05-09 21:37:53.604181+00
56	DELETE_FRONTEND_API_TOKEN	Delete FRONTEND API tokens	root	2025-05-09 21:37:53.604181+00
57	READ_FRONTEND_API_TOKEN	Read FRONTEND API tokens	root	2025-05-09 21:37:53.604181+00
58	UPDATE_FEATURE_DEPENDENCY	Update feature dependency	project	2025-05-09 21:37:53.813028+00
59	CREATE_TAG_TYPE	Create tag types	root	2025-05-09 21:37:53.97786+00
\.


--
-- Data for Name: personal_access_tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.personal_access_tokens (secret, description, user_id, expires_at, seen_at, created_at, id) FROM stdin;
user:6188b62f2f59348f3c195b66983147111682f4bb78a3f7ed9626bd84	admin	1	3000-01-01 00:00:00+00	2025-05-09 21:40:54.289+00	2025-05-09 21:37:56.258233+00	1
\.


--
-- Data for Name: project_environments; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.project_environments (project_id, environment_name, default_strategy) FROM stdin;
default	development	\N
default	production	\N
\.


--
-- Data for Name: project_settings; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.project_settings (project, default_stickiness, project_mode, feature_limit, feature_naming_pattern, feature_naming_example, feature_naming_description) FROM stdin;
\.


--
-- Data for Name: project_stats; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.project_stats (project, avg_time_to_prod_current_window, project_changes_current_window, project_changes_past_window, features_created_current_window, features_created_past_window, features_archived_current_window, features_archived_past_window, project_members_added_current_window) FROM stdin;
default	0	0	0	0	0	0	0	0
\.


--
-- Data for Name: projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.projects (id, name, description, created_at, health, updated_at) FROM stdin;
default	Default	Default project	2025-05-09 21:37:52.114861	100	2025-05-09 21:37:54.59+00
\.


--
-- Data for Name: public_signup_tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.public_signup_tokens (secret, name, expires_at, created_at, created_by, role_id, url, enabled) FROM stdin;
\.


--
-- Data for Name: public_signup_tokens_user; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.public_signup_tokens_user (secret, user_id, created_at) FROM stdin;
\.


--
-- Data for Name: reset_tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.reset_tokens (reset_token, user_id, expires_at, used_at, created_at, created_by) FROM stdin;
\.


--
-- Data for Name: role_permission; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.role_permission (role_id, created_at, permission_id, environment, permission, created_by_user_id) FROM stdin;
4	2025-05-09 21:37:52.816697+00	25	development	CREATE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	26	development	UPDATE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	27	development	DELETE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	28	development	UPDATE_FEATURE_ENVIRONMENT	\N
4	2025-05-09 21:37:52.816697+00	25	production	CREATE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	26	production	UPDATE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	27	production	DELETE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	28	production	UPDATE_FEATURE_ENVIRONMENT	\N
4	2025-05-09 21:37:52.816697+00	25	default	CREATE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	26	default	UPDATE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	27	default	DELETE_FEATURE_STRATEGY	\N
4	2025-05-09 21:37:52.816697+00	28	default	UPDATE_FEATURE_ENVIRONMENT	\N
5	2025-05-09 21:37:52.816697+00	25	development	CREATE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	26	development	UPDATE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	27	development	DELETE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	28	development	UPDATE_FEATURE_ENVIRONMENT	\N
5	2025-05-09 21:37:52.816697+00	25	production	CREATE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	26	production	UPDATE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	27	production	DELETE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	28	production	UPDATE_FEATURE_ENVIRONMENT	\N
5	2025-05-09 21:37:52.816697+00	25	default	CREATE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	26	default	UPDATE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	27	default	DELETE_FEATURE_STRATEGY	\N
5	2025-05-09 21:37:52.816697+00	28	default	UPDATE_FEATURE_ENVIRONMENT	\N
2	2025-05-09 21:37:52.823527+00	25	development	CREATE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	26	development	UPDATE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	27	development	DELETE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	28	development	UPDATE_FEATURE_ENVIRONMENT	\N
2	2025-05-09 21:37:52.823527+00	25	production	CREATE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	26	production	UPDATE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	27	production	DELETE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	28	production	UPDATE_FEATURE_ENVIRONMENT	\N
2	2025-05-09 21:37:52.823527+00	25	default	CREATE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	26	default	UPDATE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	27	default	DELETE_FEATURE_STRATEGY	\N
2	2025-05-09 21:37:52.823527+00	28	default	UPDATE_FEATURE_ENVIRONMENT	\N
2	2025-05-09 21:37:52.777238+00	2		CREATE_FEATURE	\N
2	2025-05-09 21:37:52.777238+00	3		CREATE_STRATEGY	\N
2	2025-05-09 21:37:52.777238+00	4		CREATE_ADDON	\N
2	2025-05-09 21:37:52.777238+00	5		DELETE_ADDON	\N
2	2025-05-09 21:37:52.777238+00	6		UPDATE_ADDON	\N
2	2025-05-09 21:37:52.777238+00	7		UPDATE_FEATURE	\N
2	2025-05-09 21:37:52.777238+00	8		DELETE_FEATURE	\N
2	2025-05-09 21:37:52.777238+00	9		UPDATE_APPLICATION	\N
2	2025-05-09 21:37:52.777238+00	10		UPDATE_TAG_TYPE	\N
2	2025-05-09 21:37:52.777238+00	11		DELETE_TAG_TYPE	\N
2	2025-05-09 21:37:52.777238+00	12		CREATE_PROJECT	\N
2	2025-05-09 21:37:52.777238+00	13		UPDATE_PROJECT	\N
2	2025-05-09 21:37:52.777238+00	14		DELETE_PROJECT	\N
2	2025-05-09 21:37:52.777238+00	15		UPDATE_STRATEGY	\N
2	2025-05-09 21:37:52.777238+00	16		DELETE_STRATEGY	\N
2	2025-05-09 21:37:52.777238+00	17		UPDATE_CONTEXT_FIELD	\N
2	2025-05-09 21:37:52.777238+00	18		CREATE_CONTEXT_FIELD	\N
2	2025-05-09 21:37:52.777238+00	19		DELETE_CONTEXT_FIELD	\N
2	2025-05-09 21:37:52.777238+00	29		UPDATE_FEATURE_VARIANTS	\N
4	2025-05-09 21:37:52.777238+00	2		CREATE_FEATURE	\N
4	2025-05-09 21:37:52.777238+00	7		UPDATE_FEATURE	\N
4	2025-05-09 21:37:52.777238+00	8		DELETE_FEATURE	\N
4	2025-05-09 21:37:52.777238+00	13		UPDATE_PROJECT	\N
4	2025-05-09 21:37:52.777238+00	14		DELETE_PROJECT	\N
4	2025-05-09 21:37:52.777238+00	29		UPDATE_FEATURE_VARIANTS	\N
5	2025-05-09 21:37:52.777238+00	2		CREATE_FEATURE	\N
5	2025-05-09 21:37:52.777238+00	7		UPDATE_FEATURE	\N
5	2025-05-09 21:37:52.777238+00	8		DELETE_FEATURE	\N
5	2025-05-09 21:37:52.777238+00	29		UPDATE_FEATURE_VARIANTS	\N
1	2025-05-09 21:37:52.777238+00	1		ADMIN	\N
2	2025-05-09 21:37:52.835506+00	30		MOVE_FEATURE_TOGGLE	\N
4	2025-05-09 21:37:52.835506+00	30		MOVE_FEATURE_TOGGLE	\N
2	2025-05-09 21:37:52.912651+00	31	\N	CREATE_SEGMENT	\N
2	2025-05-09 21:37:52.912651+00	32	\N	UPDATE_SEGMENT	\N
2	2025-05-09 21:37:52.912651+00	33	\N	DELETE_SEGMENT	\N
4	2025-05-09 21:37:53.319181+00	37	development	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
4	2025-05-09 21:37:53.319181+00	37	production	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
4	2025-05-09 21:37:53.319181+00	37	default	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
5	2025-05-09 21:37:53.319181+00	37	development	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
5	2025-05-09 21:37:53.319181+00	37	production	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
5	2025-05-09 21:37:53.319181+00	37	default	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
2	2025-05-09 21:37:53.319181+00	37	development	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
2	2025-05-09 21:37:53.319181+00	37	production	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
2	2025-05-09 21:37:53.319181+00	37	default	UPDATE_FEATURE_ENVIRONMENT_VARIANTS	\N
2	2025-05-09 21:37:53.406524+00	42	\N	READ_PROJECT_API_TOKEN	\N
2	2025-05-09 21:37:53.406524+00	43	\N	CREATE_PROJECT_API_TOKEN	\N
2	2025-05-09 21:37:53.406524+00	44	\N	DELETE_PROJECT_API_TOKEN	\N
4	2025-05-09 21:37:53.4112+00	42	\N	READ_PROJECT_API_TOKEN	\N
4	2025-05-09 21:37:53.4112+00	43	\N	CREATE_PROJECT_API_TOKEN	\N
4	2025-05-09 21:37:53.4112+00	44	\N	DELETE_PROJECT_API_TOKEN	\N
5	2025-05-09 21:37:53.416142+00	42	\N	READ_PROJECT_API_TOKEN	\N
5	2025-05-09 21:37:53.416142+00	43	\N	CREATE_PROJECT_API_TOKEN	\N
5	2025-05-09 21:37:53.416142+00	44	\N	DELETE_PROJECT_API_TOKEN	\N
2	2025-05-09 21:37:53.612924+00	53	\N	READ_CLIENT_API_TOKEN	\N
2	2025-05-09 21:37:53.612924+00	57	\N	READ_FRONTEND_API_TOKEN	\N
5	2025-05-09 21:37:53.813028+00	58	\N	UPDATE_FEATURE_DEPENDENCY	\N
4	2025-05-09 21:37:53.813028+00	58	\N	UPDATE_FEATURE_DEPENDENCY	\N
2	2025-05-09 21:37:53.97786+00	59	\N	CREATE_TAG_TYPE	\N
\.


--
-- Data for Name: role_user; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.role_user (role_id, user_id, created_at, project, created_by_user_id) FROM stdin;
1	1	2025-05-09 21:37:54.599519+00	default	\N
\.


--
-- Data for Name: roles; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.roles (id, name, description, type, created_at, updated_at, created_by_user_id) FROM stdin;
1	Admin	Users with the root admin role have superuser access to Unleash and can perform any operation within the Unleash platform.	root	2025-05-09 21:37:52.26007+00	\N	\N
2	Editor	Users with the root editor role have access to most features in Unleash, but can not manage users and roles in the root scope. Editors will be added as project owners when creating projects and get superuser rights within the context of these projects. Users with the editor role will also get access to most permissions on the default project by default.	root	2025-05-09 21:37:52.26007+00	\N	\N
3	Viewer	Users with the root viewer role can only read root resources in Unleash. Viewers can be added to specific projects as project members. Users with the viewer role may not view API tokens.	root	2025-05-09 21:37:52.26007+00	\N	\N
4	Owner	Users with the project owner role have full control over the project, and can add and manage other users within the project context, manage feature toggles within the project, and control advanced project features like archiving and deleting the project.	project	2025-05-09 21:37:52.402868+00	\N	\N
5	Member	Users with the project member role are allowed to view, create, and update feature toggles within a project, but have limited permissions in regards to managing the project's user access and can not archive or delete the project.	project	2025-05-09 21:37:52.402868+00	\N	\N
\.


--
-- Data for Name: segments; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.segments (id, name, description, created_by, created_at, constraints, segment_project_id) FROM stdin;
\.


--
-- Data for Name: settings; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.settings (name, content) FROM stdin;
unleash.secret	"b7cb0292ad77083890e56799a72fb3370d7e5adf"
instanceInfo	{"id" : "2894647f-7fdf-4363-9279-4584c9ca6219"}
login_history_retention	{"hours": 336}
\.


--
-- Data for Name: stat_environment_updates; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.stat_environment_updates (day, environment, updates) FROM stdin;
2025-05-09	*	1
2025-05-09	development	6
\.


--
-- Data for Name: strategies; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.strategies (created_at, name, description, parameters, built_in, deprecated, sort_order, display_name, title) FROM stdin;
2025-05-09 21:37:51.975136+00	remoteAddress	Enable the feature for a specific set of IP addresses.	[{"name":"IPs","type":"list","description":"List of IPs to enable the feature toggle for.","required":true}]	1	f	3	IPs	\N
2025-05-09 21:37:51.975136+00	applicationHostname	Enable the feature for a specific set of hostnames.	[{"name":"hostNames","type":"list","description":"List of hostnames to enable the feature toggle for.","required":false}]	1	f	4	Hosts	\N
2025-05-09 21:37:51.888971+00	default	This strategy turns on / off for your entire userbase. Prefer using "Gradual rollout" strategy (100%=on, 0%=off).	[]	1	f	1	Standard	\N
2025-05-09 21:37:52.003766+00	flexibleRollout	Roll out to a percentage of your userbase, and ensure that the experience is the same for the user on each visit.	[{"name":"rollout","type":"percentage","description":"","required":false},{"name":"stickiness","type":"string","description":"Used define stickiness. Possible values: default, userId, sessionId, random","required":true},{"name":"groupId","type":"string","description":"Used to define a activation groups, which allows you to correlate across feature toggles.","required":true}]	1	f	0	Gradual rollout	\N
2025-05-09 21:37:51.975136+00	userWithId	Enable the feature for a specific set of userIds. Prefer using "Gradual rollout" strategy with user id constraints.	[{"name":"userIds","type":"list","description":"","required":false}]	1	t	2	UserIDs	\N
\.


--
-- Data for Name: tag_types; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.tag_types (name, description, icon, created_at) FROM stdin;
simple	Used to simplify filtering of features	#	2025-05-09 21:37:52.147027+00
\.


--
-- Data for Name: tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.tags (type, value, created_at) FROM stdin;
\.


--
-- Data for Name: unleash_session; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.unleash_session (sid, sess, created_at, expired) FROM stdin;
\.


--
-- Data for Name: user_feedback; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.user_feedback (user_id, feedback_id, given, nevershow) FROM stdin;
\.


--
-- Data for Name: user_notifications; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.user_notifications (notification_id, user_id, read_at) FROM stdin;
\.


--
-- Data for Name: user_splash; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.user_splash (user_id, splash_id, seen) FROM stdin;
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.users (id, name, username, email, image_url, password_hash, login_attempts, created_at, seen_at, settings, permissions, deleted_at, is_service, created_by_user_id, is_system) FROM stdin;
-1337	Unleash System	unleash_system_user	system@getunleash.io	\N	\N	0	2025-05-09 21:37:54.164175	\N	\N	[]	\N	f	-1337	t
1	\N	admin	\N	\N	$2a$10$SC9b1qmDS9km5ShaKHmII.f4lbsMOQyjnn4Co2GgYpolphBfi/HEW	0	2025-05-09 21:37:54.473429	\N	\N	[]	\N	f	\N	f
\.


--
-- Name: addons_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.addons_id_seq', 1, false);


--
-- Name: change_request_approvals_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.change_request_approvals_id_seq', 1, false);


--
-- Name: change_request_comments_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.change_request_comments_id_seq', 1, false);


--
-- Name: change_request_events_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.change_request_events_id_seq', 1, false);


--
-- Name: change_request_rejections_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.change_request_rejections_id_seq', 1, false);


--
-- Name: change_requests_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.change_requests_id_seq', 1, false);


--
-- Name: events_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.events_id_seq', 18, true);


--
-- Name: feedback_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.feedback_id_seq', 1, false);


--
-- Name: groups_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.groups_id_seq', 1, false);


--
-- Name: incoming_webhook_tokens_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.incoming_webhook_tokens_id_seq', 1, false);


--
-- Name: incoming_webhooks_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.incoming_webhooks_id_seq', 1, false);


--
-- Name: login_events_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.login_events_id_seq', 1, false);


--
-- Name: message_banners_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.message_banners_id_seq', 1, false);


--
-- Name: migrations_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.migrations_id_seq', 234, true);


--
-- Name: notifications_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.notifications_id_seq', 1, false);


--
-- Name: observable_events_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.observable_events_id_seq', 1, false);


--
-- Name: permissions_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.permissions_id_seq', 59, true);


--
-- Name: personal_access_tokens_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.personal_access_tokens_id_seq', 1, true);


--
-- Name: roles_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.roles_id_seq', 5, true);


--
-- Name: segments_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.segments_id_seq', 1, false);


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.users_id_seq', 1, true);


--
-- Name: addons addons_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.addons
    ADD CONSTRAINT addons_pkey PRIMARY KEY (id);


--
-- Name: api_tokens api_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_tokens
    ADD CONSTRAINT api_tokens_pkey PRIMARY KEY (secret);


--
-- Name: change_request_approvals change_request_approvals_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_approvals
    ADD CONSTRAINT change_request_approvals_pkey PRIMARY KEY (id);


--
-- Name: change_request_comments change_request_comments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_comments
    ADD CONSTRAINT change_request_comments_pkey PRIMARY KEY (id);


--
-- Name: change_request_events change_request_events_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_events
    ADD CONSTRAINT change_request_events_pkey PRIMARY KEY (id);


--
-- Name: change_request_rejections change_request_rejections_change_request_id_created_by_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_rejections
    ADD CONSTRAINT change_request_rejections_change_request_id_created_by_key UNIQUE (change_request_id, created_by);


--
-- Name: change_request_rejections change_request_rejections_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_rejections
    ADD CONSTRAINT change_request_rejections_pkey PRIMARY KEY (id);


--
-- Name: change_request_schedule change_request_schedule_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_schedule
    ADD CONSTRAINT change_request_schedule_pkey PRIMARY KEY (change_request);


--
-- Name: change_request_settings change_request_settings_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_settings
    ADD CONSTRAINT change_request_settings_pkey PRIMARY KEY (project, environment);


--
-- Name: change_request_settings change_request_settings_project_environment_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_settings
    ADD CONSTRAINT change_request_settings_project_environment_key UNIQUE (project, environment);


--
-- Name: change_requests change_requests_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_requests
    ADD CONSTRAINT change_requests_pkey PRIMARY KEY (id);


--
-- Name: client_applications client_applications_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_applications
    ADD CONSTRAINT client_applications_pkey PRIMARY KEY (app_name);


--
-- Name: client_applications_usage client_applications_usage_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_applications_usage
    ADD CONSTRAINT client_applications_usage_pkey PRIMARY KEY (app_name, project, environment);


--
-- Name: client_instances client_instances_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_instances
    ADD CONSTRAINT client_instances_pkey PRIMARY KEY (app_name, environment, instance_id);


--
-- Name: client_metrics_env client_metrics_env_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_metrics_env
    ADD CONSTRAINT client_metrics_env_pkey PRIMARY KEY (feature_name, app_name, environment, "timestamp");


--
-- Name: client_metrics_env_variants client_metrics_env_variants_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_metrics_env_variants
    ADD CONSTRAINT client_metrics_env_variants_pkey PRIMARY KEY (feature_name, app_name, environment, "timestamp", variant);


--
-- Name: context_fields context_fields_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.context_fields
    ADD CONSTRAINT context_fields_pkey PRIMARY KEY (name);


--
-- Name: dependent_features dependent_features_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.dependent_features
    ADD CONSTRAINT dependent_features_pkey PRIMARY KEY (parent, child);


--
-- Name: environments environments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.environments
    ADD CONSTRAINT environments_pkey PRIMARY KEY (name);


--
-- Name: events events_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.events
    ADD CONSTRAINT events_pkey PRIMARY KEY (id);


--
-- Name: favorite_features favorite_features_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.favorite_features
    ADD CONSTRAINT favorite_features_pkey PRIMARY KEY (feature, user_id);


--
-- Name: favorite_projects favorite_projects_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.favorite_projects
    ADD CONSTRAINT favorite_projects_pkey PRIMARY KEY (project, user_id);


--
-- Name: feature_environments feature_environments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_environments
    ADD CONSTRAINT feature_environments_pkey PRIMARY KEY (environment, feature_name);


--
-- Name: feature_strategies feature_strategies_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_strategies
    ADD CONSTRAINT feature_strategies_pkey PRIMARY KEY (id);


--
-- Name: feature_strategy_segment feature_strategy_segment_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_strategy_segment
    ADD CONSTRAINT feature_strategy_segment_pkey PRIMARY KEY (feature_strategy_id, segment_id);


--
-- Name: feature_tag feature_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_tag
    ADD CONSTRAINT feature_tag_pkey PRIMARY KEY (feature_name, tag_type, tag_value);


--
-- Name: feature_types feature_types_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_types
    ADD CONSTRAINT feature_types_pkey PRIMARY KEY (id);


--
-- Name: features features_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.features
    ADD CONSTRAINT features_pkey PRIMARY KEY (name);


--
-- Name: feedback feedback_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feedback
    ADD CONSTRAINT feedback_pkey PRIMARY KEY (id);


--
-- Name: group_role group_role_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.group_role
    ADD CONSTRAINT group_role_pkey PRIMARY KEY (group_id, role_id, project);


--
-- Name: group_user group_user_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.group_user
    ADD CONSTRAINT group_user_pkey PRIMARY KEY (group_id, user_id);


--
-- Name: groups groups_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.groups
    ADD CONSTRAINT groups_pkey PRIMARY KEY (id);


--
-- Name: incoming_webhook_tokens incoming_webhook_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.incoming_webhook_tokens
    ADD CONSTRAINT incoming_webhook_tokens_pkey PRIMARY KEY (id);


--
-- Name: incoming_webhooks incoming_webhooks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.incoming_webhooks
    ADD CONSTRAINT incoming_webhooks_pkey PRIMARY KEY (id);


--
-- Name: last_seen_at_metrics last_seen_at_metrics_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.last_seen_at_metrics
    ADD CONSTRAINT last_seen_at_metrics_pkey PRIMARY KEY (feature_name, environment);


--
-- Name: login_history login_events_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.login_history
    ADD CONSTRAINT login_events_pkey PRIMARY KEY (id);


--
-- Name: banners message_banners_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.banners
    ADD CONSTRAINT message_banners_pkey PRIMARY KEY (id);


--
-- Name: migrations migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.migrations
    ADD CONSTRAINT migrations_pkey PRIMARY KEY (id);


--
-- Name: notifications notifications_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_pkey PRIMARY KEY (id);


--
-- Name: observable_events observable_events_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.observable_events
    ADD CONSTRAINT observable_events_pkey PRIMARY KEY (id);


--
-- Name: permissions permission_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.permissions
    ADD CONSTRAINT permission_unique UNIQUE (permission);


--
-- Name: permissions permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.permissions
    ADD CONSTRAINT permissions_pkey PRIMARY KEY (permission);


--
-- Name: personal_access_tokens personal_access_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.personal_access_tokens
    ADD CONSTRAINT personal_access_tokens_pkey PRIMARY KEY (id);


--
-- Name: project_environments project_environments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.project_environments
    ADD CONSTRAINT project_environments_pkey PRIMARY KEY (project_id, environment_name);


--
-- Name: project_settings project_settings_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.project_settings
    ADD CONSTRAINT project_settings_pkey PRIMARY KEY (project);


--
-- Name: project_stats project_stats_project_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.project_stats
    ADD CONSTRAINT project_stats_project_key UNIQUE (project);


--
-- Name: projects projects_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_pkey PRIMARY KEY (id);


--
-- Name: public_signup_tokens public_signup_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.public_signup_tokens
    ADD CONSTRAINT public_signup_tokens_pkey PRIMARY KEY (secret);


--
-- Name: public_signup_tokens_user public_signup_tokens_user_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.public_signup_tokens_user
    ADD CONSTRAINT public_signup_tokens_user_pkey PRIMARY KEY (secret, user_id);


--
-- Name: reset_tokens reset_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reset_tokens
    ADD CONSTRAINT reset_tokens_pkey PRIMARY KEY (reset_token);


--
-- Name: role_user role_user_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.role_user
    ADD CONSTRAINT role_user_pkey PRIMARY KEY (role_id, user_id, project);


--
-- Name: roles roles_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.roles
    ADD CONSTRAINT roles_pkey PRIMARY KEY (id);


--
-- Name: segments segments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.segments
    ADD CONSTRAINT segments_pkey PRIMARY KEY (id);


--
-- Name: settings settings_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.settings
    ADD CONSTRAINT settings_pkey PRIMARY KEY (name);


--
-- Name: stat_environment_updates stat_environment_updates_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.stat_environment_updates
    ADD CONSTRAINT stat_environment_updates_pkey PRIMARY KEY (day, environment);


--
-- Name: strategies strategies_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.strategies
    ADD CONSTRAINT strategies_pkey PRIMARY KEY (name);


--
-- Name: tag_types tag_types_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tag_types
    ADD CONSTRAINT tag_types_pkey PRIMARY KEY (name);


--
-- Name: tags tags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_pkey PRIMARY KEY (type, value);


--
-- Name: change_request_approvals unique_approvals; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_approvals
    ADD CONSTRAINT unique_approvals UNIQUE (change_request_id, created_by);


--
-- Name: roles unique_name; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.roles
    ADD CONSTRAINT unique_name UNIQUE (name);


--
-- Name: unleash_session unleash_session_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.unleash_session
    ADD CONSTRAINT unleash_session_pkey PRIMARY KEY (sid);


--
-- Name: user_feedback user_feedback_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_feedback
    ADD CONSTRAINT user_feedback_pkey PRIMARY KEY (user_id, feedback_id);


--
-- Name: user_notifications user_notifications_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_notifications
    ADD CONSTRAINT user_notifications_pkey PRIMARY KEY (notification_id, user_id);


--
-- Name: user_splash user_splash_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_splash
    ADD CONSTRAINT user_splash_pkey PRIMARY KEY (user_id, splash_id);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users users_username_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: client_instances_environment_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_instances_environment_idx ON public.client_instances USING btree (environment);


--
-- Name: events_created_by_user_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX events_created_by_user_id_idx ON public.events USING btree (created_by_user_id);


--
-- Name: events_environment_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX events_environment_idx ON public.events USING btree (environment);


--
-- Name: events_project_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX events_project_idx ON public.events USING btree (project);


--
-- Name: events_unannounced_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX events_unannounced_idx ON public.events USING btree (announced) WHERE (announced = false);


--
-- Name: feature_environments_feature_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX feature_environments_feature_name_idx ON public.feature_environments USING btree (feature_name);


--
-- Name: feature_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX feature_name_idx ON public.events USING btree (feature_name);


--
-- Name: feature_strategies_environment_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX feature_strategies_environment_idx ON public.feature_strategies USING btree (environment);


--
-- Name: feature_strategies_feature_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX feature_strategies_feature_name_idx ON public.feature_strategies USING btree (feature_name);


--
-- Name: feature_strategy_segment_segment_id_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX feature_strategy_segment_segment_id_index ON public.feature_strategy_segment USING btree (segment_id);


--
-- Name: feature_tag_tag_type_and_value_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX feature_tag_tag_type_and_value_idx ON public.feature_tag USING btree (tag_type, tag_value);


--
-- Name: idx_client_metrics_f_name; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_client_metrics_f_name ON public.client_metrics_env USING btree (feature_name);


--
-- Name: idx_feature_name; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_feature_name ON public.last_seen_at_metrics USING btree (feature_name);


--
-- Name: idx_unleash_session_expired; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_unleash_session_expired ON public.unleash_session USING btree (expired);


--
-- Name: incoming_webhook_tokens_webhook_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX incoming_webhook_tokens_webhook_id_idx ON public.incoming_webhook_tokens USING btree (incoming_webhook_id);


--
-- Name: incoming_webhooks_enabled_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX incoming_webhooks_enabled_idx ON public.incoming_webhooks USING btree (enabled);


--
-- Name: login_events_ip_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX login_events_ip_idx ON public.login_history USING btree (ip);


--
-- Name: observable_events_created_by_incoming_webhook_token_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX observable_events_created_by_incoming_webhook_token_id_idx ON public.observable_events USING btree (created_by_incoming_webhook_token_id);


--
-- Name: observable_events_source_and_source_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX observable_events_source_and_source_id_idx ON public.observable_events USING btree (source, source_id);


--
-- Name: observable_events_unannounced_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX observable_events_unannounced_idx ON public.observable_events USING btree (announced) WHERE (announced = false);


--
-- Name: project_environments_environment_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX project_environments_environment_idx ON public.project_environments USING btree (environment_name);


--
-- Name: reset_tokens_user_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX reset_tokens_user_id_idx ON public.reset_tokens USING btree (user_id);


--
-- Name: role_permission_role_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX role_permission_role_id_idx ON public.role_permission USING btree (role_id);


--
-- Name: role_user_user_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX role_user_user_id_idx ON public.role_user USING btree (user_id);


--
-- Name: segments_name_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX segments_name_index ON public.segments USING btree (name);


--
-- Name: user_feedback_user_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX user_feedback_user_id_idx ON public.user_feedback USING btree (user_id);


--
-- Name: user_splash_user_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX user_splash_user_id_idx ON public.user_splash USING btree (user_id);


--
-- Name: events unleash_update_stat_environment_changes; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER unleash_update_stat_environment_changes AFTER INSERT ON public.events FOR EACH ROW EXECUTE FUNCTION public.unleash_update_stat_environment_changes_counter();


--
-- Name: api_token_project api_token_project_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_token_project
    ADD CONSTRAINT api_token_project_project_fkey FOREIGN KEY (project) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: api_token_project api_token_project_secret_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.api_token_project
    ADD CONSTRAINT api_token_project_secret_fkey FOREIGN KEY (secret) REFERENCES public.api_tokens(secret) ON DELETE CASCADE;


--
-- Name: change_request_approvals change_request_approvals_change_request_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_approvals
    ADD CONSTRAINT change_request_approvals_change_request_id_fkey FOREIGN KEY (change_request_id) REFERENCES public.change_requests(id) ON DELETE CASCADE;


--
-- Name: change_request_approvals change_request_approvals_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_approvals
    ADD CONSTRAINT change_request_approvals_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: change_request_comments change_request_comments_change_request_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_comments
    ADD CONSTRAINT change_request_comments_change_request_fkey FOREIGN KEY (change_request) REFERENCES public.change_requests(id) ON DELETE CASCADE;


--
-- Name: change_request_comments change_request_comments_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_comments
    ADD CONSTRAINT change_request_comments_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: change_request_events change_request_events_change_request_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_events
    ADD CONSTRAINT change_request_events_change_request_id_fkey FOREIGN KEY (change_request_id) REFERENCES public.change_requests(id) ON DELETE CASCADE;


--
-- Name: change_request_events change_request_events_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_events
    ADD CONSTRAINT change_request_events_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: change_request_events change_request_events_feature_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_events
    ADD CONSTRAINT change_request_events_feature_fkey FOREIGN KEY (feature) REFERENCES public.features(name) ON DELETE CASCADE;


--
-- Name: change_request_rejections change_request_rejections_change_request_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_rejections
    ADD CONSTRAINT change_request_rejections_change_request_id_fkey FOREIGN KEY (change_request_id) REFERENCES public.change_requests(id) ON DELETE CASCADE;


--
-- Name: change_request_rejections change_request_rejections_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_rejections
    ADD CONSTRAINT change_request_rejections_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: change_request_schedule change_request_schedule_change_request_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_schedule
    ADD CONSTRAINT change_request_schedule_change_request_fkey FOREIGN KEY (change_request) REFERENCES public.change_requests(id) ON DELETE CASCADE;


--
-- Name: change_request_schedule change_request_schedule_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_schedule
    ADD CONSTRAINT change_request_schedule_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id);


--
-- Name: change_request_settings change_request_settings_environment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_settings
    ADD CONSTRAINT change_request_settings_environment_fkey FOREIGN KEY (environment) REFERENCES public.environments(name) ON DELETE CASCADE;


--
-- Name: change_request_settings change_request_settings_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_request_settings
    ADD CONSTRAINT change_request_settings_project_fkey FOREIGN KEY (project) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: change_requests change_requests_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_requests
    ADD CONSTRAINT change_requests_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: change_requests change_requests_environment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_requests
    ADD CONSTRAINT change_requests_environment_fkey FOREIGN KEY (environment) REFERENCES public.environments(name) ON DELETE CASCADE;


--
-- Name: change_requests change_requests_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.change_requests
    ADD CONSTRAINT change_requests_project_fkey FOREIGN KEY (project) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: client_applications_usage client_applications_usage_app_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_applications_usage
    ADD CONSTRAINT client_applications_usage_app_name_fkey FOREIGN KEY (app_name) REFERENCES public.client_applications(app_name) ON DELETE CASCADE;


--
-- Name: client_metrics_env_variants client_metrics_env_variants_feature_name_app_name_environm_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_metrics_env_variants
    ADD CONSTRAINT client_metrics_env_variants_feature_name_app_name_environm_fkey FOREIGN KEY (feature_name, app_name, environment, "timestamp") REFERENCES public.client_metrics_env(feature_name, app_name, environment, "timestamp") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: dependent_features dependent_features_child_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.dependent_features
    ADD CONSTRAINT dependent_features_child_fkey FOREIGN KEY (child) REFERENCES public.features(name) ON DELETE CASCADE;


--
-- Name: dependent_features dependent_features_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.dependent_features
    ADD CONSTRAINT dependent_features_parent_fkey FOREIGN KEY (parent) REFERENCES public.features(name) ON DELETE RESTRICT;


--
-- Name: favorite_features favorite_features_feature_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.favorite_features
    ADD CONSTRAINT favorite_features_feature_fkey FOREIGN KEY (feature) REFERENCES public.features(name) ON DELETE CASCADE;


--
-- Name: favorite_features favorite_features_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.favorite_features
    ADD CONSTRAINT favorite_features_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: favorite_projects favorite_projects_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.favorite_projects
    ADD CONSTRAINT favorite_projects_project_fkey FOREIGN KEY (project) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: favorite_projects favorite_projects_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.favorite_projects
    ADD CONSTRAINT favorite_projects_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: feature_environments feature_environments_environment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_environments
    ADD CONSTRAINT feature_environments_environment_fkey FOREIGN KEY (environment) REFERENCES public.environments(name) ON DELETE CASCADE;


--
-- Name: feature_environments feature_environments_feature_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_environments
    ADD CONSTRAINT feature_environments_feature_name_fkey FOREIGN KEY (feature_name) REFERENCES public.features(name) ON DELETE CASCADE;


--
-- Name: feature_strategies feature_strategies_environment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_strategies
    ADD CONSTRAINT feature_strategies_environment_fkey FOREIGN KEY (environment) REFERENCES public.environments(name) ON DELETE CASCADE;


--
-- Name: feature_strategies feature_strategies_feature_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_strategies
    ADD CONSTRAINT feature_strategies_feature_name_fkey FOREIGN KEY (feature_name) REFERENCES public.features(name) ON DELETE CASCADE;


--
-- Name: feature_strategy_segment feature_strategy_segment_feature_strategy_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_strategy_segment
    ADD CONSTRAINT feature_strategy_segment_feature_strategy_id_fkey FOREIGN KEY (feature_strategy_id) REFERENCES public.feature_strategies(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: feature_strategy_segment feature_strategy_segment_segment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_strategy_segment
    ADD CONSTRAINT feature_strategy_segment_segment_id_fkey FOREIGN KEY (segment_id) REFERENCES public.segments(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: feature_tag feature_tag_feature_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_tag
    ADD CONSTRAINT feature_tag_feature_name_fkey FOREIGN KEY (feature_name) REFERENCES public.features(name) ON DELETE CASCADE;


--
-- Name: feature_tag feature_tag_tag_type_tag_value_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.feature_tag
    ADD CONSTRAINT feature_tag_tag_type_tag_value_fkey FOREIGN KEY (tag_type, tag_value) REFERENCES public.tags(type, value) ON DELETE CASCADE;


--
-- Name: groups fk_group_role_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.groups
    ADD CONSTRAINT fk_group_role_id FOREIGN KEY (root_role_id) REFERENCES public.roles(id);


--
-- Name: group_role fk_group_role_project; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.group_role
    ADD CONSTRAINT fk_group_role_project FOREIGN KEY (project) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: role_permission fk_role_permission_permission; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.role_permission
    ADD CONSTRAINT fk_role_permission_permission FOREIGN KEY (permission) REFERENCES public.permissions(permission) ON DELETE CASCADE;


--
-- Name: group_role group_role_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.group_role
    ADD CONSTRAINT group_role_group_id_fkey FOREIGN KEY (group_id) REFERENCES public.groups(id) ON DELETE CASCADE;


--
-- Name: group_role group_role_role_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.group_role
    ADD CONSTRAINT group_role_role_id_fkey FOREIGN KEY (role_id) REFERENCES public.roles(id) ON DELETE CASCADE;


--
-- Name: group_user group_user_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.group_user
    ADD CONSTRAINT group_user_group_id_fkey FOREIGN KEY (group_id) REFERENCES public.groups(id) ON DELETE CASCADE;


--
-- Name: group_user group_user_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.group_user
    ADD CONSTRAINT group_user_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: incoming_webhook_tokens incoming_webhook_tokens_incoming_webhook_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.incoming_webhook_tokens
    ADD CONSTRAINT incoming_webhook_tokens_incoming_webhook_id_fkey FOREIGN KEY (incoming_webhook_id) REFERENCES public.incoming_webhooks(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.events(id) ON DELETE CASCADE;


--
-- Name: personal_access_tokens personal_access_tokens_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.personal_access_tokens
    ADD CONSTRAINT personal_access_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: project_environments project_environments_environment_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.project_environments
    ADD CONSTRAINT project_environments_environment_name_fkey FOREIGN KEY (environment_name) REFERENCES public.environments(name) ON DELETE CASCADE;


--
-- Name: project_environments project_environments_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.project_environments
    ADD CONSTRAINT project_environments_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: project_settings project_settings_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.project_settings
    ADD CONSTRAINT project_settings_project_fkey FOREIGN KEY (project) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: project_stats project_stats_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.project_stats
    ADD CONSTRAINT project_stats_project_fkey FOREIGN KEY (project) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: public_signup_tokens public_signup_tokens_role_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.public_signup_tokens
    ADD CONSTRAINT public_signup_tokens_role_id_fkey FOREIGN KEY (role_id) REFERENCES public.roles(id) ON DELETE CASCADE;


--
-- Name: public_signup_tokens_user public_signup_tokens_user_secret_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.public_signup_tokens_user
    ADD CONSTRAINT public_signup_tokens_user_secret_fkey FOREIGN KEY (secret) REFERENCES public.public_signup_tokens(secret) ON DELETE CASCADE;


--
-- Name: public_signup_tokens_user public_signup_tokens_user_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.public_signup_tokens_user
    ADD CONSTRAINT public_signup_tokens_user_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: reset_tokens reset_tokens_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.reset_tokens
    ADD CONSTRAINT reset_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: role_permission role_permission_role_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.role_permission
    ADD CONSTRAINT role_permission_role_id_fkey FOREIGN KEY (role_id) REFERENCES public.roles(id) ON DELETE CASCADE;


--
-- Name: role_user role_user_role_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.role_user
    ADD CONSTRAINT role_user_role_id_fkey FOREIGN KEY (role_id) REFERENCES public.roles(id) ON DELETE CASCADE;


--
-- Name: role_user role_user_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.role_user
    ADD CONSTRAINT role_user_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: segments segments_segment_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.segments
    ADD CONSTRAINT segments_segment_project_id_fkey FOREIGN KEY (segment_project_id) REFERENCES public.projects(id) ON DELETE CASCADE;


--
-- Name: tags tags_type_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_type_fkey FOREIGN KEY (type) REFERENCES public.tag_types(name) ON DELETE CASCADE;


--
-- Name: user_feedback user_feedback_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_feedback
    ADD CONSTRAINT user_feedback_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: user_notifications user_notifications_notification_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_notifications
    ADD CONSTRAINT user_notifications_notification_id_fkey FOREIGN KEY (notification_id) REFERENCES public.notifications(id) ON DELETE CASCADE;


--
-- Name: user_notifications user_notifications_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_notifications
    ADD CONSTRAINT user_notifications_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: user_splash user_splash_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_splash
    ADD CONSTRAINT user_splash_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

--
-- PostgreSQL database cluster dump complete
--

