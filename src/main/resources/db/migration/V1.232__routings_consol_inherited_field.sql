ALTER TABLE if exists routings
    ADD COLUMN inherited_from_consolidation BOOLEAN DEFAULT FALSE;
