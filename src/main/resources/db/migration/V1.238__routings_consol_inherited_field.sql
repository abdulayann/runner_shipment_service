ALTER TABLE if exists routings
    ADD COLUMN if not exists inherited_from_consolidation BOOLEAN DEFAULT FALSE;
