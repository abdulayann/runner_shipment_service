ALTER TABLE IF EXISTS notification
    ADD COLUMN IF NOT EXISTS reassigned_from_branch_id integer;