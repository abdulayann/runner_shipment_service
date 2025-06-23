alter table if exists routings
ADD COLUMN IF NOT EXISTS is_selected_for_document BOOLEAN DEFAULT false;