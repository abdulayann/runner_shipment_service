-- ============================================================
-- Script: fix_duplicate_consolidation_numbers.sql
-- Purpose: Resolve twin duplicates of (consolidation_number, tenant_id)
-- Action: Append '0' to one of the twins to restore uniqueness
--
-- Current duplicate pairs (each has exactly 2 rows):
-- BOMS24061064  | tenant 536 | count=2
-- BOMS24125035  | tenant 536 | count=2
-- BOMS25082364  | tenant 536 | count=2
-- FZHS24111529  | tenant 575 | count=2
-- MILS24110519  | tenant 612 | count=2
-- QINCSS25060544| tenant 710 | count=2
-- SNZA25052921  | tenant 551 | count=2
-- SNZS24110997  | tenant 583 | count=2
-- SSZS25062156  | tenant 654 | count=2
-- SSZS25072275  | tenant 654 | count=2
-- SSZS25072394  | tenant 654 | count=2
-- SSZS25080085  | tenant 654 | count=2
-- SSZS25080089  | tenant 654 | count=2
-- SSZS25080110  | tenant 654 | count=2
-- SSZS25080136  | tenant 654 | count=2
-- SSZS25080162  | tenant 654 | count=2
-- SSZS25080166  | tenant 654 | count=2
-- SSZS25080217  | tenant 654 | count=2
-- SSZS25080227  | tenant 654 | count=2
-- TNJS24110399  | tenant 601 | count=2
-- ============================================================
-- Perform the update (only the "second twin" gets changed)
WITH ranked AS (
    SELECT
        id,
        consolidation_number,
        tenant_id,
        ROW_NUMBER() OVER (
            PARTITION BY consolidation_number, tenant_id
            ORDER BY id
        ) AS rn
    FROM consolidation_details
)
UPDATE consolidation_details d
SET consolidation_number = d.consolidation_number || '0'
FROM ranked r
WHERE d.id = r.id
  AND r.rn = 2;