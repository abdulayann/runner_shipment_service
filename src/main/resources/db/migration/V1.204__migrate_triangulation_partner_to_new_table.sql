-- Script for transferring triangulationPartner data for Shipment entity

-- Step 1: Create the triangulation_partner_shipment table
CREATE TABLE IF NOT EXISTS triangulation_partner_shipment (
    shipment_id BIGINT NOT NULL,
    partner_id BIGINT NOT NULL,
    PRIMARY KEY (shipment_id, partner_id),
    CONSTRAINT fk_triangulation_partner_shipment
      FOREIGN KEY (shipment_id) REFERENCES shipment_details(id)
      ON DELETE CASCADE
);

-- Step 2: Transfer data from shipment to triangulation_partner_shipment
--INSERT INTO triangulation_partner_shipment (shipment_id, partner_id)
--SELECT shipment_details.id, shipment_details.triangulation_partner
--FROM shipment_details
--WHERE shipment_details.triangulation_partner IS NOT NULL;

-- Script for transferring triangulationPartner data for Consolidation entity

-- 1. Create the new table triangulation_partner_consolidation if not exists
CREATE TABLE IF NOT EXISTS triangulation_partner_consolidation (
    consolidation_id BIGINT NOT NULL,
    partner_id BIGINT NOT NULL,
    PRIMARY KEY (consolidation_id, partner_id),
    CONSTRAINT fk_triangulation_partner_consolidation
      FOREIGN KEY (consolidation_id) REFERENCES consolidation_details(id)
      ON DELETE CASCADE
);

-- 2. Insert data from the old triangulation_partner column into the new table triangulation_partner_consolidation
--INSERT INTO triangulation_partner_consolidation (consolidation_id, partner_id)
--SELECT consolidation_details.id, consolidation_details.triangulation_partner
--FROM consolidation_details
--WHERE consolidation_details.triangulation_partner IS NOT NULL;
