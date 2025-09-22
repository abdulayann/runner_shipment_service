ALTER TABLE app_config ALTER COLUMN value TYPE TEXT;

INSERT INTO app_config (
    "key", value, guid, created_at, updated_at, created_by, updated_by, is_deleted
)
VALUES (
    'BOOKING_TO_BOOKING_CLONE_CONFIG',
    '{
      "header": {
        "label": "Header",
        "value": "header",
        "fields": [
          { "label": "Mode", "value": "mode" },
          { "label": "Shipment Type", "value": "shipmentType" },
          { "label": "Cargo Type", "value": "cargoType" },
          { "label": "Service Type", "value": "serviceType" },
          { "label": "Payment Terms", "value": "paymentTerms" },
          { "label": "Origin", "value": "origin" },
          { "label": "POL", "value": "pol" },
          { "label": "POD", "value": "pod" },
          { "label": "Destination", "value": "destination" }
        ]
      },
      "party": {
        "label": "Party",
        "value": "party",
        "fields": [
          { "label": "Client", "value": "client" },
          { "label": "Shipper", "value": "shipper" },
          { "label": "Consignee", "value": "consignee" },
          { "label": "Notify Party", "value": "notifyParty" },
          { "label": "Add. Party", "value": "additionalParty" }
        ]
      },
      "general": {
        "label": "General",
        "value": "general",
        "fields": [
          { "label": "Incoterms", "value": "incoterms" },
          { "label": "Carrier", "value": "carrier" }
        ]
      },
      "containers": {
        "label": "Container(s)",
        "value": "containers",
        "fields": [
          { "label": "Type", "value": "containerType", "is_disabled": true },
          { "label": "Container Count", "value": "containerCount" },
          { "label": "Pkg per Container", "value": "packagesPerContainer" },
          { "label": "Cargo Wt. per Container", "value": "cargoWeightPerContainer" },
          { "label": "Commodity Category", "value": "containerCommodityCategory" }
        ]
      },
      "packages": {
        "label": "Package(s)",
        "value": "packages",
        "fields": [
          { "label": "Pkg Type", "value": "packageType", "is_disabled": true },
          { "label": "Package Count", "value": "packageCount", "is_disabled": true },
          { "label": "Volume per Pack", "value": "volumePerPack" },
          { "label": "Cargo Wt. per Pack", "value": "cargoWeightPerPack" },
          { "label": "Commodity Category", "value": "packageCommodityCategory" },
          { "label": "Dimension per Pack", "value": "dimensionPerPack" },
          { "label": "Volume", "value": "volume" },
          { "label": "Cargo Wt.", "value": "cargoWeight" }
        ]
      },
      "cargoSummary": {
        "label": "Cargo Summary",
        "value": "cargoSummary",
        "fields": [
          { "label": "Description", "value": "description" },
          { "label": "Marks & No.", "value": "marksAndNumbers" },
          { "label": "Additional Terms", "value": "additionalTerms" }
        ]
      }
    }'::jsonb,
    uuid_generate_v4(),
    NOW(),
    NOW(),
    'SYSTEM',
    'SYSTEM',
    false
)
ON CONFLICT ("key") DO NOTHING;

INSERT INTO app_config (
    "key", value, guid, created_at, updated_at, created_by, updated_by, is_deleted
)
VALUES (
    'SHIPMENT_TO_SHIPMENT_CLONE_CONFIG',
    '{
      "header": {
        "label": "Header",
        "value": "header",
        "fields": [
          { "label": "Mode", "value": "mode" },
          { "label": "Shipment Type", "value": "shipmentType" },
          { "label": "Cargo Type", "value": "cargoType" },
          { "label": "Service Type", "value": "serviceType" },
          { "label": "Payment Terms", "value": "paymentTerms" },
          { "label": "Origin", "value": "origin" },
          { "label": "POL", "value": "pol" },
          { "label": "POD", "value": "pod" },
          { "label": "Destination", "value": "destination" }
        ]
      },
      "party": {
        "label": "Party",
        "value": "party",
        "fields": [
          { "label": "Client", "value": "client" },
          { "label": "Shipper", "value": "shipper" },
          { "label": "Consignee", "value": "consignee" },
          { "label": "Notify Party", "value": "notifyParty" },
          { "label": "Add. Party", "value": "additionalParty" }
        ]
      },
      "general": {
        "label": "General",
        "value": "general",
        "fields": [
          { "label": "Incoterms", "value": "incoterms" },
          { "label": "Carrier", "value": "carrier" }
        ]
      },
      "cargoSummary": {
        "label": "Cargo Summary",
        "value": "cargoSummary",
        "fields": [
          { "label": "Description", "value": "description" },
          { "label": "Marks & No.", "value": "marksAndNumbers" },
          { "label": "Additional Terms", "value": "additionalTerms" }
        ]
      }
    }'::jsonb,
    uuid_generate_v4(),
    NOW(),
    NOW(),
    'SYSTEM',
    'SYSTEM',
    false
)
ON CONFLICT ("key") DO NOTHING;

INSERT INTO app_config (
    "key", value, guid, created_at, updated_at, created_by, updated_by, is_deleted
)
VALUES (
    'SHIPMENT_TO_BOOKING_CLONE_CONFIG',
    '{
      "header": {
        "label": "Header",
        "value": "header",
        "fields": [
          { "label": "Mode", "value": "mode" },
          { "label": "Shipment Type", "value": "shipmentType" },
          { "label": "Cargo Type", "value": "cargoType" },
          { "label": "Service Type", "value": "serviceType" },
          { "label": "Payment Terms", "value": "paymentTerms" },
          { "label": "Origin", "value": "origin" },
          { "label": "POL", "value": "pol" },
          { "label": "POD", "value": "pod" },
          { "label": "Destination", "value": "destination" }
        ]
      },
      "party": {
        "label": "Party",
        "value": "party",
        "fields": [
          { "label": "Client", "value": "client" },
          { "label": "Shipper", "value": "shipper" },
          { "label": "Consignee", "value": "consignee" },
          { "label": "Notify Party", "value": "notifyParty" },
          { "label": "Add. Party", "value": "additionalParty" }
        ]
      },
      "general": {
        "label": "General",
        "value": "general",
        "fields": [
          { "label": "Incoterms", "value": "incoterms" },
          { "label": "Carrier", "value": "carrier" }
        ]
      },
      "containers": {
        "label": "Container(s)",
        "value": "containers",
        "fields": [
          { "label": "Type", "value": "containerType", "is_disabled": true },
          { "label": "Container Count", "value": "containerCount" },
          { "label": "Cargo Wt. per Container", "value": "cargoWeightPerContainer" },
          { "label": "Commodity Category", "value": "containerCommodityCategory" }
        ]
      },
      "packages": {
        "label": "Package(s)",
        "value": "packages",
        "fields": [
          { "label": "Pkg Type", "value": "packageType", "is_disabled": true },
          { "label": "Package Count", "value": "packageCount", "is_disabled": true },
          { "label": "Volume per Pack", "value": "volumePerPack" },
          { "label": "Cargo Wt. per Pack", "value": "cargoWeightPerPack" },
          { "label": "Commodity Category", "value": "packageCommodityCategory" },
          { "label": "Dimension per Pack", "value": "dimensionPerPack" },
          { "label": "Volume", "value": "volume" },
          { "label": "Cargo Wt.", "value": "cargoWeight" }
        ]
      },
      "cargoSummary": {
        "label": "Cargo Summary",
        "value": "cargoSummary",
        "fields": [
          { "label": "Description", "value": "description" },
          { "label": "Marks & No.", "value": "marksAndNumbers" },
          { "label": "Additional Terms", "value": "additionalTerms" }
        ]
      }
    }'::jsonb,
    uuid_generate_v4(),
    NOW(),
    NOW(),
    'SYSTEM',
    'SYSTEM',
    false
)
ON CONFLICT ("key") DO NOTHING;
