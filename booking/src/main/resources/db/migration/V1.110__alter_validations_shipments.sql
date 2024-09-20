Update validations
set schema_validator = '{
  "properties": {
    "client": {
      "type": "object",
      "properties": {
        "orgData": {
          "required": true
        },
        "addressData": {
          "required": true
        }
      }
    },
    "direction": {
      "required": true
    },
    "shipmentType": {
      "required": true
    },
    "transportMode": {
      "required": true
    },
    "carrierDetails": {
      "type": "object",
      "properties": {
        "originPort": {
          "required": true
        },
        "destinationPort": {
          "required": true
        },
        "flightNumber": {
          "pattern": "(\\d{4}[A-Z]|\\d{4}|\\d{3}[A-Z]|\\d{3})"
        }
      }
    }
  }
}'
where entity = 'SHIPMENT';