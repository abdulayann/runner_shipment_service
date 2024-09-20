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
    "status": {
      "required": true
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
        "flightNumber": {
          "pattern": "(\\d{4}[A-Z]|\\d{4}|\\d{3}[A-Z]|\\d{3})",
          "errors": {
            "pattern": "Please provide proper Flight number"
          }
        },
        "destinationPort": {
          "required": true
        }
      }
    }
  }
}'
where entity = 'SHIPMENT';