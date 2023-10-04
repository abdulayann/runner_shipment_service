package com.dpw.runner.shipment.services.ReportingService.Models.Commons;

import java.util.List;

public class ShipmentAndContainerResponse {
        public String hsnNumber;
        public String masterBill;
        public String houseBill;
        public String consigneeCompanyName;
        public String consigneeAddress1;
        public String consigneeAddress2;
        public String consigneeCountry;
        public String consigneeZip;
        public String consignerCompanyName;
        public String consignerAddress1;
        public String consignerAddress2;
        public String consignerCountry;
        public String consignerZip;
        public String notifyCompanyName;
        public String notifyAddress1;
        public String notifyAddress2;
        public String notifyCountry;
        public String notifyZip;
        public List<String> consignerAddressFreeText;
        public List<String> consigneeAddressFreeText;
        public List<String> notifyPartyAddressFreeText;
        public List<ShipmentContainers> shipmentContainers;
}
