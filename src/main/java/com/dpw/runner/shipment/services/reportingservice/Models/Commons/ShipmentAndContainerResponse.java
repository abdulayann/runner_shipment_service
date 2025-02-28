package com.dpw.runner.shipment.services.reportingservice.Models.Commons;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentAndContainerResponse implements IRunnerResponse {
        @JsonProperty("HsnNumber")
        public String hsnNumber;
        @JsonProperty("MasterBill")
        public String masterBill;
        @JsonProperty("HouseBill")
        public String houseBill;
        @JsonProperty("ConsigneeCompanyName")
        public String consigneeCompanyName;
        @JsonProperty("ConsigneeAddress1")
        public String consigneeAddress1;
        @JsonProperty("ConsigneeAddress2")
        public String consigneeAddress2;
        @JsonProperty("ConsigneeCountry")
        public String consigneeCountry;
        @JsonProperty("ConsigneeZip")
        public String consigneeZip;
        @JsonProperty("ConsignerCompanyName")
        public String consignerCompanyName;
        @JsonProperty("ConsignerAddress1")
        public String consignerAddress1;
        @JsonProperty("ConsignerAddress2")
        public String consignerAddress2;
        @JsonProperty("ConsignerCountry")
        public String consignerCountry;
        @JsonProperty("ConsignerZip")
        public String consignerZip;
        @JsonProperty("NotifyCompanyName")
        public String notifyCompanyName;
        @JsonProperty("NotifyAddress1")
        public String notifyAddress1;
        @JsonProperty("NotifyAddress2")
        public String notifyAddress2;
        @JsonProperty("NotifyCountry")
        public String notifyCountry;
        @JsonProperty("NotifyZip")
        public String notifyZip;
        @JsonProperty("ConsignerAddressFreeText")
        private List<String> consignerAddressFreeText;
        @JsonProperty("ConsigneeAddressFreeText")
        private List<String> consigneeAddressFreeText;
        @JsonProperty("NotifyPartyAddressFreeText")
        private List<String> notifyPartyAddressFreeText;
        @JsonProperty("Containers")
        private List<ShipmentContainers> shipmentContainers;
        @JsonProperty("Description")
        public String description;
        @JsonProperty("Weight")
        public String weight;
        @JsonProperty("Volume")
        public String volume;
        @JsonProperty("Packs")
        public String packs;
        @JsonProperty("PacksUnit")
        public String packsUnit;
        @JsonProperty("WeightUnit")
        public String weightUnit;
        @JsonProperty("VolumeUnit")
        public String volumeUnit;
        @JsonProperty("VolumeUnitDescription")
        public String volumeUnitDescription;
        @JsonProperty("WeightUnitDescription")
        public String weightUnitDescription;
        @JsonProperty("PacksUnitDescription")
        public String packsUnitDescription;
        @JsonProperty("MarksnNumber")
        public String marksnNumbers;
        @JsonProperty("FreightOverseasCurrency")
        public String freightOverseasCurrency;
}
