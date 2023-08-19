package com.dpw.runner.shipment.services.dto.request.booking;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

/**
 * {
 * "request_array": [
 * {
 * "origin": "INMUN_POR", // required
 * "destination": "AEJEA_POR", // required
 * "preferred_date": "2023-07-21 - Booking search page", // required // similar to booking date
 * "preferred_date_type": "PICKUP", // required // hardcode as PICKUP
 * "loads_info": [
 * {
 * "load_details": {
 * "load_type": "FCL", // FCL for container, LCL for package OCEAN, LSE for Air // required
 * "cargo_type": "20SD", // required
 * "commodity": "FAK", // required
 * "hazardous_info": { // required - **Open point** since runner is not capturing this info in booking module
 * "is_hazardous": "false" // always false
 * }
 * },
 * "load_attributes": {
 * "weight": 20,
 * "weight_uom": "mt",
 * "quantity": 1,
 * "quantity_uom": "unit",
 * "volume": 20,
 * "volume_uom": "cbm",
 * "dimensions": {
 * "length": 20,
 * "width": 20,
 * "height": 20,
 * "uom": "cm"
 * }
 * }
 * }
 * ],
 * "mode_of_transport": "OCEAN/AIR", // Runner to maintain this mapping from the quote list response and send back //required
 * "requested_currency": "INR/USD", // optional and NPM to send back charge codes in their procured currency ** Open Point ** Runner to check with mohit
 * "shipment_movement": "EXPORT/IMPORT/DOMESTIC", // required
 * "service_mode": "P2P", // required **
 * "contracts_info": { // required
 * "customer_org_id": "FRC0001", // required
 * "contract_id": "ABC123" // required if contract selected
 * },
 * "business_info": {
 * "product_name": "FCL_OCEAN/LCL_OCEAN/LSE_OCEAN/LCL_AIR", // required // and this info can be mapped from product_type field in quote list and this will also be required for alteration and runner should not derive this and should send back whatever is sent in quote list.
 * "tenant_id": "1234" // UI -> Send null --> Backend :-> Need to map (static value mapping to be stored in backend vs product_name to tenant_id)
 * },
 * "fetch_default_rates": "true", // required // runner to hardcode as true
 * "carrier_code": "DPW_PS" // optional if present
 * }
 * ]
 * }
 */
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OfferRequest extends CommonRequest implements IRunnerRequest {
    @JsonProperty("origin")
    private String pol;

    @JsonProperty("destination")
    private String pod;

    @JsonProperty("preferred_date")
    private LocalDateTime preferredDate;

    @JsonProperty("preferred_date_type")
    private String preferredDateType;

    @JsonProperty("mode_of_transport")
    private String mode;

    @JsonProperty("requested_currency")
    private String requestedCurrency;

    private List<ContainerRequest> containers;
    private List<PackingRequest> packs;

    private String cargoType;

    @JsonProperty("shipment_movement")
    private String shipmentMovement;

    @JsonProperty("service_mode")
    private String serviceMode;

    @JsonProperty("contracts_info")
    private ContractInfoRequest contractsInfo;

    @JsonProperty("business_info")
    private BusinessInfoRequest businessInfoRequest;

    @JsonProperty("fetch_default_rates")
    private String fetchDefaultRates;

    @JsonProperty("carrierCode")
    private String carrierCode;
}
