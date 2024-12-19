package com.dpw.runner.shipment.services.dto.request.platform;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@Data
public class PlatformCreateRequest extends CommonRequest implements IRunnerRequest {
    @JsonProperty(value = "booking_reference_code")
    private String booking_ref_code;
    private String business_code;
    private String origin_code;
    private String destination_code;
    private String pol;
    private String pod;
    private String contract_id;
    private String customer_org_id;
    private String customer_email;
    private LocalDateTime created_at;
    private List<LoadRequest> load;
    private RouteRequest route;
    private List<ChargesRequest> charges;
    private List<OrgRequest> bill_to_party;
    private String parent_contract_id;
    private ListContractResponse.BranchInfo branch_info;
    @JsonProperty(value = "min_transit_hours")
    private String minTransitHours;
    @JsonProperty(value = "max_transit_hours")
    private String maxTransitHours;
    @JsonProperty(value = "carrier_code")
    private String mainLegCarrierCode;
    @JsonProperty(value = "carrier_display_name")
    private String carrierDisplayName;
    @JsonProperty(value = "vessel_name")
    private String vesselName;
    private String voyage;
    private LocalDateTime eta;
    private LocalDateTime ets;
    private LocalDateTime ata;
    private LocalDateTime ats;
    @JsonProperty(value = "transport_mode")
    private String transportMode;
    @JsonProperty(value = "shipment_movement")
    private String shipmentMovement;
    @JsonProperty(value = "is_hazardous")
    private Boolean isDg;
    private String source;
    private List<Map<String, Object>> addresses;
}
