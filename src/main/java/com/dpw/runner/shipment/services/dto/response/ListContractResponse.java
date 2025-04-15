package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ListContractResponse implements IRunnerResponse {
    @JsonProperty("contracts")
    private List<ContractResponse> contracts;
    @JsonProperty("count")
    private Long count;
    private Map<String, UnlocationsResponse> unlocMasterData;
    @JsonProperty("carrierMasterData")
    private Map<String, EntityTransferCarrier> carrierMasterData;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ContractResponse implements Serializable {
        @JsonProperty("_id")
        private String _id;
        @JsonProperty("contract_id")
        private String contract_id;
        @JsonProperty("parent_contract_id")
        private String parent_contract_id;
        @JsonProperty("contract_type")
        private String contract_type;
        @JsonProperty("source")
        private String source;
        @JsonProperty("source_type")
        private String source_type;
        @JsonProperty("customer_org_id")
        private String customer_org_id;
        @JsonProperty("origin")
        private String origin;
        @JsonProperty("destination")
        private String destination;
        @JsonProperty("valid_from")
        private LocalDateTime valid_from;
        @JsonProperty("valid_till")
        private LocalDateTime valid_till;
        @JsonProperty("product_type")
        private String product_type;
        @JsonProperty("state")
        private String state;
        @JsonProperty("tenant_id")
        private String tenant_id;
        @JsonProperty("offer_type")
        private String offer_type;
        @JsonProperty("via_nodes")
        private List<String> via_nodes;
        @JsonProperty("load_types")
        private List<String> load_types;
        @JsonProperty("cargo_types")
        private List<String> cargo_types;
        @JsonProperty("commodities")
        private List<String> commodities;
        @JsonProperty("carrier_codes")
        private List<String> carrier_codes;
        @JsonProperty("cha")
        private String cha;
        @JsonProperty("forwarder")
        private String forwarder;
        @JsonProperty("bco")
        private String bco;
        @JsonProperty("cycle")
        private String cycle;
        @JsonProperty("meta")
        private Meta meta;
        @JsonProperty("createdAt")
        private LocalDateTime createdAt;
        @JsonProperty("updatedAt")
        private LocalDateTime updatedAt;
        @JsonProperty("contract_usage")
        private List<ContractUsage> contract_usage;
        @JsonProperty("destination_name")
        private String destination_name;
        @JsonProperty("origin_name")
        private String origin_name;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Meta implements Serializable{
        @JsonProperty("pod")
        private String pod;
        @JsonProperty("pol")
        private String pol;
        @JsonProperty("route")
        private List<Route> route;
        @JsonProperty("incoterm")
        private String incoterm;
        @JsonProperty("service_mode")
        private String service_mode;
        @JsonProperty("payment_terms")
        private String payment_terms;
        @JsonProperty("additional_info")
        private String additional_info;
        @JsonProperty("mode_of_transport")
        private String mode_of_transport;
        @JsonProperty("shipment_movement")
        private String shipment_movement;
        @JsonProperty("branch_info")
        private BranchInfo branch_info;
        @JsonProperty("min_transit_hours")
        private String minTransitHours;
        @JsonProperty("max_transit_hours")
        private String maxTransitHours;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class BranchInfo implements Serializable{
        @JsonProperty("id")
        private String id;
        @JsonProperty("country")
        private String country;
        @JsonProperty("sales_agent_primary_email")
        private String sales_agent_primary_email;
        @JsonProperty("sales_agent_secondary_email")
        private String sales_agent_secondary_email;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Route implements Serializable{
        @JsonProperty("type")
        private String type;
        @JsonProperty("node")
        private RouteInfo node;
        @JsonProperty("origin")
        private RouteInfo origin;
        @JsonProperty("destination")
        private RouteInfo destination;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class RouteInfo implements Serializable{
        @JsonProperty("code")
        private String code;
        @JsonProperty("country_id")
        private Long country_id;
        @JsonProperty("is_archived")
        private Boolean is_archived;
        @JsonProperty("is_dpw_owned")
        private Boolean is_dpw_owned;
        @JsonProperty("application_namespace_id")
        private Long application_namespace_id;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class ContractUsage implements Serializable{
        @JsonProperty("usage_id")
        private String usage_id;
        @JsonProperty("filter_params")
        private FilterParams filter_params;
        @JsonProperty("usage")
        private Long usage;
        @JsonProperty("usage_uom")
        private String usage_uom;
        @JsonProperty("meta")
        private ContractUsageMeta meta;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class FilterParams implements Serializable{
        @JsonProperty("load_type")
        private List<String> load_type;
        @JsonProperty("cargo_type")
        private List<String> cargo_type;
        @JsonProperty("commodity")
        private List<String> commodity;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class ContractUsageMeta implements Serializable{
        @JsonProperty("original_usage")
        private Long original_usage;
        @JsonProperty("original_usage_uom")
        private String original_usage_uom;
        @JsonProperty("load_attributes")
        private LoadAttributes load_attributes;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class LoadAttributes implements Serializable{
        @JsonProperty("weight")
        private BigDecimal weight;
        @JsonProperty("quantity")
        private Long quantity;
        @JsonProperty("volume")
        private BigDecimal volume;
        @JsonProperty("weight_uom")
        private String weight_uom;
        @JsonProperty("quantity_uom")
        private String quantity_uom;
        @JsonProperty("volume_uom")
        private String volume_uom;
        @JsonProperty("dimensions")
        private Dimensions dimensions;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Dimensions implements Serializable{
        @JsonProperty("width")
        private Long width;
        @JsonProperty("height")
        private Long height;
        @JsonProperty("length")
        private Long length;
        @JsonProperty("uom")
        private String uom;
    }

}

