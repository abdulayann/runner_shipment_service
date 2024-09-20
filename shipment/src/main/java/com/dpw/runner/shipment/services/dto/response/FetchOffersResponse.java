package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FetchOffersResponse implements IRunnerResponse {
 
    @JsonProperty("offer_type")
    private String offer_type;
    @JsonProperty("offers")
    private List<Offer> offers;
    @JsonProperty("reason")
    private String reason;
    @JsonProperty("message")
    private String message;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class AssociatedRate implements Serializable{
        @JsonProperty("slabs")
        private List<Object> slabs;
        @JsonProperty("calculated_sell")
        private BigDecimal calculated_sell;
        @JsonProperty("calculated_cost")
        private BigDecimal calculated_cost;
        @JsonProperty("procured_sell")
        private BigDecimal procured_sell;
        @JsonProperty("procured_cost")
        private BigDecimal procured_cost;
        @JsonProperty("unit_sell")
        private BigDecimal unit_sell;
        @JsonProperty("unit_cost")
        private BigDecimal unit_cost;
        @JsonProperty("procured_unit_sell")
        private BigDecimal procured_unit_sell;
        @JsonProperty("procured_unit_cost")
        private BigDecimal procured_unit_cost;
        @JsonProperty("calculated_margin")
        private BigDecimal calculated_margin;
        @JsonProperty("procured_margin")
        private BigDecimal procured_margin;
        @JsonProperty("rates_uom")
        private String rates_uom;
        @JsonProperty("conversion_rate")
        private BigDecimal conversion_rate;
        @JsonProperty("chargeable")
        private BigDecimal chargeable;
        @JsonProperty("chargeable_uom")
        private String chargeable_uom;
        @JsonProperty("total_unit_count")
        private BigDecimal total_unit_count;
        @JsonProperty("measurement_unit")
        private String measurement_unit;
        @JsonProperty("applicable_on_booking")
        private Boolean applicable_on_booking;
        @JsonProperty("mode_of_transport")
        private String mode_of_transport;
        @JsonProperty("rate_classification")
        private String rate_classification;
        @JsonProperty("node_code")
        private String node_code;
        @JsonProperty("rate_taxes")
        private List<Object> rate_taxes;
        @JsonProperty("charge_group")
        private List<String> charge_group;
        @JsonProperty("slab_floor")
        private BigDecimal slab_floor;
        @JsonProperty("slab_ceil")
        private BigDecimal slab_ceil;
        @JsonProperty("carrier")
        private String carrier;
        @JsonProperty("rate_type")
        private String rate_type;
        @JsonProperty("rate_name")
        private String rate_name;
        @JsonProperty("base_price_currency")
        private String base_price_currency;
        @JsonProperty("required_currency")
        private String required_currency;
        @JsonProperty("procured_currency")
        private String procured_currency;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class EntityRateCard implements Serializable{
        @JsonProperty("loads_rates_info")
        private List<LoadsRatesInfo> loads_rates_info;
        @JsonProperty("aggregated_shipment_load_rates_info")
        private List<LoadsRatesInfo> aggregated_shipment_load_rates_info;
        @JsonProperty("mode_of_transport")
        private String mode_of_transport;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class LoadsRatesInfo implements Serializable{
        @JsonProperty("associated_rates")
        private List<AssociatedRate> associated_rates;
        @JsonProperty("quantity")
        private Long quantity;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Meta implements Serializable{
        @JsonProperty("route")
        private List<Route> route;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Node implements Serializable{
        @JsonProperty("code")
        private String code;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Offer implements Serializable {
        @JsonProperty("offer_type")
        private String offer_type;
        @JsonProperty("chargeable")
        private BigDecimal chargeable;
        @JsonProperty("chargeable_uom")
        private String chargeable_uom;
        @JsonProperty("entity_rate_cards")
        private List<EntityRateCard> entity_rate_cards;
        @JsonProperty("requested_currency")
        private String requested_currency;
        @JsonProperty("product_name")
        private String product_name;
        @JsonProperty("tenant_uuid")
        private String tenant_uuid;
        @JsonProperty("meta")
        private Meta meta;
        @JsonProperty("shipment_level_rates")
        private List<AssociatedRate> shipment_level_rates;
        @JsonProperty("min_transit_hours")
        private String minTransitHours;
        @JsonProperty("max_transit_hours")
        private String maxTransitHours;
        @JsonProperty("carrier")
        private String carrier;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Route implements Serializable{
        @JsonProperty("type")
        private String type;
        @JsonProperty("node")
        private Node node;
        @JsonProperty("origin")
        private Node origin;
        @JsonProperty("destination")
        private Node destination;
    }

}

