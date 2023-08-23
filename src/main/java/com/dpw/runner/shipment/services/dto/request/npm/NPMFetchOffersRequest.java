package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class NPMFetchOffersRequest extends CommonRequest implements IRunnerRequest {
    private String origin;
    private String destination;
    private String POL;
    private String POD;
    private String currency;
    private ExchangeRates exchange_rates;
    private String preferred_date;
    private String preferred_date_type;
    private String carrier;
    private List<LoadInformation> loads_information;
    private String mode_of_transport;
    private String product_name;
    private ContractDetails contract_details;
    private String shipment_type;
    private String service_mode;
    private boolean slab_rates;
    private String scope_restriction;
    private String service_category;
    private String customer_category;
    private boolean fetch_default_rates;
    private boolean is_alteration;
    private String offer_type;

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ExchangeRates {
        private String base;
        private Map<String, Double> rate;
    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoadInformation {
        private LoadDetail load_detail;
        private LoadAttributes load_attributes;
    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoadDetail {
        private String load_type;
        private String cargo_type;
        private String product_category_code;
    }


    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoadAttributes {
        private BigDecimal weight;
        private String weight_uom;
        private BigDecimal distance;
        private String distance_uom;
        private Long quantity;
        private Long delta_quantity;
        private String quantity_uom;
        private BigDecimal volume;
        private String volume_uom;
        private BigDecimal chargeable;
        private String chargeable_uom;
        private Map<String, BigDecimal> dimensions;
    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ContractDetails {
        private List<String> contracts;
        private String company_code;
    }
}
