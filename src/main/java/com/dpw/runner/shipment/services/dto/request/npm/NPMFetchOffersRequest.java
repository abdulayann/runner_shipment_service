package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
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
    @JsonProperty("POL")
    private String POL;
    @JsonProperty("POD")
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
    private Boolean fetch_default_rates;
    private Boolean is_alteration;
    private String offer_type;
    private BusinessInfo business_info;
    private ContractsInfo contracts_info;
    private String shipment_movement;
    private List<LoadInformation> loads_info;
    private String carrier_code;

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ExchangeRates implements Serializable{
        private String base;
        private Map<String, Double> rate;
    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoadInformation implements Serializable {
        private LoadDetail load_detail;
        private LoadAttributes load_attributes;
    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoadDetail implements Serializable{
        private String load_type;
        private String cargo_type;
        private String product_category_code;
        private String commodity;
        private Boolean is_dangerous;
        private String code;
        private String un_number;
    }


    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class LoadAttributes implements Serializable{
        private BigDecimal weight;
        private String weight_uom;
        private Long quantity;
        private Long delta_quantity;
        private String quantity_uom;
        private BigDecimal volume;
        private String volume_uom;
        private BigDecimal chargeable;
        private String chargeable_uom;
    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ContractDetails implements Serializable{
        private List<String> contracts;
        private String company_code;
    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BusinessInfo implements Serializable{
        private String product_name;
        private String tenant_id;
    }

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ContractsInfo implements Serializable{
        private String customer_org_id;
        private String contract_id;
    }
}
