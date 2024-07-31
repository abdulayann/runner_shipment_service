package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.commons.entity.enums.ProductProcessTypes;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ProductSequenceConfigResponse implements IRunnerResponse {

    private Long id;
    private TenantProductsResponse tenantProducts;
    private ProductProcessTypes productProcessTypes;
    private String sequenceGroup;
    private String sequenceGroupForPaymentNoGen;
    private GenerationType generationType;
    private String prefix;
    private Integer serialCounter;
    private LocalDateTime sequenceStartTime;
    private Long shipmentSettingsId;
}
