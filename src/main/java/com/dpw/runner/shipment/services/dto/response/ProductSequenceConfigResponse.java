package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ProductSequenceConfigResponse implements IRunnerResponse {

    private Long id;
    private TenantProducts tenantProducts;
    private ProductProcessTypes productProcessTypes;
    private String sequenceGroup;
    private String sequenceGroupForPaymentNoGen;
    private GenerationType generationType;
    private String prefix;
    private Integer serialCounter;
    private LocalDateTime sequenceStartTime;
    private Long shipmentSettingsId;
}
