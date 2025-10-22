package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@Schema(description = "Product Sequence Config Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductSequenceConfigRequest extends CommonRequest implements IRunnerRequest {

    private Long id;
    private TenantProductsRequest tenantProducts;
    private ProductProcessTypes productProcessTypes;
    private String sequenceGroup;
    private String sequenceGroupForPaymentNoGen;
    private GenerationType generationType;
    private String prefix;
    private Integer serialCounter;
    private LocalDateTime sequenceStartTime;
    private Long shipmentSettingsId;
}
