package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import org.springframework.stereotype.Component;

@Component
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Getter
@Setter
public class MblDuplicatedLog extends BaseEntity {
    private String shipmentId;
    private String consolidationNo;
    private String mblNumber;
    private Integer tenantId;
}
