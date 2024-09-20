package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
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
