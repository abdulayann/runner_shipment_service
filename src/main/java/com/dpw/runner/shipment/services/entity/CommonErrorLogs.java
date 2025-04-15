package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.CommonErrorType;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import javax.validation.constraints.Size;

@Entity
@Setter
@Getter
@Table(name = "common_error_logs")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CommonErrorLogs extends MultiTenancy {
    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    @Size(max = 255, message = "max size is 255 for entityType")
    private String entityType;

    @Enumerated(EnumType.STRING)
    @Column(name = "error_type")
    private CommonErrorType errorType;

    @Column(name = "error_message")
    private String errorMessage;
}
