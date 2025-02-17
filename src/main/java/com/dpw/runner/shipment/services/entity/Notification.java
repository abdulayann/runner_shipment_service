package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.NotificationRequestType;
import com.dpw.runner.shipment.services.utils.TenantIdData;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;

@Entity
@Setter
@Getter
@Table(name = "notification")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Notification extends MultiTenancy {

    @Column(name = "entity_type")
    @Size(max = 255, message = "max size is 255 for entityType")
    private String entityType;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "requested_branch_id")
    @TenantIdData
    private Integer requestedBranchId;

    @Column(name = "requested_user")
    private String requestedUser;

    @Column(name = "requested_on")
    private LocalDateTime requestedOn;

    @Column(name = "request_type")
    @Enumerated(EnumType.STRING)
    private NotificationRequestType notificationRequestType;

    @Column(name = "reason")
    @Size(max = 512, message = "max size is 512 for reason")
    private String reason;

    @Column(name = "reassigned_to_branch_id")
    @TenantIdData
    private Integer reassignedToBranchId;
}
