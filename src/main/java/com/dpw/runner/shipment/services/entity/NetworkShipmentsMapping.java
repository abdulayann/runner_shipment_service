package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import javax.validation.constraints.Size;

@Entity
@Setter
@Getter
@Table(name = "network_shipments_mapping")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NetworkShipmentsMapping extends MultiTenancy {
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "network_transfer_id", nullable = false)
    private NetworkTransfer networkTransfer;

    @Column(name = "entity_type")
    @Size(max = 255, message = "max size is 255 for entityType")
    private String entityType;

    @Column(name = "entity_number")
    @Size(max = 255, message = "max size is 255 for entityNumber")
    private String entityNumber;

    @Column(name = "shipment_number")
    @Size(max = 255, message = "max size is 255 for shipmentNumber")
    private String shipmentNumber;
}
