package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Data
@Setter
@Getter
@Table(name = "console_shipment_mapping")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class ConsolShipLinkDetails extends BaseEntity {
    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "shipment_id")
    private Long shipmentId;
}
