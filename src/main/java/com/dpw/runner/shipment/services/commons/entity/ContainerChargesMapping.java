package com.dpw.runner.shipment.services.commons.entity;

import com.dpw.runner.shipment.services.commons.entity.commons.BaseEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Getter
@Setter
@Table(name = "container_charges_mapping")
@AllArgsConstructor
@NoArgsConstructor
public class ContainerChargesMapping extends BaseEntity {
    @Column(name = "container_id")
    private Long containerId;

    @Column(name = "charge_id")
    private Long chargeId;
}
