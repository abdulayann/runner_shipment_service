package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Entity
@Setter
@Getter
@Table(name = "order_number")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@SuppressWarnings("java:S1700") // A field should not duplicate the name of its containing class
public class OrderNumber extends MultiTenancy {
    @Column(name = "order_number")
    private Long orderNumber;
}
