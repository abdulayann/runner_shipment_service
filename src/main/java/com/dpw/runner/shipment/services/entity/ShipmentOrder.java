package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;

@Entity
@Setter
@Getter
@Table(name = "shipment_order")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class ShipmentOrder extends BaseEntity {
    @Column(name = "order_guid")
    private UUID orderGuid;

    @Column(name = "order_number")
    private String orderNumber;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "order_date")
    private LocalDateTime orderDate;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentOrderId")
    @BatchSize(size = 50)
    private List<Packing> orderPackings;

}
